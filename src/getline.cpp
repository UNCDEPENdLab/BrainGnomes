#include <Rcpp.h>
#include <iostream>
#include <string>


#ifdef _WIN32
  // Windows does not support termios or raw mode input
  #define POSIX_TERMINAL_SUPPORT 0
#else
  #include <unistd.h>    // isatty, STDIN_FILENO
  #include <termios.h>   // terminal control
  #define POSIX_TERMINAL_SUPPORT 1
#endif

#include <unistd.h>
#include <termios.h>   


// RAII guard for restoring terminal settings
#if POSIX_TERMINAL_SUPPORT
class TermiosGuard {
  struct termios oldt;
  bool active;
  
  public:
    TermiosGuard() : active(false) {}
  
  void activate() {
    if (isatty(STDIN_FILENO)) {
      tcgetattr(STDIN_FILENO, &oldt);
      struct termios newt = oldt;
      // turn off canonical mode and echoing to terminal
      newt.c_lflag &= ~(ICANON | ECHO);
      //newt.c_lflag &= ~(ICANON);
      tcsetattr(STDIN_FILENO, TCSANOW, &newt);
      active = true;
    }
  }
  
  ~TermiosGuard() {
    if (active) {
      tcsetattr(STDIN_FILENO, TCSANOW, &oldt);
    }
  }
};
#endif

//' Read a Line of Input from the User in Both Interactive and Non-Interactive Sessions
//'
//' Provides a safe and portable way to prompt the user for input from the terminal,
//' working seamlessly in both interactive R sessions and non-interactive `Rscript`
//' sessions (when connected to a TTY).
//'
//' In an interactive session (e.g., RStudio or R console), this function delegates
//' to base R's `readline()`. In non-interactive contexts where a TTY is available,
//' it switches the terminal to non-canonical mode and performs low-level character-by-character
//' input handling with support for echoing, backspace editing, and cancellation
//' via Escape or EOF. Ctrl+C will terminate the process as usual.
//'
//' @param prompt A character string to display as the input prompt.
//'
//' @return A character string with the user's input, or `NULL` (`R_NilValue`) if input is cancelled
//' via Escape (`ESC`, ASCII 27) or EOF (`Ctrl+D` or equivalent).
//'
//' @details
//' - Supports visible typing and live editing with backspace.
//' - Returns early with `NULL` if the user presses Escape or EOF.
//' - Uses raw terminal input in non-interactive mode; requires that stdin is a TTY.
//' - Does **not** intercept `Ctrl+C`; this will terminate the process as normal.
//'
//' @examples
//' \dontrun{
//'   # In Rscript or R console
//'   input <- readline_safe("Enter your name: ")
//'   if (!is.null(input)) cat("Hello,", input, "!\n")
//' }
//'
//' @export
// [[Rcpp::export]]

SEXP getline(std::string prompt) {
  // Check if R is interactive
  Rcpp::Environment base_env("package:base");
  Rcpp::Function interactive_func = base_env["interactive"];
  bool is_R_interactive = Rcpp::as<bool>(interactive_func());
  
  if (is_R_interactive) {
    Rcpp::Function readline = base_env["readline"];
    std::string result = Rcpp::as<std::string>(readline(prompt));
    return Rcpp::wrap(result);
  }
  
#if POSIX_TERMINAL_SUPPORT
  // Non-interactive Rscript session: ensure we're in a tty
  if (!isatty(STDIN_FILENO)) {
    Rcpp::Rcout << "(Not a TTY; cannot read input)\n";
    return R_NilValue;
  }
  
  // Raw mode terminal input
  Rcpp::Rcout << prompt << std::flush;
  std::string input;
  TermiosGuard guard;
  guard.activate();

  int ch; // character code
  
  // here, we are echoing to the screen manually to allow backspace to delete characters
  try {
    while (true) {
      Rcpp::checkUserInterrupt();
      
      ch = getchar();
      
      if (ch == 27 || ch == EOF) {  // ESC or EOF
        Rcpp::Rcout << std::endl;
        return R_NilValue;
      } else if (ch == '\n' || ch == '\r') {
        break;
      } else if (ch == 127 || ch == 8) {  // Backspace
        if (!input.empty()) {
          input.pop_back();
          Rcpp::Rcout << "\b \b" << std::flush;  // Erase character from screen
        }
      } else {
        input += static_cast<char>(ch);
        Rcpp::Rcout << static_cast<char>(ch) << std::flush;
      }
    }
  } catch (...) {
    Rcpp::Rcout << std::endl;
    return R_NilValue;
  }
  
  Rcpp::Rcout << std::endl; // make sure a newline is output prior to return
  return Rcpp::wrap(input);

#else
  // Windows fallback
  Rcpp::Rcout << prompt << std::flush;
  std::string input;
  std::getline(std::cin, input);
  return Rcpp::wrap(input);
#endif
}