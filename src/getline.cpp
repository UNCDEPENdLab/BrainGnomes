#include <Rcpp.h>
#include <unistd.h>    // isatty, STDIN_FILENO
#include <termios.h>   // terminal control
#include <csignal>     // signal handling
#include <iostream>
#include <string>

// RAII guard for restoring terminal settings
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
      
      if (ch == 27 || ch == EOF) {  // Ctrl+C, ESC, or EOF
        return R_NilValue;
      } else if (ch == '\n' || ch == '\r') {
        break;
      } else if (ch == 127 || ch == 8) {  // Backspace
        if (!input.empty()) {
          input.pop_back();
          std::cout << "\b \b" << std::flush;  // Erase character from screen
        }
      } else {
        input += static_cast<char>(ch);
        std::cout << static_cast<char>(ch) << std::flush;
      }
    }
  } catch (...) {
    return R_NilValue;
  }
  
  return Rcpp::wrap(input);
}