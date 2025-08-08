#include <Rcpp.h>
#include <iostream>
#include <string>
#include <vector>
#include <sstream>
#include <limits>
#include <unistd.h>    // isatty, STDIN_FILENO

//' Portable Menu Prompt for Interactive or TTY Sessions
//'
//' This function mimics the behavior of base R's `menu()` function. In interactive
//' sessions (e.g., R console or RStudio), it calls `utils::menu()` directly. In
//' non-interactive but TTY-capable sessions (e.g., an `Rscript` run in a terminal),
//' it displays a numbered list of choices and uses standard input to read the user's selection.
//'
//' The menu allows selection of an option by entering the corresponding number,
//' with `0` used to cancel the selection (consistent with `menu()` behavior).
//' 
//' @name menu_safe
//' @param choices A character vector of menu options to present to the user.
//' @param title Optional character string to display as the menu title.
//' @return An integer corresponding to the selected menu item (1-based index), or 0 if cancelled.
//'
//' @details
//' - If `menu_safe()` is called in an interactive R session, it defers to `utils::menu()`.
//' - In a non-interactive terminal (TTY), it prints the options and reads user input via `std::getline()`.
//' - If standard input is not connected to a terminal (e.g., piped input), the function returns 0 and prints a warning.
//'
//' @examples
//' \dontrun{
//' # Interactive R session
//' choice <- menu_safe(c("Apple", "Banana", "Cherry"), "Choose a fruit:")
//' if (choice == 0) cat("You cancelled the selection.\n")
//' else cat("You selected:", choice, "\n")
//'
//' # From a terminal via Rscript
//' # Rscript -e 'Rcpp::sourceCpp("menu_safe.cpp"); menu_safe(c("Yes", "No"))'
//' }
//'
//' @importFrom utils menu
//' @keywords internal
// [[Rcpp::export]]
int menu_safe(Rcpp::CharacterVector choices, Rcpp::Nullable<std::string> title = R_NilValue) {
  if (choices.size() == 0) {
    Rcpp::stop("No choices provided.");
  }
  
  // Use R's base::menu() in interactive mode
  Rcpp::Environment base_env("package:base");
  Rcpp::Function interactive_func = base_env["interactive"];
  bool is_interactive = Rcpp::as<bool>(interactive_func());

  if (is_interactive) {
    Rcpp::Environment utils_env("package:utils");
    Rcpp::Function menu_func = utils_env["menu"];
    return Rcpp::as<int>(menu_func(choices, title));
  }

  // Non-interactive mode: emulate menu in TTY
  if (!isatty(STDIN_FILENO)) {
    Rcpp::Rcout << "(Not a TTY; cannot read input)\n";
    return 0;
  }

  if (title.isNotNull()) {
    Rcpp::Rcout << "\n" << Rcpp::as<std::string>(title) << "\n\n";  
  } else {
    Rcpp::Rcout << "\n";
  }

  // Print choices
  for (int i = 0; i < choices.size(); ++i) {
    Rcpp::Rcout << "  " << (i + 1) << ": " << choices[i] << "\n";
  }

  Rcpp::Rcout << std::endl << "Selection: " << std::flush;

  std::string input;
  int selection = -1;

  // Read line and validate
  while (true) {
    std::getline(std::cin, input);

    std::istringstream iss(input);
    if ((iss >> selection) && (selection >= 0) && (selection <= choices.size())) {
      break;
    }

    Rcpp::Rcout << "Enter an item from the menu, or 0 to exit" << std::endl << std::flush;
    Rcpp::Rcout << "Selection: " << std::flush;
  }

  return selection;
}