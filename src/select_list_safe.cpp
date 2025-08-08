#include <Rcpp.h>
#include <iostream>
#include <string>
#include <vector>
#include <sstream>
#include <unistd.h>  // isatty, STDIN_FILENO

//' A Safe Version of `select.list()` for Interactive and TTY Use
//'
//' Provides a cross-platform wrapper for R's [`select.list()`] that works both
//' in interactive R sessions and in non-interactive `Rscript` sessions at a terminal (TTY).
//' If running interactively, it calls `utils::select.list()`. If running in a non-interactive
//' terminal, it displays a textual menu and captures numeric input from the user.
//'
//' This function is especially useful in command-line scripts that need user input
//' in a controlled and fallback-compatible way.
//' 
//' @name select_list_safe
//' @param choices A character vector of choices to present.
//' @param title Optional title string to display above the menu (like in `select.list()`).
//' @param multiple Logical; if `TRUE`, the user may select multiple items by entering space-separated numbers.
//' @return A character string or character vector of selections. Returns an empty string (`""`) if the user cancels
//'   by entering `0`. If `multiple = TRUE`, a character vector of selected items is returned.
//' @examples
//' \dontrun{
//'   select_list_safe(c("Apples", "Oranges", "Bananas"), title = "Select fruit:")
//' }
//' @seealso [utils::select.list()]
//' @keywords internal

// [[Rcpp::export]]
Rcpp::CharacterVector select_list_safe(Rcpp::CharacterVector choices,
                               Rcpp::Nullable<std::string> title = R_NilValue,
                               bool multiple = false) {
  if (choices.size() == 0) {
    Rcpp::stop("No choices provided.");
  }
  
  // Use utils::select.list() if in an interactive R session
  Rcpp::Environment base_env("package:base");
  Rcpp::Function interactive_func = base_env["interactive"];
  bool is_interactive = Rcpp::as<bool>(interactive_func());
  
  if (is_interactive) {
    Rcpp::Environment utils_env("package:utils");
    Rcpp::Function select_func = utils_env["select.list"];
    return select_func(choices, Rcpp::_["title"] = title, Rcpp::_["multiple"] = multiple, Rcpp::_["graphics"] = false);
  }
  
  // Fallback for TTY
  if (!isatty(STDIN_FILENO)) {
    Rcpp::Rcout << "(Not a TTY; cannot read input)\n";
    return R_NilValue;
  }
  
  std::vector<std::string> selected;
  Rcpp::Rcout << "\n";
  
  if (title.isNotNull()) {
    Rcpp::Rcout << Rcpp::as<std::string>(title) << "\n\n";
  }
  
  for (int i = 0; i < choices.size(); ++i) {
    Rcpp::Rcout << "  " << (i + 1) << ": " << choices[i] << "\n";
  }

  if (multiple) {
    Rcpp::Rcout << "\nEnter one or more numbers separated by spaces and then ENTER, or 0 to cancel\n";
  }
  
  Rcpp::Rcout << std::endl << "Selection: " << std::flush;
  std::string input;
  std::vector<int> indices;
  
  while (true) {
    indices.clear();
    std::getline(std::cin, input); // read the line of user input
    std::istringstream iss(input); // now bind stream to user input
    
    int val;
    bool invalid = false;
    
    while (iss >> val) {
      if (val == 0) {
        return Rcpp::wrap(""); // cancel
      } else if (val >= 1 && val <= choices.size()) {
        indices.push_back(val - 1);
      } else {
        Rcpp::Rcout << "Invalid entry: " << val << ". Must be between 1 and " << choices.size() << "\n";
        invalid = true;
        break;
      }
    }
    
    // 🔴 Handle case where input was not numeric (e.g., "abc", "3a")
    if (iss.fail() && indices.empty()) {
      Rcpp::Rcout << "Invalid input. Please enter numbers separated by spaces." << std::endl;
    }
    
    if (!invalid && !indices.empty()) break; // success!
    
    Rcpp::Rcout << "Selection: " << std::flush;
  }
  
  // convert to IntegerVector to allow subsetting of named choices input
  Rcpp::IntegerVector r_indices(indices.begin(), indices.end());
  if (!multiple) r_indices = r_indices[0]; // only return the first selection
  
  return choices[r_indices];
}