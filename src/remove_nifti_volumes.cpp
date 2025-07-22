#include "BrainGnomes.h"

//' Remove Specified Timepoints from a 4D NIfTI Image
//'
//' This function removes specified timepoints (volumes) from a 4D NIfTI image and saves
//' the resulting image to a new file. Timepoints are specified using 1-based indexing,
//' consistent with R conventions.
//'
//' @name remove_nifti_volumes
//' @param infile Character string. Path to the input 4D NIfTI file.
//' @param remove_tpts Integer vector. Timepoints (1-based) to remove from the image.
//' @param outfile Character string. Path to save the output NIfTI file with selected volumes.
//'
//' @return None. The function writes a new NIfTI file to \code{outfile}.
//'
//' @details This function uses the \code{volumes} argument in RNifti to efficiently read
//' only the retained timepoints from disk. If all volumes are removed, an error is thrown.
//' The input image must be 4-dimensional (i.e., include a time dimension).
//'
//' @examples
//' \dontrun{
//' remove_nifti_volumes("input_bold.nii.gz", remove_tpts = c(1, 2, 100), outfile = "trimmed_bold.nii.gz")
//' }
//'
//' @export

// [[Rcpp::export]]
void remove_nifti_volumes(std::string infile, const std::vector<int>& remove_tpts, std::string outfile) {
  RNifti::NiftiImage image(infile, false); // don't read data
  
  if (image.dim().size() < 4 || image.dim()[3] <= 1) {
    stop("Input image must be 4D.");
  }
  
  int n_vols = image.dim()[3]; // number of timepoints
  
  // Convert remove_tpts to 0-based indexing and mark those to keep
  std::vector<bool> keep_mask(n_vols, true);
  for (int t : remove_tpts) {
    if (t < 1 || t > n_vols) stop("remove_tpts contains invalid index: %d (must be between 1 and %d)", t, n_vols);
    keep_mask[t - 1] = false;
  }
  
  // Create vector of timepoints to retain
  std::vector<dim_t> keep_tpts;
  for (int i = 0; i < n_vols; ++i) {
    if (keep_mask[i]) keep_tpts.push_back(i);
  }
  
  if (keep_tpts.empty()) {
    stop("All timepoints would be removed — at least one volume must be retained.");
  }
  
  // Use volumes argument to read only a subset of timepoints
  RNifti::NiftiImage subset = RNifti::NiftiImage(infile, keep_tpts);
  
  // Save to output
  subset.toFile(outfile);
}