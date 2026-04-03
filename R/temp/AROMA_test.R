# Function version of AROMA test

AROMA_test <- function(aroma_path, maskpath, mixing_file, noise_ics){
  suppressPackageStartupMessages({
library(pracma)
library(RNifti)
library(speedglm)
library(oro.nifti)
library(foreach)
library(dplyr)
library(Matrix)
library(doParallel)})

filter_text <- scan(noise_ics, what = "character", quiet = TRUE)
badics <- as.numeric(strsplit(filter_text, ",")[[1]])
melmix <- data.matrix(read.table(mixing_file, header = FALSE, colClasses = "numeric"))
aroma_nifti <- readNifti(aroma_path)
aroma_dims <- dim(aroma_nifti)
n_vox_aroma <- prod(aroma_dims[1:3])
n_t_aroma <-aroma_dims[4]
aroma_matrix <- array(aroma_nifti, dim = c(n_vox_aroma, n_t_aroma))
#removing the aroma nifti for memory
rm(aroma_nifti)
gc()



mask <- readNifti(maskpath)
voxels <- 1:n_vox_aroma
in_mask <- as.logical(mask)
coords <- sample(voxels[in_mask], 10)
test_sample <- aroma_matrix[coords,]

partialLm <- function(y, X, ivs = NULL) {
  m <- speedlm.fit(y = y, X = X, intercept = FALSE)
  pred <- X[, ivs] %*% coef(m)[ivs]
  return(as.vector(y - pred))
}
start_time <- Sys.time()
message("Starting voxelwise partial regression: ", start_time)
# starting voxelwise regression
res <- foreach::foreach(v = iter(test_sample, by = "row"),.noexport = "test_sample", .packages = "speedglm", .inorder = TRUE, .multicombine = TRUE, .combine = rbind) %dopar% {
  partialLm(matrix(v, ncol = 1), melmix, badics)
}
if (is.vector(res)) res <- matrix(res, nrow = 1)

end_time <- Sys.time()
duration <- difftime(end_time, start_time, units = "mins")
message("partialLm fitting completed in ", round(duration, 3), " minutes.")
test_vec <- as.vector(test_sample)
res_vec <- as.vector(res)
difference <- test_sample - res
cor_results <- cor(test_vec, res_vec)

message("correlation between AROMA result and test should be 1. Correlation: ", {cor_results})

}
