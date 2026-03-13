test_masking <- function(maskpath, datapath){
require(RNifti)
require(matrixStats)
require(dplyr)

mask <- readNifti(maskpath)
data4d <- readNifti(datapath)

mask_logical <- (mask != 0) & is.finite(mask)
img_dims <- dim(data4d)
n_vox <- prod(img_dims[1:3])
n_t <- img_dims[4]

img_matrix <- array(data4d, dim = c(n_vox, n_t))

min_vec <- rowMins(img_matrix) # minimum time point for that voxel 

mask_vec <- as.vector(mask)

mask_result <- cbind(mask_vec, min_vec)
mask_result_df <- as.data.frame(mask_result)

mask_result_df <- mask_result_df %>% mutate(externalposvox = case_when(
    .$mask_vec > 0 ~ 0, 
    .$mask_vec == 0 & min_vec != 0 ~ 1, 
    .$mask_vec == 0 & min_vec == 0 ~ 0
), internalzerovox = case_when(
    .$mask_vec == 0 ~ 0, 
    .$mask_vec > 0 & min_vec == 0 ~ 1, 
    .$mask_vec >0 & min_vec > 0 ~ 0
))

externalposvox <- sum(mask_result_df$externalposvox)
internalzerovox <- sum(mask_result_df$internalzerovox)

return(message(internalzerovox, " voxels = 0 inside the mask. ", externalposvox, " voxels > 0 outside the mask."))
}



