####  Helper Functions  ####

#Set ggplot theme
ggplot2::theme_set(ggplot2::theme_classic())

#Find Project Box Path
find_box_data <- function(box_project_name) {
  box_project <- as.character(box_project_name)
  box_path <- str_c("/Users/akemberling/Box/Adam Kemberling/Box_Projects/", paste(box_project))
  return(box_path)
}

cpr_boxpath <- find_box_data("continuous_plankton_recorder")


#Load GMRI color Palettes
source("/Users/akemberling/Box/Adam Kemberling/Box_Projects/R_exercise_library/gmri_colors/gmripalette/R/gmri_colors.R")


#Get decades from years
floor_decade <- function(value){ return(value - value %% 10) }

#Apply PCA Loads to a new dataset from specific PC loading
apply_pca_load <- function(pca_load, pca_rotations, mode_num = 1) {
  
  #Pull PCA rotations/loadings
  rotations <- as.data.frame(pca_rotations)
  rotations_t <- t(rotations)
  
  #Principal component whose weights we want to apply
  mode_num <- as.integer(mode_num)
  
  #Copy of the initial values to apply them to
  pca_adjusted <- pca_load[,2:ncol(pca_load)]
  
  #Multiply the columns by their PCA weights
  for (i in 1:ncol(rotations_t)) {
    pca_adjusted[, i] <- pca_adjusted[, i] * rotations_t[mode_num, i]
    
  }
  
  return(pca_adjusted)
}
