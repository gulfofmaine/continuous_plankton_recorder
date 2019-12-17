####  Helper Functions  ####

#Set ggplot theme
ggplot2::theme_set(ggplot2::theme_classic())

#' Locate Data Path on Box
#'
#' @param box_project_name 
#'
#' @return box_path project directory string for box project data
#' @export
#'
#' @examples
find_box_data <- function(box_project_name) {
  box_project <- as.character(box_project_name)
  box_path <- str_c("/Users/akemberling/Box/Adam Kemberling/Box_Projects/", paste(box_project))
  return(box_path)
}

cpr_boxpath <- find_box_data("continuous_plankton_recorder")


#Load GMRI color Palettes
source("/Users/akemberling/Box/Adam Kemberling/Box_Projects/R_exercise_library/gmri_colors/gmripalette/R/gmri_colors.R")



#' Floor Decade
#'
#' @param year_vector Vector of integer years
#'
#' @return decade_vector returned vector of years rounded down to their decade
#' @export
#'
#' @examples
floor_decade <- function(year_vector){ 
  decade_vector <- year_vector - year_vector %% 10
  return(decade_vector)
}

#' Apply Principal Component Loadings to Data Matrix
#'
#' @param pca_load The data we wish do apply loadings to. Must have same column dimensions as the PCA dataset.
#' @param pca_rotations Roatations obtained from the PCA object (results from porcomp())
#' @param mode_num The Principal component loading to apply as an integer
#'
#' @return pca_adjusted dataframe containing original values of pca_load adjusted by the selected PCA loading's weights
#' @export
#'
#' @examples
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
