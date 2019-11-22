####  Helper Functions  ####

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