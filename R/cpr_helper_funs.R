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
ccel_boxpath <- "/Users/akemberling/Box/Climate Change Ecology Lab"


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


####  Corrplot Functions  ####

#' Extract upper triangle of the correlation matrix
#'
#' @param correlation_matrix Correlation matrix object created by cor()
#'
#' @return correlation_matrix Correlation matrix with NA values substituted for lower-triangle correlations
#' @export
#'
#' @examples
get_upper_tri <- function(correlation_matrix){
  correlation_matrix[lower.tri(correlation_matrix)] <- NA
  return(correlation_matrix)
}

#Pull correlations with p-values
corr_plot_setup <- function(wide_df) {
  
  # 1. Pull data used for corellation matrix
  corr_data <- wide_df %>% 
    select(-year, -period)
  
  # 2. Pull the correlation matrix and melt to a dataframe
  corr_mat <- corr_data %>% cor() 
  
  # 2b. Correlation Matrix as a dataframe
  corr_out <- corr_mat %>% reshape2::melt(na.rm = TRUE)
  
  # 2c. Upper Triangle of correlation matrix
  upper_tri <- corr_mat %>% 
    get_upper_tri() %>%
    reshape2::melt() %>% 
    drop_na()
  
  # 3. do it again but pull the p-values
  p_data <- corrplot::cor.mtest(corr_mat)$p 
  
  #Assign the same names as the corr matrix
  dimnames(p_data) <- dimnames(corr_mat)
  
  #reshape to match correlation df
  p_data <- reshape2::melt(p_data, na.rm = T) %>% rename(pval = value)
  
  
  #Put the two together
  corr_full <- inner_join(corr_out, p_data, by = c("Var1", "Var2")) %>% 
    #Format levels and labels
    mutate(Var1 = fct_relevel(Var1, sort),
           Var2 = fct_relevel(Var2, sort),
           sig_symbol = if_else(pval <= 0.05 & value > 0, "+", " "),
           sig_symbol = if_else(pval <= 0.05 & value < 0, "-", sig_symbol))
  
  return(corr_full)
}

#Not in Function
`%notin%` <- purrr::negate(`%in%`)

# custom corr plot
cpr_corr_plot <- function(corr_dataframe, period = "Q1", plot_style = "tall"){
  
  #Filter Var1 and Var2 to reshape plot
  
  #Taxa
  my_taxa <- c("calanus", "calanus1to4", "centropages", "chaetognatha",
               "euphausiacea", "metridia", "oithona", "para_pseudocalanus",
               "paraeucheata", "temora")
  
  long_plot <- corr_dataframe %>% 
    filter(Var1 %notin% my_taxa,
           Var2 %in% my_taxa)
  
  tall_plot <- corr_dataframe %>% 
    filter(Var1 %in% my_taxa,
           Var2 %notin% my_taxa)
  
  if(plot_style == "tall") {
    plot_option  <- tall_plot
    leg_position <- "right"
  } else {
    plot_option  <- long_plot
    leg_position <- "bottom"
  }
  
  
  ggplot(plot_option, aes(x = Var1, y = fct_rev(Var2), fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sig_symbol), 
              color = "black", 
              size = 3) +
    scale_fill_gradient2(low = "blue", 
                         high = "red", 
                         mid = "white", 
                         midpoint = 0, 
                         limit = c(-1,1), 
                         space = "Lab", 
                         name="Pearson\nCorrelation") +
    labs(x = NULL, 
         y = NULL, 
         title = period) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 6),
          axis.text.y = element_text(size = 6),
          legend.position = leg_position) +
    coord_fixed()
  
  
}




