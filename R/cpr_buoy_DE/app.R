#### Buoy Data Exploration App  ####
# Adam A. Kemberling
# 12/4/2019
# App for displaying  relaationship between plankton communities and oceanographic data
# plankton data is from continuous plankton recorder data, buoy data is from the Gulf of Maine array

####  Packages  ####
library(ggpmisc)
library(tidyverse)
library(here)

####  Functions  ####

# #For working locally
# source(here::here("cpr_helper_funs.R"))

#Set ggplot theme
theme_set(theme_bw())

####  Load Data  ####

# #Local locaation
# cpr_buoys <- read_csv(str_c(cpr_boxpath,"data", "processed_data", "cpr_quarters_buoys.csv", sep = "/"),
#                       col_types = cols())

#For Publishing
cpr_buoys <- read_csv("./Data/cpr_quarters_buoys.csv", col_types = cols())

####_________________####
####  Data Prep  ####


####_Taxa List  ####
taxa_names <- unique(cpr_buoys$species)
taxa_names <- set_names(taxa_names, 
                        nm = c("Calanus V+", "Calanus I - IV", "Centropages", "Chaetognatha", "Euphausiacea",
                               "Metridia", "Oithona", "Pseudocalanus", "Paraeucheata", "Temora"))


####_Buoy List  ####
buoy_names <- c("Buoy_B", "Buoy_E", "Buoy_F", "Buoy_I", "Buoy_M", "Buoy_N")
buoy_names <- set_names(buoy_names, 
                        nm = c("Buoy B - Western Maine Shelf", 
                               "Buoy C - Central Maine Shelf",
                               "Buoy F - Penobscot Bay",
                               "Buoy I - Eastern Maine Shelf",
                               "Buoy M - Jordan Basin",
                               "Buoy N - Northeast Channel"))

####_Predictor List  ####
predictor_names <- c("mean_sal", "mean_temp", "mean_dens", "mean_strat_index")
predictor_names <- set_names(predictor_names,
                             nm = c("Average Salinity",
                                    "Average Temperature",
                                    "Average Seawater Density",
                                    "Stratification Index"))

####_Plot List  ####
plot_list_master <- list()

#The data split up for making plots
taxa_buoy_list <- cpr_buoys %>% 
    filter(is.na(buoy_id) == FALSE) %>% 
    split(.$species) %>% 
    map(~ .x %>% split(.$buoy_id))


#The list we are going to fill with plots
buoy_list <- list(
    "Buoy_B" = list(),
    "Buoy_E" = list(),
    "Buoy_F" = list(),
    "Buoy_I" = list(),
    "Buoy_M" = list(),
    "Buoy_N" = list()
)

plot_list_out <- list(
    "calanus"            = buoy_list,
    "calanus1to4"        = buoy_list,
    "centropages"        = buoy_list,
    "chaetognatha"       = buoy_list,
    "euphausiacea"       = buoy_list,
    "metridia"           = buoy_list,
    "oithona"            = buoy_list,
    "para_pseudocalanus" = buoy_list,
    "paraeucheata"       = buoy_list,
    "temora"             = buoy_list
    
)

for (taxa in 1:length(taxa_names)) {
    for (buoy in 1:length(buoy_names)) {
        
        #Average Salinity Plots
        plot_list_out[[taxa]][[buoy]]$mean_sal <- taxa_buoy_list[[taxa]][[buoy]] %>% 
            ggplot(aes(x = mean_sal, y = anomaly)) +
                geom_smooth(method = "lm", se = FALSE, color = "gray50") +
                geom_point() +
                stat_poly_eq(formula = y ~ x, 
                             eq.with.lhs = "italic(hat(y))~`=`~",
                             aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                             parse = TRUE) +  
                facet_grid(period ~ reading_depth) +
                labs(x = "Average Salinity",
                     y = "Population Anomaly (sd)") +
                theme_bw()
        
        #Average Temperature Plots
        plot_list_out[[taxa]][[buoy]]$mean_temp <- taxa_buoy_list[[taxa]][[buoy]] %>% 
            ggplot(aes(x = mean_temp, y = anomaly)) +
                geom_smooth(method = "lm", se = FALSE, color = "gray50") +
                geom_point() +
                stat_poly_eq(formula = y ~ x, 
                             eq.with.lhs = "italic(hat(y))~`=`~",
                             aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                             parse = TRUE) +  
                facet_grid(period ~ reading_depth) +
                labs(x = "Average Temperature",
                     y = "Population Anomaly (sd)")
        
        #Average Density Plots
        plot_list_out[[taxa]][[buoy]]$mean_dens <- taxa_buoy_list[[taxa]][[buoy]] %>% 
            ggplot(aes(x = mean_dens, y = anomaly)) +
            geom_smooth(method = "lm", se = FALSE, color = "gray50") +
            geom_point() +
            stat_poly_eq(formula = y ~ x, 
                         eq.with.lhs = "italic(hat(y))~`=`~",
                         aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                         parse = TRUE) +  
            facet_grid(period ~ reading_depth) +
            labs(x = "Average Water Density",
                 y = "Population Anomaly (sd)")
        
        #Stratification Index Plots
        plot_list_out[[taxa]][[buoy]]$mean_strat_index <- taxa_buoy_list[[taxa]][[buoy]] %>% 
            ggplot(aes(x = mean_strat_index, y = anomaly)) +
                geom_smooth(method = "lm", se = FALSE, color = "gray50") +
                geom_point() +
                stat_poly_eq(formula = y ~ x, 
                             eq.with.lhs = "italic(hat(y))~`=`~",
                             aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                             parse = TRUE) +  
                facet_wrap(period ~ .) +
                labs(x = "Stratification Index",
                     y = "Population Anomaly (sd)")
        
    }
}


####_________________####
####  Define UI ####
ui <- fluidPage(
    
    ####  Page Details  ####
    title = "Buoy Data Explorer", 

    ####  Application Banner  ####
    fluidRow(style = "padding: 20px;",
        
        column(12,
               img(src = "GMRI_logo.png")      
        )
    ),
    
    ####  Title and Introduction  ####
    
    ####  Selection Options  ####
    fluidRow(style = 'padding-bottom: 20px;',
        
             #Buffer column
             column(1),
             column(4,
                    style = "color: black",
                    align = "left",
                    
                    
                    # Taxa 1 Selector
                    selectInput(inputId = "taxa1",
                                label   = "1. Select a Taxa for the left-side plot",
                                choices = taxa_names
                                
                    ),
                    
                    # Buoy 1 Selector
                    selectInput(inputId = "buoy1",
                                label   = "2. Select a Buoy",
                                choices = buoy_names
                    ),
                    
                    # Predictor 1 Selector
                    selectInput(inputId = "predictor1",
                                label   = "3. Select a predictor",
                                choices = predictor_names
                    ),
                    
                    actionButton(inputId = "left_button", label = "Update Left-Side", icon("refresh"))
                    
             ),
             column(2),
             
             column(4,
                    style = "color: black",
                    align = "left",
                    
                    
                    # Taxa 2 Selector
                    selectInput(inputId = "taxa2",
                                label   = "1. Select a Taxa for the right-side plot",
                                choices = taxa_names
                                
                    ),
                    
                    # Buoy 2 Selector
                    selectInput(inputId = "buoy2",
                                label   = "2. Select a Buoy",
                                choices = buoy_names
                    ),
                    
                    # Predictor 2 Selector
                    selectInput(inputId = "predictor2",
                                label   = "3. Select a predictor",
                                choices = predictor_names
                    ),
                    
                    actionButton(inputId = "right_button", label = "Update Right-Side", icon("refresh"))
                    
             ),
             
             column(1)
             
             
             
             
        
    ),
    
    ####  Plotting Space  ####
    fluidRow(
        column(width = 6,
               wellPanel(
                   align = "center",
                   style = 'float: center;',
                   plotOutput("left_plot",
                              height = "600px",
                              width = "auto")
                   )
               ),
        column(width = 6,
               wellPanel(
                   align = "center",
                   style = 'float: center;',
                   plotOutput("right_plot",
                              height = "600px",
                              width = "auto")
                   )
               )
        
    )
)

####_________________####
####  Define Server Logic  ####
server <- function(input, output) {

    
    ####  Observe Left Side  ####
    observeEvent(input$left_button, {
        ####Left Side Plot  ####
        output$left_plot <- renderPlot({
            plot_list_out[[input$taxa1]][[input$buoy1]][input$predictor1]
        })
    })
    
    ####  Observe Right Side  ####
    observeEvent(input$left_button, {
        ####Right Side Plot  ####
        output$right_plot <- renderPlot({
            plot_list_out[[input$taxa2]][[input$buoy2]][input$predictor2]
        })
    })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
