#### CPR Data Anomaly Explorer  ####
# Adam A. Kemberling
# 9/22/2020
# App for displaying fits and metrics of seasonal splines from cpr data
# zooplankton data is from continuous plankton recorder
# Full time series a combination of NOAA and SAHFOS data products

####  Packages  ####
library(shiny)
library(ggpmisc)
library(splines)
library(mgcv)
library(tidyverse)
library(patchwork)
library(sf)
library(here)
library(gmRi)


####  Functions  ####

# Load CPR Helper Functions
source(here::here("R/cpr_helper_funs.R"))
ccel_boxpath <- shared.path(os.use = "unix", group = "Climate Change Ecology Lab", folder = NULL)

# Use GMRI Stylesheet
gmRi::use_gmri_style_shiny(stylesheet = "gmri rmarkdown")

#Set ggplot theme
theme_set(theme_bw())

# Coastlines
northeast <- rnaturalearth::ne_states(country = "united states of america") %>% st_as_sfc(crs = 4326)
canada <- rnaturalearth::ne_states(country = "canada") %>% st_as_sfc(crs = 4326)



####  CPR Raw Data  ####

# Combined dataset from NOAA/SAHFOS, concentrations in common units: # / meters cubed
cpr <- read_csv(str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "2020_combined_data", "zooplankton_combined.csv", sep = "/"), 
                guess_max = 1e6, col_types = cols())

#eliminate sample_id column that is mixed in, and format the dates
cpr <- cpr %>% 
    mutate(
        cal_date = as.POSIXct(str_c(year, month, day, sep = "/"), format = "%Y/%m/%d"),
        jday = lubridate::yday(cal_date),
        .before = `phytoplankton color index`) %>% 
    rename(
        lat = `latitude (degrees)`, 
        lon = `longitude (degrees)`) %>% 
    mutate(lon = ifelse(lon > 0, lon * -1, lon))






####  1. Make Taxa Lists  ####


#Identify the columns that represent abundances
taxa_cols <- names(cpr)[14:ncol(cpr)]
names(taxa_cols) <- taxa_cols


#Make a list with details on each taxa
taxa_list <- map(taxa_cols, function(x){
    taxa_name <- sym(x)
    taxa_subset <- cpr %>% 
        select(year, jday, lat, lon, abundance = !!taxa_name)
    
})




####  2. Calanus Test  ####
calanus_anoms <- cpr_spline_fun(cpr_dat = taxa_list$`calanus i-iv`, 
               spline_bins = 10, 
               season_bins = 4, 
               study_area = "GOM")




#### 3. Remove Incomplete Time Series  ####

#Find those pesky NA taxa
na_counts <- map(taxa_list, function(x){
    sum(is.na(x$abundance))
}) %>% bind_cols() %>%
    pivot_longer(names_to = "taxa", values_to = "total NA's", cols = everything()) %>%
    mutate(status = case_when(
        `total NA's` == 290 ~ "NOAA only",
        `total NA's` == 4799 ~ "SAHFOS only",
        `total NA's` == 0 ~ "Found in both",
        TRUE ~ "Only NA's"
    ))

#Taxa with full time series
keepers <- filter(na_counts, status == "Found in both")
fullts_taxa <- taxa_list[names(taxa_list) %in% keepers$taxa]


####  4. Selection Options  ####

# Update the list of choices to full time series only
taxa_names <- taxa_cols[which(names(taxa_list) %in% keepers$taxa)]

# List of Display options for each
display_names <- c("Abundance Timeline" = "timeline_plot", 
                   "Spatial Distribution" = "map_plot", 
                   "Seasonal Spline fit" = "resid_hist", 
                   "Anomaly timeline" = "anom_plot")



####  5. Calculate Detrended Abundances  ####
anomaly_list <- map(.x = fullts_taxa,
                    .f = cpr_spline_fun, 
                    spline_bins = 10, 
                    season_bins = 4, 
                    study_area = "GOM_new")

# From there we have the:
# 1. Predictions with actual original values
# 2. Summaries over 91-day quarters
# 3. the model itself


####  6. Distribution Map  ####
anomaly_list <- map(anomaly_list, function(x){
    data <- x$cprdat_predicted %>% 
        mutate(`Taxa Presence` = ifelse(abundance > 0, "Present", "Absent"))
    data_sf <- data %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
    
    x$map_plot <- ggplot() + 
        geom_sf(data = filter(data_sf, `Taxa Presence` == "Present"), aes(shape = `Taxa Presence`, color = `Taxa Presence`), alpha = 0.8) +
        geom_sf(data = filter(data_sf, `Taxa Presence` == "Absent"), aes(shape = `Taxa Presence`, color = `Taxa Presence`), alpha = 0.6) +
        geom_sf(data = northeast) +
        geom_sf(data = canada) +
        scale_shape_manual(values = c("Absent" = 3, "Present" = 16)) +
        scale_color_manual(values = c("Absent" = "black", "Present" = "royalblue")) +
        guides(fill = guide_legend(title = "", label = FALSE),
               shape = guide_legend(title.position = "top", title.hjust = 0.5)) +
        coord_sf(xlim = c(-71,-64.8), ylim = c(41, 44.3)) +
        theme_bw() +
        theme(legend.position = "bottom") +
        facet_wrap(~datebounds)
    
    return(x)
    
    
})

# # Tester
# anomaly_list$`calanus finmarchicus v-vi`$map_plot

####  7. Histogram of residuals  ####
anomaly_list <- map(anomaly_list, function(x){
    
    residual_data <- tibble(`Spline Residuals` = x$spline_model$residuals) 
    
    x$resid_hist <- ggplot(residual_data) + 
        geom_histogram(aes(`Spline Residuals`), bins = 30, fill = gmri_cols("gmri blue")) +
        geom_vline(xintercept = 0, linetype = 2, color = gmri_cols("orange"), size = 1)
    
    return(x)
    
    
})

# # Test
# anomaly_list$`calanus finmarchicus v-vi`$resid_hist


####  8. Anomaly Timeseries  ####
anomaly_list <- map(anomaly_list, function(x){
    
    timeline_data <- x$period_summs %>% 
        mutate(
            datebounds = factor(datebounds, levels = c("1-365", "0-92", "92-184", "184-276", "276-365")),
            `Anomaly Direction` = ifelse(period_anom_mu > 0, "Positive Anomaly", "Negative Anomaly"))
    
    x$anom_plot <- timeline_data %>% 
        ggplot(aes(year, period_anom_mu)) +
        geom_hline(yintercept = 0, linetype = 2, color = "gray60") +
        geom_line(aes(group = datebounds), size = 1, color = "lightblue") + 
        geom_point(aes(color = `Anomaly Direction`), size = 1) + 
        scale_color_manual(values = c("Positive Anomaly" = as.character(gmri_cols("orange")),
                                      "Negative Anomaly" = as.character(gmri_cols("gmri blue")))) +
        facet_wrap(~datebounds, ncol = 2) +
        labs(x = NULL, y = "Seasonal Anomaly Mean")
    
    return(x)
    
    
})


# Test
anomaly_list$`calanus finmarchicus v-vi`$cprdat_predicted %>% 
    mutate(date = as.Date(str_c(year, "-01-01")),
           date = date + jday)

#### 9. Abundance Timelines  ####

anomaly_list <- map(anomaly_list, function(x){
    
    timeline_data <- x$cprdat_predicted %>% 
        mutate(
            date = as.Date(str_c(year, "-01-01")),
            date = date + jday,
            datebounds = factor(datebounds, levels = c("1-365", "0-92", "92-184", "184-276", "276-365"))
        )
    
    x$timeline_plot <- timeline_data %>% 
        ggplot(aes(date, abundance) ) +
        geom_line(aes(group = 1), size = 1, color = "lightblue") + 
        # geom_point(aes(color = `Anomaly Direction`), size = 1) + 
        # scale_color_manual(values = c("Positive Anomaly" = as.character(gmri_cols("orange")),
        #                               "Negative Anomaly" = as.character(gmri_cols("gmri blue")))) +
        # facet_wrap(~datebounds, ncol = 2) +
        scale_y_continuous(labels = scales::scientific_format()) +
        labs(x = NULL, y = "Observed Abundance (#/square meter)")
    
    return(x)
    
    
})

anomaly_list$`calanus finmarchicus v-vi`$timeline_plot


####_________________####
####  Define UI ####
ui <- fluidPage(
    
    ####  Page Details  ####
    title = "CPR Explorer", 
    theme = "styles/gmri_rmarkdown.css", 
    
    ####  Application Banner  ####
    fluidRow(style = "padding: 20px;",

             column(12,
                    img(src = "GMRI_logo.png")
             )
    ),

    ####  Title and Introduction  ####
    
    fluidRow(style = 'padding-bottom: 20px;',
             
             #Buffer column
             column(1),
             column(width = 10,
                tags$h1("Exploring Zooplankton Distribution and Abundance Patterns"),
                tags$h2("Continuous Plankton Recorder Survey: Gulf of Maine"),
                tags$br(),
                tags$p("The Gulf of Maine Continuous Plankton Recorder Survey is a scientific resource
                       used to identify spatial and temporal patterns of zooplankton in near-surface waters (<50m).
                       The CPR sampler is towed behind a ship of opportunity several times a year. Transects extend 
                       from Nova Scotia to either Boston MS, or more recently Portland ME.")
                ),
             column(1)
    ),
    
    ####  Selection Options  ####
    fluidRow(style = 'padding-bottom: 20px;',
             
             #Buffer column
             column(1),
             column(4,
                    style = "color: black",
                    align = "left",
                    
                    
                    # Taxa 1 Selector
                    selectInput(inputId = "taxa1",
                                label   = "1. Select a Taxa",
                                choices = taxa_names),
                    
                    # Buoy 1 Selector
                    selectInput(inputId = "display1",
                                label   = "2. Select a Display",
                                choices = display_names),
                    
                    actionButton(inputId = "left_button", label = "Update Left-Side", icon("refresh"))
                    
             ),
             column(2),
             
             column(4,
                    style = "color: black",
                    align = "left",
                    
                    
                    # Taxa 2 Selector
                    selectInput(inputId = "taxa2",
                                label   = "1. Select a Taxa",
                                choices = taxa_names),
                    
                    # Buoy 2 Selector
                    selectInput(inputId = "display2",
                                label   = "2. Select a Display",
                                choices = display_names),
                    
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
        
    ),
    
    ####  Additional Comments  ####
    fluidRow(
        column(1),
        column(10,
               tags$strong("NOTE:"),
               tags$p("The data and figures above are a product of CPR data collected from both NOAA 
                       and the Sir Alister Hardy Foundation (SAHFOS). Pre-processing to common units, and consistent 
                       group classifications occurred prior to any plotting. Spatial extent was also limited to the
                       study area of the Gulf of Maine."),
               # Add footer with github info
              gmRi::insert_gmri_footer(footer_file = "akemberling_gmri_footer.html")),
        column(1)
    )
)

####_________________####
####  Define Server Logic  ####
server <- function(input, output) {
    
    
    ####  Observe Left Side  ####
    observeEvent(input$left_button, {
        ####Left Side Plot  ####
        output$left_plot <- renderPlot({
            #ggplot()
            anomaly_list[[input$taxa1]][input$display1]
        }, bg="transparent")
    })

    ####  Observe Right Side  ####
    observeEvent(input$right_button, {
        ####Right Side Plot  ####
        output$right_plot <- renderPlot({
            #ggplot()
            anomaly_list[[input$taxa2]][input$display2]
        }, bg="transparent")
    })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)