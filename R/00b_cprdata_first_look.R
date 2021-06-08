#### Loading CPR Dataset - Revised Data
#### Adam A. Kemberling
#### 12/2/2019

# NOTE: This is a revised reload of the CPR plankton data. Data now broken down into quarters

####  Packages  ####
library(tidyverse)
library(here)

####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))

#Set ggplot theme
theme_set(theme_bw())


####  Load Data  ####
calanus            <- read.table(str_c(cpr_boxpath, "data","CPRtimeseries_textX", "GOMx.Calanus_finmarchicus.txt", sep = "/"), header = F, skip = 3)
calanus_1to4       <- read.table(str_c(cpr_boxpath, "data","CPRtimeseries_textX", "GOMx.Calanus_I-IV.txt", sep = "/"), header = F, skip = 3)
centropages        <- read.table(str_c(cpr_boxpath, "data","CPRtimeseries_textX", "GOMx.Centropages_typicus.txt", sep = "/"), header = F, skip = 3)
chaetognatha       <- read.table(str_c(cpr_boxpath, "data","CPRtimeseries_textX", "GOMx.Chaetognatha_eyecount.txt", sep = "/"), header = F, skip = 3)
euphausiacea       <- read.table(str_c(cpr_boxpath, "data","CPRtimeseries_textX", "GOMx.Euphausiacea_Total.txt", sep = "/"), header = F, skip = 3)
metridia           <- read.table(str_c(cpr_boxpath, "data","CPRtimeseries_textX", "GOMx.Metridia_lucens.txt", sep = "/"), header = F, skip = 3)
oithona            <- read.table(str_c(cpr_boxpath, "data","CPRtimeseries_textX", "GOMx.Oithona_spp..txt", sep = "/"), header = F, skip = 3)
para_pseudocalanus <- read.table(str_c(cpr_boxpath, "data","CPRtimeseries_textX", "GOMx.Para-Pseudocalanus_spp..txt", sep = "/"), header = F, skip = 3)
paraeucheata       <- read.table(str_c(cpr_boxpath, "data","CPRtimeseries_textX", "GOMx.Paraeuchaeta_norvegica.txt", sep = "/"), header = F, skip = 3)
temora             <- read.table(str_c(cpr_boxpath, "data","CPRtimeseries_textX", "GOMx.Temora_longicornis.txt", sep = "/"), header = F, skip = 3)


####  Data Reshaping  ####

#put them in a list so you can map adjustment functions to each at one time
cpr_species <- list(
  "calanus"            = calanus, 
  "calanus1to4"        = calanus_1to4,
  "centropages"        = centropages,
  "chaetognatha"       = chaetognatha,
  "euphausiacea"       = euphausiacea,
  "metridia"           = metridia,
  "oithona"            = oithona,
  "para_pseudocalanus" = para_pseudocalanus,
  "paraeucheata"       = paraeucheata,
  "temora"             = temora
)


# Fix column names
replace_names <- function(x) {setNames(x, c("year",	"annual",	"q1",	"q2",	"q3",	"q4"))}
cpr_species <- map(cpr_species, replace_names)

# Look at the structure
str(cpr_species$calanus)

# Gather the period columns so you can filter out and facet
cpr_reshape <- function(x) {
  data_out <- x %>% gather(key = "period", value = "anomaly", annual, q1, q2, q3, q4)
  return(data_out)
}

#Map through the list
cpr_species <- map(cpr_species, cpr_reshape)


# Bind to one df
cpr_all <- bind_rows(cpr_species, .id = "species")

# #Export out
# write_csv(cpr_all, str_c(cpr_boxpath, "data/processed_data/cpr_allspecies_long_quarters.csv"), 
#           col_names = TRUE)

# Remove originals
rm(calanus, calanus_1to4, centropages, chaetognatha, euphausiacea, metridia, oithona, para_pseudocalanus, paraeucheata, temora, cpr_species)



####  SST  ####
#Merging it with SST
sst_long_lagged <- read_csv(str_c(cpr_boxpath, "data", "processed_data", "SST_with_lags.csv", sep = "/")) %>% 
  mutate(period = case_when(
    period == "annual" ~ "Annual",
    period == "jf" ~ "January - February",
    period == "ma" ~ "March - April",
    period == "mj" ~ "May - June",
    period == "ja" ~ "July - August",
    period == "so" ~ "September - October",
    period == "nd" ~ "November - December"),
    period = factor(period, 
                    levels = c("Annual", "January - February", "March - April", "May - June", "July - August", "September - October", "November - December")))