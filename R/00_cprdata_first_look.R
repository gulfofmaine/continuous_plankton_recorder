#### Loading CPR Dataset
#### Adam A. Kemberling
#### 11/8/2019

####  Packages  ####
library(tidyverse)
library(here)

#Set ggplot theme
theme_set(theme_bw())


####  Load Data  ####
calanus            <- read.table(here("data","CPRtimeseries_textX6", "GOMx.Calanus_finmarchicus.txt"), header = F, skip = 3)
calanus_1to4       <- read.table(here("data","CPRtimeseries_textX6", "GOMx.Calanus_I-IV.txt"), header = F, skip = 3)
centropages        <- read.table(here("data","CPRtimeseries_textX6", "GOMx.Centropages_typicus.txt"), header = F, skip = 3)
chaetognatha       <- read.table(here("data","CPRtimeseries_textX6", "GOMx.Chaetognatha_eyecount.txt"), header = F, skip = 3)
euphausiacea       <- read.table(here("data","CPRtimeseries_textX6", "GOMx.Euphausiacea_Total.txt"), header = F, skip = 3)
metridia           <- read.table(here("data","CPRtimeseries_textX6", "GOMx.Metridia_lucens.txt"), header = F, skip = 3)
oithona            <- read.table(here("data","CPRtimeseries_textX6", "GOMx.Oithona_spp..txt"), header = F, skip = 3)
para_pseudocalanus <- read.table(here("data","CPRtimeseries_textX6", "GOMx.Para-Pseudocalanus_spp..txt"), header = F, skip = 3)
paraeucheata       <- read.table(here("data","CPRtimeseries_textX6", "GOMx.Paraeuchaeta_norvegica.txt"), header = F, skip = 3)
temora             <- read.table(here("data","CPRtimeseries_textX6", "GOMx.Temora_longicornis.txt"), header = F, skip = 3)


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
replace_names <- function(x) {setNames(x, c("year",	"annual",	"p1",	"p2",	"p3",	"p4",	"p5",	"p6"))}
cpr_species <- map(cpr_species, replace_names)

# Look at the structure
str(cpr_species$calanus)

# Gather the period columns so you can filter out and facet
cpr_reshape <- function(x) {
  data_out <- x %>% gather(key = "period", value = "anomaly", annual, p1,p2, p3, p4, p5, p6)
  return(data_out)
}


cpr_species <- map(cpr_species, cpr_reshape)


# Bind to one df
cpr_all <- bind_rows(cpr_species, .id = "species")
write_csv(cpr_all, here("data", "processed_data", "cpr_allspecies_long.csv"), col_names = TRUE)

# Remove originals
rm(calanus, calanus_1to4, centropages, chaetognatha, euphausiacea, metridia, oithona, para_pseudocalanus, paraeucheata, temora)


# Move to EDA



