###############################################################
# R for using TidyCensus and Tmap packages
###############################################################

# Extract US Census Bureau American Community Survey (ACS) 
# 5-year estimates at the census tract level for Baltimore, MD 
# and create a choropleth map

library(dplyr)      # for data management
library(tidycensus) # for ACS data extraction
library(tmap)       # for mapping
library(tmaptools)  # for color palettes  
library(tigris)     # for shapefile
library(sf)         # for spatial data operations

#--------------------------------------------------------------
# Open Census API and load data dictionaries
#--------------------------------------------------------------

# Set up a census api key to access data
# Sign up here:https://api.census.gov/data/key_signup.html
# census_api_key("...")
census_api_key("d438627a503fcef13365561eba491f02bd33e014")

# ACS Data Dictionaries
# acs5 specifies the 5-year estimates

# acs = load_variables(2022, "acs5", cache = TRUE)
# acs<-as.data.frame(acs)
# head(acs)

acs_s = load_variables(2022, "acs5/subject", cache = TRUE)
acs_s<-as.data.frame(acs_s)
head(acs_s)

#--------------------------------------------------------------
# Download data for median household median income 
#--------------------------------------------------------------

acs_s[grepl("S1901", acs_s$name),]
acs_s$label[acs_s$name=="S1901_C01_012"] 

# Extract data
medhhinc <- 
  get_acs(survey = "acs5",
          table = "S1901",
          year = 2022,
          
          geography = "tract",
          state = "MD",
          county = "Baltimore City",
          
          output = "wide",
          geometry = FALSE)

# Keep and rename variables of interest
medhhinc <- medhhinc %>%
  select(GEOID, 
         S1901_C01_012E) %>%
  rename(MedHHInc = S1901_C01_012E)

#--------------------------------------------------------------
# Read in Baltimore map and join data
#--------------------------------------------------------------

# Use the tigris package to import a shapefile of 
# Baltimore census tracts
balt <- tracts(state = "MD", 
               county = "Baltimore City",
               year = 2022)

qtm(balt) # quick thematic map of your shapefile

# Join area-level data to your shapefile by GEOID
balt <- merge(balt, medhhinc, by="GEOID", all.x=TRUE)

head(balt)

#--------------------------------------------------------------
# Create map
#--------------------------------------------------------------

map <- tm_shape(balt) +
  tm_polygons(
    fill = "MedHHInc",
    fill.scale = tm_scale_intervals(
      style = "jenks",
      n = 5,
      values = "brewer.greens"
    ),
    fill.legend = tm_legend(
      title = "Median HH income"
    )
  ) +
  tm_shape(st_union(balt)) +
  tm_borders(lwd = 2, col = "black") +
  tm_legend(position = c("left", "bottom")) +
  tm_title(
    "Median Household Income, Baltimore, MD",
    position = tm_pos_out("center", "top")
  )

map




