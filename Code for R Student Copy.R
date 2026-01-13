# Grammar of Graphics Analytics PD

# Libraries
library(dplyr)
library(magrittr)
library(readr)
library(stringr)
library(ggplot2)
library(quartets)
library(sf)
library(httr)

# Anscombe's Quartet example
# Plot all data together
ggplot(anscombe_quartet, aes(x = x, y = y)) + 
  geom_point()
# Split by dataset
ggplot(anscombe_quartet, aes(x = x, y = y, color = dataset)) + 
  geom_point()
# Split by dataset more effectively
ggplot(anscombe_quartet, aes(x = x, y = y, color = dataset)) + 
  geom_point() + 
  facet_wrap(~dataset)
# Try adding linear regression lines
ggplot(anscombe_quartet, aes(x = x, y = y, color = dataset)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = "y ~ x") + 
  facet_wrap(~dataset)

# Loading data from github
url_district <- "https://raw.githubusercontent.com/brussell23/grammar_of_graphics/refs/heads/main/staar_district_g8_y25_math.csv"
url_state <- "https://raw.githubusercontent.com/brussell23/grammar_of_graphics/refs/heads/main/staar_state_g3_g8_y21_y25_math_rla.csv"
url_snapshot <- "https://raw.githubusercontent.com/brussell23/grammar_of_graphics/refs/heads/main/district_snapshot_2223.csv"

staar_district <- read_csv(url_district)
staar_state <- read_csv(url_state)
district_snapshot <- read_csv(url_snapshot)

# Change STAAR % to decimal
staar_district %<>%
  mutate(`STAAR - Mathematics|Performance Levels|Did Not Meet|Percentage` = `STAAR - Mathematics|Performance Levels|Did Not Meet|Percentage`/100,
         `STAAR - Mathematics|Performance Levels|Approaches and Above|Percentage` = `STAAR - Mathematics|Performance Levels|Approaches and Above|Percentage`/100,
         `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage` = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`/100,
         `STAAR - Mathematics|Performance Levels|Masters|Percentage` = `STAAR - Mathematics|Performance Levels|Masters|Percentage`/100)
staar_state %<>%
  mutate(`STAAR - Mathematics|Performance Levels|Did Not Meet|Percentage` = `STAAR - Mathematics|Performance Levels|Did Not Meet|Percentage`/100,
         `STAAR - Mathematics|Performance Levels|Approaches and Above|Percentage` = `STAAR - Mathematics|Performance Levels|Approaches and Above|Percentage`/100,
         `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage` = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`/100,
         `STAAR - Mathematics|Performance Levels|Masters|Percentage` = `STAAR - Mathematics|Performance Levels|Masters|Percentage`/100,
         `STAAR - Reading|Performance Levels|Did Not Meet|Percentage` = `STAAR - Reading|Performance Levels|Did Not Meet|Percentage`/100,
         `STAAR - Reading|Performance Levels|Approaches and Above|Percentage` = `STAAR - Reading|Performance Levels|Approaches and Above|Percentage`/100,
         `STAAR - Reading|Performance Levels|Meets and Above|Percentage` = `STAAR - Reading|Performance Levels|Meets and Above|Percentage`/100,
         `STAAR - Reading|Performance Levels|Masters|Percentage` = `STAAR - Reading|Performance Levels|Masters|Percentage`/100)
district_snapshot %<>%
  mutate(DPETECOP = DPETECOP/100,
         DPETLEPP = DPETLEPP/100)

# Create numeric Year column for state STAAR data
staar_state %<>%
  mutate(Year = as.numeric(str_sub(Administration, start = -4)))

# Merge snapshot with district STAAR
district_df <- inner_join(staar_district, district_snapshot, by = c("ID/CDC" = "DISTRICT"))

# (1) Scatterplots and Line Graphs ####
# Statewide % Meets Math Across Years
ggplot(staar_state, aes()) + 
  geom_# scatterplot
# District % Meets Math vs % Economically Disadvantaged
ggplot(district_df, aes()) + 
  geom_# scatterplot
# Add smoothed line
ggplot(district_df, aes()) + 
  geom_# scatterplot + 
  geom_smooth(color = "blue") + 
# Add linear regression line 
  geom_smooth(color = "red", method = "lm")
# District % Meets vs % English Learners
ggplot(district_df, aes()) + 
  geom_point() + 
  geom_smooth(color = "blue") + 
  geom_smooth(color = "red", method = "lm")

# Separate state STAAR performance by grade level
# Start with adding a color aesthetic for grade level
ggplot(staar_state, aes()) + 
  geom_point()

# Different colored points are hard to see, so try lines
# Math
ggplot(staar_state, aes()) + 
  geom_#line graph
# Reading
ggplot(staar_state, aes()) + 
  geom_#line graph

# (2) Histograms, Boxplots, and Dotplots ####
# Create a histogram of District % Meets STAAR Math
ggplot(staar_district, aes()) + 
  geom_# #
# Separate histograms by Region
ggplot(district_df, aes()) + 
  geom_# # + 
  facet_wrap()
# Region Boxplots or Dotplots
ggplot(district_df, aes()) + 
  geom_# # + 
  facet_wrap()
ggplot(district_df, aes()) + 
  geom_# #(binaxis = "y", stackdir = "center") + 
  facet_wrap()

# (3) Labels and Pretty Graphs ####
# Let's make some nicely formatted graphs with axis labels and automatic percent formatting
ggplot(district_df, aes(x = DPETECOP, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`)) + 
  geom_point() + 
  geom_smooth(color = "blue") + 
  # Label x-axis, y-axis, and title
  labs() + 
  scale_x_continuous(labels = scales::percent) + 
  scale_y_continuous(labels = scales::percent)

# Factor/categorical variables are assigned colors differently than numeric variables
# Math
ggplot(staar_state, aes(x = Year, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`, group = as.factor(`Tested Grade`), color = as.factor(`Tested Grade`))) + 
  geom_line() + 
  # Label x-axis, y-axis, title, and legend (color)
  labs() + 
  scale_y_continuous(labels = scales::percent)
# Reading
ggplot(staar_state, aes(x = Year, y = `STAAR - Reading|Performance Levels|Meets and Above|Percentage`, group = as.factor(`Tested Grade`), color = as.factor(`Tested Grade`))) + 
  geom_line() + 
  labs() + 
  scale_y_continuous(labels = scales::percent)

# Bonus: Visualizing Spatial Data ####
# School District shapefiles
district_shapefile_url <- "https://services2.arcgis.com/5MVN2jsqIrNZD4tP/arcgis/rest/services/Current_Districts/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json"
district_shapefile <- parse_url(district_shapefile_url)
request <- build_url(district_shapefile)
district_geodata <- st_read(request)
district_geodata <- st_make_valid(district_geodata)

# Basic district polygon map
ggplot(district_geodata) + geom_sf(fill = "#0d6cb9")

# District shapefile merged with district staar/snapshot
district_df_geo = inner_join(district_geodata, district_df, by = c("DISTRICT_C" = "ID/CDC"), suffix = c("_geo", ""))

# STAAR % Meets Map
ggplot(district_df_geo) + 
  geom_sf(aes()) + 
  scale_fill_continuous(labels = scales::percent) + 
  # Add new title for Legend (fill)
  labs()
# District % Ecodis Map
ggplot(district_df_geo) + 
  geom_sf(aes()) + 
  scale_fill_continuous(labels = scales::percent) + 
  # Add new title for Legend (fill)
  labs()
# District Types: categorical variable
ggplot(district_df_geo) + 
  geom_sf(aes()) + 
  # Add title for Legend (fill)
  labs()


# Final Examples ####
# STAAR trends with text labels and rectangle for STAAR redesign ####
ggplot(staar_state, aes(x = Year, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`, group = as.factor(`Tested Grade`), color = as.factor(`Tested Grade`))) + 
  geom_line() + 
  # Create rectangle to show timing of STAAR Redesign in 2023
    # Includes x and y min/max values, fill color, and alpha for opacity (0 to 1)
  annotate("rect", ) + 
  # Add text labels to each data point
    # Requires x/y coordinates and the column for the label text (label)
  geom_label(aes()) +
  scale_y_continuous(labels = scales::percent) + 
  # Add labels for x-axis, y-axis, legend (color)
  labs() 
# ggrepel package allows you to add text labels that naturally spread out and won't overlap (geom_label_repel)

# STAAR Districts graph with specific districts labeled with text
district_list <- c() # Create vector of District Names in quotes
district_subset <- district_df %>% 
  filter(Organization %in% district_list)
ggplot(district_df, aes(x = DPETECOP, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`)) + 
  geom_point() + 
  geom_smooth(color = "blue") + 
  # Add highlighted points from the district subset
  geom_point() +  
  # Add labels to show the names of highlighted districts
    # Customize labels with text size, hjust (horizontal alignment), vjust (vertical alignment)
  geom_label() +
  scale_x_continuous(labels = scales::percent) + 
  scale_y_continuous(labels = scales::percent) + 
  labs()
# Text labels with ggrepel will automatically avoid covering the highlighted points and other labels
 
  
