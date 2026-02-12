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
ggplot(staar_state, aes(x = Year, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`)) + 
  geom_point()
# District % Meets Math vs % Economically Disadvantaged
ggplot(district_df, aes(x = DPETECOP, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`)) + 
  geom_point()
# Add smoothed line
ggplot(district_df, aes(x = DPETECOP, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`)) + 
  geom_point() + 
  geom_smooth(color = "blue") + 
# Add linear regression line 
  geom_smooth(color = "red", method = "lm")
# District % Meets vs % English Learners
ggplot(district_df, aes(x = DPETLEPP, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`)) + 
  geom_point() + 
  geom_smooth(color = "blue") + 
  geom_smooth(color = "red", method = "lm")

# Separate state STAAR performance by grade level
ggplot(staar_state, aes(x = Year, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`, color = `Tested Grade`)) + 
  geom_point()
# Different colored points are hard to see, so try lines
ggplot(staar_state, aes(x = Year, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`, group = `Tested Grade`, color = `Tested Grade`)) + 
  geom_line()
ggplot(staar_state, aes(x = Year, y = `STAAR - Reading|Performance Levels|Meets and Above|Percentage`, group = `Tested Grade`, color = `Tested Grade`)) + 
  geom_line()

# Line plots with different themes
ggplot(staar_state, aes(x = Year, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`, group = `Tested Grade`, color = `Tested Grade`)) + 
  geom_line()
ggplot(staar_state, aes(x = Year, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`, group = `Tested Grade`, color = `Tested Grade`)) + 
  geom_line() + 
  theme_light()
ggplot(staar_state, aes(x = Year, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`, group = `Tested Grade`, color = `Tested Grade`)) + 
  geom_line() + 
  theme_classic()
ggplot(staar_state, aes(x = Year, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`, group = `Tested Grade`, color = `Tested Grade`)) + 
  geom_line() + 
  theme_minimal()
# Scatterplot with different themes
ggplot(district_df, aes(x = DPETECOP, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`)) + 
  geom_point()
ggplot(district_df, aes(x = DPETECOP, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`)) + 
  geom_point(color = "yellow") + 
  theme_dark()
ggplot(district_df, aes(x = DPETECOP, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`)) + 
  geom_point() + 
  theme_bw()

# (2) Histograms, Boxplots, and Dotplots ####
ggplot(staar_district, aes(x = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`)) + 
  geom_histogram()
# Separate histograms by Region
ggplot(district_df, aes(x = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`)) + 
  geom_histogram() + 
  facet_wrap(~REGION)
# Region Boxplots or Dotplots
ggplot(district_df, aes(y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`)) + 
  geom_boxplot() + 
  facet_wrap(~REGION, nrow = 2)
ggplot(district_df, aes(x = 1, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`)) + 
  geom_dotplot(binaxis = "y", stackdir = "center") + 
  facet_wrap(~REGION, nrow = 2)

# (3) Labels and Pretty Graphs ####
# Let's make some nicely formatted graphs with axis labels and automatic percent formatting
ggplot(district_df, aes(x = DPETECOP, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`)) + 
  geom_point() + 
  geom_smooth(color = "blue") + 
  labs(x = "District % Economically Disadvantaged", y = "District % STAAR Meets and Above 8th Grade Math", title = "% Meets Grade 8 Math vs % Economically Disadvantage with Smoothed Line") + 
  scale_x_continuous(labels = scales::percent) + 
  scale_y_continuous(labels = scales::percent) #+ 
  #theme_classic()

# Factor/categorical variables are assigned colors differently than numeric variables
ggplot(staar_state, aes(x = Year, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`, group = as.factor(`Tested Grade`), color = as.factor(`Tested Grade`))) + 
  geom_line() + 
  labs(x = "Year", y = "District % Meets STAAR", title = "Mathematics STAAR Trends by Grade Level", color = "Grade Level") + 
  scale_y_continuous(labels = scales::percent)

ggplot(staar_state, aes(x = Year, y = `STAAR - Reading|Performance Levels|Meets and Above|Percentage`, group = as.factor(`Tested Grade`), color = as.factor(`Tested Grade`))) + 
  geom_line() + 
  labs(x = "Year", y = "District % Meets STAAR", title = "Reading STAAR Trends by Grade Level", color = "Grade Level") + 
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
district_df_geo = left_join(district_geodata, district_df, by = c("DISTRICT_C" = "ID/CDC"), suffix = c("_geo", ""))

# STAAR % Meets Map
ggplot(district_df_geo) + 
  geom_sf(aes(fill = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`)) + 
  scale_fill_continuous(labels = scales::percent) + 
  labs(fill = "% Meets Grade 8 Math")
# District % Ecodis Map
ggplot(district_df_geo) + 
  geom_sf(aes(fill = DPETECOP)) + 
  scale_fill_continuous(labels = scales::percent) + 
  labs(fill = "District % Ecodis")
# District Types: categorical variable
ggplot(district_df_geo) + 
  geom_sf(aes(fill = COMMTYPE))

# Change color scales in these maps
# Emphasize missing districts
ggplot(district_df_geo) + 
  geom_sf(aes(fill = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`)) + 
  #scale_fill_continuous(labels = scales::percent) +
  scale_fill_gradient(low = "grey50", high = "white", labels = scales::percent, na.value = "yellow") + 
  labs(fill = "% Meets Grade 8 Math")
# Blue scale, emphasizing high ecodis districts
ggplot(district_df_geo) + 
  geom_sf(aes(fill = DPETECOP)) + 
  scale_fill_gradient(low = "white", high = "blue", labels = scales::percent, na.value = "grey50") + 
  labs(fill = "District % Ecodis")

# Final Examples ####
# STAAR trends with text labels and rectangle for STAAR redesign ####
ggplot(staar_state, aes(x = Year, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`, group = as.factor(`Tested Grade`), color = as.factor(`Tested Grade`))) + 
  geom_line() + 
  annotate("rect", xmin = 2022.7, xmax = 2023.3, ymin = 0.25, ymax = 0.50, alpha = .5, fill = "grey") + 
  geom_label(aes(x = Year, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`, label = scales::percent(`STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`, scale=100))) +
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "% Meets STAAR", title = "Mathematics STAAR Trends", color = "Grade Level") 
# ggrepel package allows you to add text labels that naturally spread out and won't overlap
ggplot(staar_state, aes(x = Year, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`, group = as.factor(`Tested Grade`), color = as.factor(`Tested Grade`))) + 
  geom_line() + 
  annotate("rect", xmin = 2022.7, xmax = 2023.3, ymin = 0.25, ymax = 0.50, alpha = .5, fill = "grey") + 
  ggrepel::geom_label_repel(aes(x = Year, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`, label = scales::percent(`STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`, scale=100))) +
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "% Meets STAAR", title = "Mathematics STAAR Trends", color = "Grade Level") 

# STAAR Districts graph with specific districts labeled with text
district_list <- c("HOUSTON ISD", "DALLAS ISD", "FORT WORTH ISD", "AUSTIN ISD")
district_subset <- district_df %>% 
  filter(Organization %in% district_list)
ggplot(district_df, aes(x = DPETECOP, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`)) + 
  geom_point() + 
  geom_point(data = district_subset, aes(x = DPETECOP, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`), color = "red", size = 5, shape = 18) + 
  geom_label(data = district_subset, aes(x = DPETECOP, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`, label = Organization), size = 2.5, hjust = -0.1) +
  scale_x_continuous(labels = scales::percent) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "District % Economically Disadvantaged", y = "District % STAAR Meets and Above Grade 8 Math")
# Text labels with ggrepel
ggplot(district_df, aes(x = DPETECOP, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`)) + 
  geom_point() + 
  geom_point(data = district_subset, aes(x = DPETECOP, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`), color = "red", size = 5, shape = 18) + 
  ggrepel::geom_label_repel(data = district_subset, aes(x = DPETECOP, y = `STAAR - Mathematics|Performance Levels|Meets and Above|Percentage`, label = Organization), size = 2.5) + 
  scale_x_continuous(labels = scales::percent) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "District % Economically Disadvantaged", y = "District % STAAR Meets and Above Grade 8 Math")
 
  
