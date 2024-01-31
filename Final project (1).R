#### Install and load libraries ####
devtools::install_github('UrbanInstitute/urbnmapr')
install.packages("maps")
install.packages(c("plyr"))
library(ggplot2)
library(maps)
library(plyr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(maps)
library(ggthemes) 
library(tidyverse)
library(urbnmapr)
library(gridExtra)
library(magick)
library(gganimate)


#Reading the dataset 
df <- read.csv("/Users/mtajik/Desktop/Data Visualization/Final project/annual_aqi_by_county_2022.csv")

#### --------------------> Data Cleaning for Air Quality days across states <-------------------------####
#Percentage of good days
df['goodpct'] = df['Good.Days'] / df['Days.with.AQI']

# Reorder columns with 'goodpct' after 'Good Days'
target_variable <- 'Good.Days'
column_to_move <- 'goodpct'
df <- df %>%
  select(names(df)[1:which(names(df) == target_variable)], 
         column_to_move,
         names(df)[(which(names(df) == target_variable) + 1):length(names(df))])


#Percentage of Moderate days
df['moderatepct'] = df['Moderate.Days'] / df['Days.with.AQI']

# Reorder columns with 'moderatepct' after 'Moderate Days'
target_variable <- 'Moderate.Days'
column_to_move <- 'moderatepct'
df <- df %>%
  select(names(df)[1:which(names(df) == target_variable)], 
         column_to_move,
         names(df)[(which(names(df) == target_variable) + 1):length(names(df))])


#Percentage of Unhealhty for sensitive groups days
df['Sensitivepct'] = df['Unhealthy.for.Sensitive.Groups.Days'] / df['Days.with.AQI']

# Reorder columns with 'moderatepct' after 'Moderate Days'
target_variable <- 'Unhealthy.for.Sensitive.Groups.Days'
column_to_move <- 'Sensitivepct'
df <- df %>%
  select(names(df)[1:which(names(df) == target_variable)], 
         column_to_move,
         names(df)[(which(names(df) == target_variable) + 1):length(names(df))])

#Percentage of unhealthy days
df['Unhealthypct'] = df['Unhealthy.Days'] / df['Days.with.AQI']

# Reorder columns with 'moderatepct' after 'Moderate Days'
target_variable <- 'Unhealthy.Days'
column_to_move <- 'Unhealthypct'
df <- df %>%
  select(names(df)[1:which(names(df) == target_variable)], 
         column_to_move,
         names(df)[(which(names(df) == target_variable) + 1):length(names(df))])


#Percentage of Very Unhealthy days
df['Very.Unhealthypct'] = df['Very.Unhealthy.Days'] / df['Days.with.AQI']

# Reorder columns with 'moderatepct' after 'Moderate Days'
target_variable <- 'Very.Unhealthy.Days'
column_to_move <- 'Very.Unhealthypct'
df <- df %>%
  select(names(df)[1:which(names(df) == target_variable)], 
         column_to_move,
         names(df)[(which(names(df) == target_variable) + 1):length(names(df))])

#Percentage of Hazardous days
df['Hazardouspct'] = df['Hazardous.Days'] / df['Days.with.AQI']

# Reorder columns with 'moderatepct' after 'Moderate Days'
target_variable <- 'Hazardous.Days'
column_to_move <- 'Hazardouspct'
df <- df %>%
  select(names(df)[1:which(names(df) == target_variable)], 
         column_to_move,
         names(df)[(which(names(df) == target_variable) + 1):length(names(df))])


#Checkc. if there are nul values in the entire dataset 
missing_values <- colSums(is.na(df))

# Print columns with missing values
for (col_name in names(missing_values)) {
  if (missing_values[col_name]) {
    cat(paste("Column '", col_name, "' has missing values.\n", sep = ""))
  } else {
    cat(paste("Column '", col_name, "' does not have missing values.\n", sep = ""))
  }
}


#Create column Region and assign states to them
df <- df %>%
  mutate(
    Region = case_when(
      State %in% c('New York', 'New Jersey', 'Pennsylvania', 'Connecticut', 'Maine', 'Massachusetts', 'New Hampshire', 'Rhode Island', 'Vermont') ~ 'Northeast',
      State %in% c('Illinois','Indiana', 'Indiana', 'Michigan', 'Ohio', 'Wisconsin','Iowa', 'Kansas', 'Minnesota', 'Missouri', 'Nebraska', 'North Dakota', 'South Dakota') ~ 'MidWest',
      State %in% c('Delaware','District Of Columbia', 'Florida', 'Georgia', 'Maryland', 'North Carolina','South Carolina', 'Virginia', 'West Virginia','Alabama', 'Kentucky', 'Mississippi', 'Tennessee', 'Arkansas', 'Louisiana', 'Oklahoma', 'Texas') ~ 'South',
      State %in% c('Arizona', 'Colorado', 'Idaho', 'New Mexico', 'Montana', 'Utah', 'Nevada', 'Wyoming', 'Alaska','California','Hawaii','Washington','Oregon') ~ 'West',
      TRUE ~ 'Other'  # Default value for other cases
    )
  )

#Move column region next to County column
target_variable <- 'County'
column_to_move <- 'Region'
df <- df %>%
  select(names(df)[1:which(names(df) == target_variable)], 
         column_to_move,
         names(df)[(which(names(df) == target_variable) + 1):length(names(df))])




####-----------> Mapping Good Days, Unhealthy Days and Hazardous Days accross states <----------------####
state_level_df <- data.frame(
  State = state.name,
  long = state.center$x,
  lat = state.center$y,
  stringsAsFactors = FALSE
) %>%
  inner_join(df, by = "State")
options(repr.plot.width = 10, repr.plot.height = 6)
ggplot(state_level_df, aes(long, lat)) +
  borders("state", fill = "snow2") +
  geom_point(aes(x = long, y = lat, size = Hazardous.Days, color = Hazardouspct)) +
  geom_text(aes(label = State), vjust = 1.5, size = 2.3) +
  theme(text = element_text(size = 6)) +
  scale_size_continuous(
    range = c(2, 6),
    #labels = scales::percent,
    name = "Hazardous Days",
    breaks = c(0, 3, 6, 9) # Adjust the breaks based on your dataset
  ) +
  scale_color_continuous(
    low = "yellowgreen",
    high = "red",
    name = "Hazardous Days Level",
    labels = scales::percent
  ) +
  theme_bw()


#### Mapping Good Days
state_level_df <- data.frame(
  State = state.name,
  long = state.center$x,
  lat = state.center$y,
  stringsAsFactors = FALSE
) %>%
  inner_join(df, by = "State")
options(repr.plot.width = 10, repr.plot.height = 6)
ggplot(state_level_df, aes(long, lat)) +
  borders("state", fill = "snow2") +
  geom_point(aes(x = long, y = lat, size = Good.Days, color = goodpct), 
             show.legend = TRUE) + 
  geom_text(aes(label = State), vjust = 1.5, size = 2.3) +
  theme(text = element_text(size = 6)) +
  scale_size_continuous(
    range = c(2, 6),
    name = "Good Days",
    breaks = c(0, 100, 200, 300, 400, 500 )
  ) +
  scale_color_continuous(
    low = "red",
    high = "yellowgreen",
    name = "Good Days Level",
    labels = scales::percent
  ) +
  theme_bw()


##### Mapping Unhealthy Days
state_level_df <- data.frame(
  State = state.name,
  long = state.center$x,
  lat = state.center$y,
  stringsAsFactors = FALSE
) %>%
  inner_join(df, by = "State")
options(repr.plot.width = 10, repr.plot.height = 6)
ggplot(state_level_df, aes(long, lat)) +
  borders("state", fill = "snow2") +
  geom_point(aes(x = long, y = lat, size = Unhealthy.Days, color = Unhealthypct), 
             show.legend = TRUE) + # Add show.legend = TRUE
  geom_text(aes(label = State), vjust = 1.5, size = 2.3) +
  theme(text = element_text(size = 7)) +
  scale_size_continuous(
    range = c(2, 6),
    name = "Unhealthy Days",
    breaks = c(0, 10, 20, 30, 40, 50)
  ) +
  scale_color_continuous(
    low = "yellowgreen",
    high = "red",
    name = "Unhealthy Days Level",
    labels = scales::percent
  ) +
  theme_bw()

states_map <- get_urbn_map(map = "states", sf = TRUE)

# Rename the 'State' column in the 'df' to match the column name in the map data
#names(df)[which(names(df) == "State")] <- "state_name"
# make Median.AQI numeric
state_aqi$Median_AQI <- as.numeric(state_aqi$Median_AQI)

# Aggregate Median.AQI data to the state level
state_aqi <- df %>%
  group_by(state_name) %>%
  summarise(Median_AQI = median(Median.AQI, na.rm = TRUE))

# Merge state-level map data with Median.AQI data
state_map_aqi <- left_join(states_map, state_aqi, by = c("state_name" = "state_name"))

# Check for missing values in the Median AQI column
missing_aqi <- sum(is.na(state_map_aqi$Median_AQI))
if (missing_aqi > 0) {
  cat("Warning: There are", missing_aqi, "states with missing Median AQI values.")
}

# Create a choropleth map depicting Median AQI by State
ggplot() +
  geom_sf(data = state_map_aqi, aes(fill = Median_AQI)) +
  scale_fill_gradient(low = "forestgreen", high = "lightyellow", name = "Median AQI") +
  labs(title = "Median Air Quality Index (AQI) by State")

####----------------------> Data Cleaning for County Level Mapping <---------------------------------####
## Changing State and County to lower case to match them and extract fips
df$State <- tolower(df$State)
df$County <- tolower(df$County)

#Load fips for counties 
data("county.fips")

# Split CountyName column into separate State and County columns
county.fips <- county.fips %>%
  separate(polyname, into = c("State", "County"), sep = ",", remove = FALSE, extra = "drop")


#Add some counties to counties.fips that are available in df, but not in counties.fips
new_row <- data.frame(
  fips = '2013',          
  State = 'alaska',  
  County = 'aleutians east '
)

# Make sure the data types match
county.fips <- county.fips %>%
  mutate(
    County = as.character(County),  
    fips = as.character(fips)  
  ) %>%
  add_row(.before = 1, new_row) 

new_row <- data.frame(
  fips = '2020',          
  State = 'alaska',  
  County = 'anchorage '
)

# Make sure the data types match
county.fips <- county.fips %>%
  mutate(
    County = as.character(County),  
    fips = as.character(fips)  
  ) %>%
  add_row(.before = 1, new_row) 

new_row <- data.frame(
  fips = '2068',          
  State = 'alaska',  
  County = 'denali '
)

# Make sure the data types match
county.fips <- county.fips %>%
  mutate(
    County = as.character(County),  
    fips = as.character(fips)  
  ) %>%
  add_row(.before = 1, new_row) 

new_row <- data.frame(
  fips = '2090',          
  State = 'alaska',  
  County = 'fairbanks north star '
)

# Make sure the data types match
county.fips <- county.fips %>%
  mutate(
    County = as.character(County),  
    fips = as.character(fips)  
  ) %>%
  add_row(.before = 1, new_row) 

new_row <- data.frame(
  fips = '2110',          
  State = 'alaska',  
  County = 'juneau '
)

# Make sure the data types match
county.fips <- county.fips %>%
  mutate(
    County = as.character(County),  
    fips = as.character(fips)  
  ) %>%
  add_row(.before = 1, new_row)


new_row <- data.frame(
  fips = '2122',          
  State = 'alaska',  
  County = 'kenai peninsula '
)

# Make sure the data types match
county.fips <- county.fips %>%
  mutate(
    County = as.character(County),  
    fips = as.character(fips)  
  ) %>%
  add_row(.before = 1, new_row)

new_row <- data.frame(
  fips = '2170',          
  State = 'alaska',  
  County = 'matanuska-susitna '
)

# Make sure the data types match
county.fips <- county.fips %>%
  mutate(
    County = as.character(County),  
    fips = as.character(fips)  
  ) %>%
  add_row(.before = 1, new_row)

new_row <- data.frame(
  fips = '2185',          
  State = 'alaska',  
  County = 'north slope '
)

# Make sure the data types match
county.fips <- county.fips %>%
  mutate(
    County = as.character(County),  
    fips = as.character(fips)  
  ) %>%
  add_row(.before = 1, new_row)


new_row <- data.frame(
  fips = '15000',          
  State = 'hawaii',  
  County = 'hawaii'
)

# Make sure the data types match
county.fips <- county.fips %>%
  mutate(
    County = as.character(County),  
    fips = as.character(fips)  
  ) %>%
  add_row(.before = 1, new_row)


new_row <- data.frame(
  fips = '15003',          
  State = 'hawaii',  
  County = 'honolulu'
)

# Make sure the data types match
county.fips <- county.fips %>%
  mutate(
    County = as.character(County),  
    fips = as.character(fips)  
  ) %>%
  add_row(.before = 1, new_row)

new_row <- data.frame(
  fips = '15007',          
  State = 'hawaii',  
  County = 'kauai'
)

# Make sure the data types match
county.fips <- county.fips %>%
  mutate(
    County = as.character(County),  
    fips = as.character(fips)  
  ) %>%
  add_row(.before = 1, new_row)

new_row <- data.frame(
  fips = '15009',          
  State = 'hawaii',  
  County = 'maui'
)

# Make sure the data types match
county.fips <- county.fips %>%
  mutate(
    County = as.character(County),  
    fips = as.character(fips)  
  ) %>%
  add_row(.before = 1, new_row)



#Delete some counties and states that their fips are not available either online or in counties.fips
df <- df %>%
  filter(State != "country of mexico" &
           State != "puerto rico" &
           State != "virgin islands" &
           County != "bristol city" &
           County != "hampton city" &
           County != "hopewell city" &
           County != "salem city")


#Correct the spelling of countoes based on countoes.fips so that we can extract fips later based on them
df <- df %>%
  mutate(
    County = case_when(
      County %in% c('dekalb') ~ 'de kalb',
      County %in% c('district of columbia') ~ 'washington',
      County %in% c('okaloosa') ~ 'okaloosa:main',
      County %in% c('st. lucie') ~ 'st lucie',
      County %in% c('dupage') ~ 'du page',
      County %in% c('saint clair') ~ 'st clair',
      County %in% c('laporte') ~ 'la porte',
      County %in% c('st. joseph') ~ 'st joseph',
      County %in% c('st. bernard') ~ 'st bernard',
      County %in% c('st. james') ~ 'st james',
      County %in% c('st. john the baptist') ~ 'st john the baptist',
      County %in% c('st. martin') ~ 'st martin:south',
      County %in% c('st. tammany') ~ 'st tammany',
      County %in% c('baltimore (city)') ~ 'baltimore',
      County %in% c("prince george's") ~ 'prince georges',
      County %in% c("st. clair") ~ 'st clair',
      County %in% c("saint louis") ~ 'st louis',
      County %in% c("desoto") ~ 'de soto',
      County %in% c("saint charles") ~ 'st charles',
      County %in% c("sainte genevieve") ~ 'ste genevieve',
      County %in% c("st. louis city") ~ 'st louis city',
      County %in% c("galveston") ~ 'galveston:main',
      County %in% c("lynchburg city") ~ 'lunenburg',
      County %in% c("richmond city") ~ 'richmond',
      County %in% c("suffolk city") ~ 'suffolk',
      County %in% c("norfolk city") ~ 'norfolk',
      County %in% c("virginia beach city") ~ 'virginia beach',
      TRUE ~ County  
    )
  )

#Initialize an empty column for fips in df
df$county_fips <- NA

#Iterate through counties and assign fips 
for (i in 1:nrow(df)) {
  # Find the matching row in County.fips
  matching_row <- county.fips$County == df$County[i]
  
  # Assign the value from "fips" column to "county_fips" column in df
  df$county_fips[i] <- county.fips$fips[matching_row]
}

df <- df %>%
  mutate(
    county_fips = case_when(
      County %in% c('aleutians east ') ~ '02013',
      TRUE ~ county_fips  # Default value for other cases
    )
  )

#Change fips tpo character 
df$county_fips <- as.character(df$county_fips)

#Fetch counties map
counties %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(fill = "grey", color = "#ffffff", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)



####----------------------------------> Mapping Good Days in county level <--------------------------------------- ####
County_data_good_days <- left_join(df, counties, by = "county_fips") 

final_map <- ggplot() +
  # Adding the base map with grey color for states and counties not covered
  geom_map(data = map_data("county"), map = map_data("county"),
           aes(x = long, y = lat, map_id = region),
           fill = "lightgrey", color = "grey", size = 0.5) +  # Set fill to NA for a transparent background
  geom_map(data = map_data("state"), map = map_data("state"),
           aes(x = long, y = lat, map_id = region),
           fill = "lightgrey", color = "grey", size = 0.5) +  # Set fill to NA for a transparent background
  # Your second code snippet for the filled counties with the correct variable name
  geom_polygon(data = County_data_good_days, 
               aes(x = long, y = lat, group = group, fill = goodpct),
               color = "#ffffff", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45, xlim = c(-120, -70), ylim = c(24, 50)) +
  # Set the color scale with low to high gradient
  scale_fill_gradient(name = "Good Days", 
                      low = "darkolivegreen2", high = "olivedrab", 
                      labels = scales::percent_format(),
                      guide = guide_colorbar(reverse = FALSE)) +
  # Remove axis labels and titles
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


final_map

#####------------------------------> Pollutants by region (Violin Plots) #####<------------------------------------------
theme_set(theme_minimal())
df$Region <- factor(df$Region)
#CO2
ggplot(df, aes(x = Region, y = Days.CO, fill = Region)) +
  geom_violin(trim = T) +
  labs(title = "CO2 Days by Region", x = "Region", y = "CO2 Days") +
  scale_fill_manual(values = c("MidWest" = "lightblue", "Northeast" = "lightgreen", "South" = "lightpink", "West" = "lightyellow")) +
  theme(legend.position = "right")

theme_set(theme_minimal(base_size = 14) + theme(legend.position = "right"))

p <- ggplot(df, aes(x = Region, y = Days.CO, fill = Region)) +
  geom_violin(trim =F, alpha = 0.7, color = "white", size = 1.5) +
  labs(title = "Carbon Monoxide Days by Region", x = "Region", y = "Carbon Monoxide Days") +
  scale_fill_manual(values = c("MidWest" = "#4e79a7", "Northeast" = "#59a14f", "South" = "#f28e2c", "West" = "#e15759")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray", size = 0.2),
    panel.grid.minor = element_blank(),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1.5, "lines"),
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    strip.text = element_text(size = 14),
    legend.position = "right"
  ) + ylim(0,366)+
  geom_jitter(shape = 16, position = position_jitter(0.2), color = "black", alpha = 0.5,size=.5) +
  theme(legend.position = "right")
print(p)

#NO2
theme_set(theme_minimal(base_size = 14) + theme(legend.position = "right"))

p <- ggplot(df, aes(x = Region, y = Days.NO2, fill = Region)) +
  geom_violin(trim =F, alpha = 0.7, color = "white", size = 1.5) +
  labs(title = "Nitrogen Dioxide Days by Region", x = "Region", y = "Nitrogen Dioxide Days") +
  scale_fill_manual(values = c("MidWest" = "#4e79a7", "Northeast" = "#59a14f", "South" = "#f28e2c", "West" = "#e15759")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray", size = 0.2),
    panel.grid.minor = element_blank(),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1.5, "lines"),
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    strip.text = element_text(size = 14),
    legend.position = "right"
  ) + ylim(0,366)+
  geom_jitter(shape = 16, position = position_jitter(0.2), color = "black", alpha = 0.5,size=.5) +
  theme(legend.position = "right")

# Print the plot
print(p)

#Ozone
theme_set(theme_minimal(base_size = 14) + theme(legend.position = "right"))

p <- ggplot(df, aes(x = Region, y = Days.Ozone, fill = Region)) +
  geom_violin(trim =F, alpha = 0.7, color = "white", size = 1.5) +
  labs(title = "Ozone Days by Region", x = "Region", y = "Ozone Days") +
  scale_fill_manual(values = c("MidWest" = "#4e79a7", "Northeast" = "#59a14f", "South" = "#f28e2c", "West" = "#e15759")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray", size = 0.2),
    panel.grid.minor = element_blank(),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1.5, "lines"),
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    strip.text = element_text(size = 14),
    legend.position = "right"
  ) +ylim(0,366)+
  geom_jitter(shape = 16, position = position_jitter(0.2), color = "black", alpha = 0.5,size=.5) +
  theme(legend.position = "right")

# Print the plot
print(p)


#PM2.5
theme_set(theme_minimal(base_size = 14) + theme(legend.position = "right"))

p <- ggplot(df, aes(x = Region, y = Days.PM2.5, fill = Region)) +
  geom_violin(trim =F, alpha = 0.7, color = "white", size = 1.5) +
  labs(title = "Small Particulate Matter by Region", x = "Region", y = "PM 2.5 Days") +
  scale_fill_manual(values = c("MidWest" = "#4e79a7", "Northeast" = "#59a14f", "South" = "#f28e2c", "West" = "#e15759")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray", size = 0.2),
    panel.grid.minor = element_blank(),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1.5, "lines"),
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    strip.text = element_text(size = 14),
    legend.position = "right"
  ) + ylim(0,366)+
  geom_jitter(shape = 16, position = position_jitter(0.2), color = "black", alpha = 0.5,size=.5) +
  theme(legend.position = "right")

# Print the plot
print(p)

####--------------------> Statistics Summary (Pie chart and bar chart) <------------------------------------------####

# Splitting data into regions
West_Region <- subset(df, Region == 'West')
MidWest_Region <- subset(df, Region == 'MidWest')
Northeast_Region <- subset(df, Region == 'Northeast')
South_Region <- subset(df, Region == 'South')

# Calculate average AQI values for the USA and each region
USA_summary <- df %>% summarize(
  goodavg = mean(goodpct),
  modavg = mean(moderatepct),
  sensavg = mean(Sensitivepct),
  unhealthyavg = mean(Unhealthypct),
  veryavg = mean(Very.Unhealthypct),
  hazardavg = mean(Hazardouspct)
)

# Reshape USA summary data for plotting
USA_summary <- pivot_longer(USA_summary, 1:6, names_to = "AQI", values_to = "days_pct")
USA_summary$AQI <- factor(USA_summary$AQI, levels = c("goodavg", "modavg", "sensavg", "unhealthyavg", "veryavg", "hazardavg"))

# Calculating average AQI values for each region and reshaping data for plotting
regions_summary <- lapply(list(West_Region, MidWest_Region, Northeast_Region, South_Region), function(region) {
  region_summary <- region %>% summarize(
    goodavg = mean(goodpct),
    modavg = mean(moderatepct),
    sensavg = mean(Sensitivepct),
    unhealthyavg = mean(Unhealthypct),
    veryavg = mean(Very.Unhealthypct),
    hazardavg = mean(Hazardouspct)
  )
  region_summary <- pivot_longer(region_summary, 1:6, names_to = "AQI", values_to = "days_pct")
  region_summary$AQI <- factor(region_summary$AQI, levels = c("goodavg", "modavg", "sensavg", "unhealthyavg", "veryavg", "hazardavg"))
  return(region_summary)
})

# Plotting a 3D pie chart for the West region
library(plotrix)
pie3D(
  regions_summary[[1]]$days_pct,
  radius = 1.5,
  theta = 0.7,
  border = "white",
  shade = 0.5,
  col = c("forestgreen", "olivedrab3", "gold1", "darkorange", "brown3", "deeppink4"),
  explode = 0.1,
  main = "AQI in the West Region - % of Days Measured"
)

# Adding legend above the pie chart
legend(
  "top",
  inset = c(0, 0),
  legend = regions_summary[[1]]$AQI,
  fill = c("forestgreen", "olivedrab3", "gold1", "darkorange", "brown3", "deeppink4"),
  title = "AQI Category",
  cex = 0.5,
  horiz = TRUE,
  xpd = TRUE
)

# Creating a bar chart for AQI categories in the USA
ggplot(data = USA_summary, aes(x = (AQI), y = days_pct)) +
  geom_bar(stat = "identity", fill = c("forestgreen", "gold1", "darkorange", "brown2", "deeppink4", "purple4")) +
  theme_bw() + ylab("Days Measured") + labs(title = "AQI in the US") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c("Good", "Moderate", "Unhealthy for \nSensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous"))


#####------------------------------> Histogram #####<------------------------------------------

# Select only the necessary columns related to AQI metrics
aqi_metrics <- air_quality_data %>%
  select(Good.Days, Moderate.Days, Unhealthy.for.Sensitive.Groups.Days, Unhealthy.Days, Very.Unhealthy.Days, Hazardous.Days)

# Define distinct colors for each AQI metric
colors <- c("forestgreen", "gold1", "darkorange", "brown2", "deeppink4", "purple4")

# Plot histograms for AQI metrics with different colors and label x-axis as "Number of Days"
par(mfrow = c(3, 2), mar = c(4, 4, 2, 2))  # Organize plots in a 3x2 grid with increased bottom margin

# Loop through each AQI metric to create histograms with distinct colors and labeled x-axis
for (i in 1:6) {
  col <- colnames(aqi_metrics)[i]  # Retrieve column name for the current AQI metric
  hist(aqi_metrics[[i]], main = col, xlab = "Number of Days", col = colors[i], border = "black")
}

####----------------------------------- Lollipop graph of Median.AQI <------------------------------------####
# Calculate median AQI by State
medians_by_state <- aggregate(Median.AQI ~ State, data = df, FUN = median)

# Convert Median.AQI to numeric type for plotting
library(forcats)
medians_by_state$Median.AQI <- as.numeric(medians_by_state$Median.AQI)

# Plot a lollipop graph using ggplot
ggplot(medians_by_state, aes(x = Median.AQI, y = reorder(State, Median.AQI))) +
  geom_segment(aes(x = 0, xend = Median.AQI, y = reorder(State, Median.AQI), yend = reorder(State, Median.AQI)), color = "grey") +
  geom_point(color = "forestgreen", size = 3) +
  labs(title = "Median AQI by State", x = "Median AQI", y = "States") +
  theme_minimal()


#####------------------------------> Choropleth----Median AQI<---------------------------
# Obtain state-level map data
states_map <- get_urbn_map(map = "states", sf = TRUE)

# Rename the 'State' column in the 'df' to match the column name in the map data
names(df)[which(names(df) == "State")] <- "state_name"

# Aggregate Median.AQI data to the state level
state_aqi <- df %>%
  group_by(state_name) %>%
  summarise(Median_AQI = median(Median.AQI, na.rm = TRUE))

# Merge state-level map data with Median.AQI data
state_map_aqi <- left_join(states_map, state_aqi, by = c("state_name" = "state_name"))

# Check for missing values in the Median AQI column
missing_aqi <- sum(is.na(state_map_aqi$Median_AQI))
if (missing_aqi > 0) {
  cat("Warning: There are", missing_aqi, "states with missing Median AQI values.")
}

# Create a choropleth map depicting Median AQI by State
ggplot() +
  geom_sf(data = state_map_aqi, aes(fill = Median_AQI)) +
  scale_fill_gradient(low = "forestgreen", high = "lightyellow", name = "Median AQI") +
  labs(title = "Median Air Quality Index (AQI) by State")



####----------------------------> Box Plots For Hazardous, Good, Unhealthy, and Unhealthy for sensitive Days <---------------------------------------------------------------------

# Box plots for different AQI metrics by Region
par(mfrow = c(2, 2))  # Set up a 2x2 grid for the box plots

# Plot 1: Good Days by Region
plot1 <- ggplot(df, aes(x = Region, y = Good.Days, fill = Region)) +
  geom_boxplot() +
  labs(title = "Good Days by Region") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 0, hjust = 1)
  ) +
  scale_fill_brewer(palette = "Set3") +  
  ylab("Good Days")

# Plot 2: Unhealthy for Sensitive Groups by Region
plot2 <- ggplot(df, aes(x = Region, y = Unhealthy.for.Sensitive.Groups.Days, fill = Region)) +
  geom_boxplot() +
  labs(title = "Unhealthy for Sensitive Groups by Region") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 0, hjust = 1)
  ) +
  scale_fill_brewer(palette = "Set3") +  
  ylab("Unhealthy for Sensitive Groups Days")

# Plot 3: Unhealthy Days by Region
plot3 <- ggplot(df, aes(x = Region, y = Unhealthy.Days, fill = Region)) +
  geom_boxplot() +
  labs(title = "Unhealthy Days by Region") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 0, hjust = 1)
  ) +
  scale_fill_brewer(palette = "Set3") +  
  ylab("Unhealthy Days")

# Plot 4: Hazardous Days by Region
plot4 <- ggplot(df, aes(x = Region, y = Hazardous.Days, fill = Region)) +
  geom_boxplot() +
  labs(title = "Hazardous Days by Region") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 0, hjust = 1)
  ) +
  scale_fill_brewer(palette = "Set3") +  
  ylab("Hazardous Days")

# Combine and arrange the plots into a grid
grid.arrange(grobs = list(plot1, plot2, plot3, plot4), ncol = 2)
#####------------------------------> lollipop_air quality metrics #####<------------------------------------------


# Load necessary libraries
library(ggplot2)
library(reshape2)

# Melt the data for easier plotting
melted_data <- melt(df, id.vars = "Region", measure.vars = c("Good.Days", "Moderate.Days", "Unhealthy.Days"))

# Function to assign colors based on a range
assign_color <- function(value) {
  ifelse(value < 50, "green", ifelse(value > 300, "red", "yellow"))  # Adjust the range values and colors as needed
}

# Filter out 'Other' category from the dataset
melted_data <- melted_data[melted_data$Region != 'Other', ]

# Create a horizontal lollipop plot with colored dots and a legend (transposed)
lollipop_plot_horizontal_transposed <- ggplot(melted_data, aes(x = Region, y = value, group = variable, color = variable)) +
  geom_segment(aes(xend = Region, yend = 0), color = "gray") +
  geom_point(size = 3, shape = 21) +  # Dots at the ends of segments
  scale_color_manual(values = c("Good.Days" = "forestgreen", "Moderate.Days" = "gold1", "Unhealthy.Days" = "darkorange")) +  # Define colors for each variable
  labs(title = "Air Quality Metrics Across Regions",
       x = "Region", y = "Days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()  # Transpose the plot by flipping the coordinates

# Display the transposed horizontal lollipop plot with colored dots and a legend
lollipop_plot_horizontal_transposed


#####------------------------------> Top Counties - Unhealthy days, Very Unhealthy and hazadouse days #####<------------------------------------------


# Filter the data to select the top counties with the highest counts of unhealthy days in various categories
# Sort the 'df' data frame by the count of unhealthy days, very unhealthy days, and hazardous days

# Select the top counties with the highest count of unhealthy days
top_unhealthy <- df[order(-df$Unhealthy.Days), ][1:10, ] 


# Function to generate bar plots for the selected top counties based on each unhealthy day category
create_bar_plot <- function(data, category) {
  fill_color <- switch(category,
                       "Unhealthy.Days" = "brown2",
                       "Very.Unhealthy.Days" = "deeppink4",
                       "Hazardous.Days" = "purple4")
  
  ggplot(data, aes(x = reorder(County, -get(category)), y = get(category))) +
    geom_bar(stat = "identity", fill = fill_color) +
    labs(title = paste("Top Counties by", category), x = "County", y = category) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}

# Create individual bar plots for each category (Unhealthy, Very Unhealthy, and Hazardous Days)
plot_unhealthy <- create_bar_plot(top_unhealthy, "Unhealthy.Days")
plot_very_unhealthy <- create_bar_plot(top_unhealthy, "Very.Unhealthy.Days")
plot_hazardous <- create_bar_plot(top_unhealthy, "Hazardous.Days")

# Arrange the generated bar plots into a single row layout using grid.arrange or patchwork
library(gridExtra)  # Load the gridExtra package

# Display the bar plots side by side
grid.arrange(plot_unhealthy, plot_very_unhealthy, plot_hazardous, nrow = 1)



#####------------------------------> Correlation heat map #####<------------------------------------------
# Load necessary libraries
library(ggplot2)
library(reshape2)

# Assuming you have a dataframe 'df' with columns: Median.AQI, Days.CO, Days.NO2, Days.Ozone, Days.PM2.5, Days.PM10

# Subset the relevant columns
pollutants <- df[c("Median.AQI", "Days.CO", "Days.NO2", "Days.Ozone", "Days.PM2.5", "Days.PM10")]

# Calculate the correlation matrix
correlation_matrix <- cor(pollutants)

# Melt the correlation matrix for plotting
melted_corr <- melt(correlation_matrix)

# Plot heatmap
ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red", name = "Correlation") +
  labs(title = "Correlation Heatmap between Median.AQI and Pollutants",
       x = "Variables", y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
######
