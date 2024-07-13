# Loading necessary libraries
library(tidyverse)

# Reading and loading the data files
country_data <- read.csv("country_data.csv", stringsAsFactors = FALSE)
country_metadata <- read.csv("country_metadata.csv", stringsAsFactors = FALSE)

# Data cleaning and transformation

## Checking for null values in country_data
summary(country_data)
sum(is.na(country_data))

## Checking for null values in country_metadata
summary(country_metadata)
sum(is.na(country_metadata))

## Imputing the missing numeric values with mean
numeric_columns <- sapply(country_data, is.numeric)
country_data[numeric_columns] <- lapply(country_data[numeric_columns], function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
})

## Imputing the missing categorical values with mode
categorical_columns <- c("iso_code", "continent", "location", "date", "tests_units")
for (col in categorical_columns) {
  mode_value <- names(sort(table(country_data[[col]]), decreasing = TRUE))[1]
  country_data[[col]][is.na(country_data[[col]])] <- mode_value
}

## Checking again for missing values
print(summary(country_data))
sum(is.na(country_data))

## Handling missing values in the country metadata file

### Imputing the missing numeric values with mean
numeric_columns_meta <- sapply(country_metadata, is.numeric)
country_metadata[numeric_columns_meta] <- lapply(country_metadata[numeric_columns_meta], function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
})

### Imputing the missing categorical values with mode (for 'location')
country_metadata$location[is.na(country_metadata$location)] <- 
  names(sort(table(country_metadata$location), decreasing = TRUE))[1]

### Checking again for missing values
print(summary(country_metadata))
sum(is.na(country_metadata))

## Data selection

### Selection of relevant columns
country_data_selected <- country_data %>%
  select(iso_code, continent, location, date,
         total_cases, new_cases, total_deaths, new_deaths,
         total_cases_per_million, total_deaths_per_million)

country_metadata_selected <- country_metadata %>%
  select(location, population_density, median_age, aged_65_older, gdp_per_capita,
         life_expectancy, human_development_index)

### summarise 

summary_stats <- country_data_selected %>%
  group_by(location) %>%
  summarize(total_cases = max(total_cases, na.rm = TRUE),
            total_deaths = max(total_deaths, na.rm = TRUE))
print(summary_stats)

### Filter data for Ireland and 9 other countries
countries_of_interest <- c("Ireland", "Cuba", "Latvia", "Malawi", "Montserrat",
                           "Namibia", "Pitcairn", "Timor", "Togo", "United States")

# Visualisations

library(dplyr)
library(ggplot2)

## World map of Total Covid 19 cases

### Merge datasets
world_map_data <- country_data_selected %>%
  inner_join(country_metadata_selected, by = "location")

# Coordinates
coordinates <- data.frame(
  location = c("Ireland", "Cuba", "Latvia", "Malawi", "Montserrat",
               "Namibia", "Pitcairn", "Timor", "Togo", "United States"),
  long = c(-8.243890, -77.781166, 24.603189, 34.301525, -62.187366, 17.3231107, -130.101782, 124.6370774, 1.0199765, -100.445882),
  lat = c(53.412910, 21.521757, 56.879635, -13.254308, 16.742498, -23.2335499, -25.0657719, -9.3460171, 8.7800265, 39.7837304)
)

### Filter and merge coordinates
world_map_data <- world_map_data %>%
  filter(location %in% coordinates$location) %>%
  inner_join(coordinates, by = "location")

### Get world map coordinates
world_coordinates <- map_data("world")

### Create the world map plot
world_map_plot <- ggplot() + 
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),
    color = "green", fill = "lightyellow"
  ) + 
  geom_point(
    data = world_map_data,
    aes(x = long, y = lat, color = total_cases, size = population_density),
    alpha = 0.7
  ) +
  geom_text(
    data = world_map_data,
    aes(x = long, y = lat, label = location),
    size = 3, vjust = -1
  ) +
  scale_color_viridis_c(name = "Total Cases", option = "viridis") +
  scale_size_continuous(name = "Population Density") +
  labs(title = "COVID-19 Total Cases by Country",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

### Display the plot
print(world_map_plot)

## Bar Chart showing total cases across countries

### Aggregating data to get the total cases for each location
bar_data <- aggregate(new_cases ~ location, data = country_data_selected, sum)

### Converting total cases to millions for better readability
bar_data$total_cases_millions <- bar_data$new_cases / 1e6

### Sorting the data by total cases
country_data_sorted <- bar_data[order(bar_data$total_cases_millions, decreasing = TRUE), ]

### Function to format labels in millions
label_millions <- function(x) {
  paste0(format(x, big.mark = ","), "M")
}

### Plot bar chart with values displayed on the bars in millions
bar_chart <- ggplot(country_data_sorted, aes(x = reorder(location, total_cases_millions), y = total_cases_millions)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
  coord_flip() +
  geom_text(aes(label = label_millions(total_cases_millions)), hjust = -0.1, size = 2.0) +
  labs(title = "Total COVID-19 Cases across Countries",
       subtitle = "Ireland and 9 Other Countries",
       x = "Country", y = "Total Cases (Millions)") +
  scale_y_continuous(labels = label_millions) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12))

print(bar_chart)

## Scatter plot with linear regression line for COVID-19 Total Deaths vs Total Cases
scatterplot <- ggplot(country_data_selected, aes(x = total_cases, y = total_deaths, color = location)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "COVID-19 Total Deaths vs Total Cases",
       subtitle = "Ireland and 9 Other Countries",
       x = "Total Cases", y = "Total Deaths",
       color = "Country")

print(scatterplot)

## Time Series plot for new cases across countries over the periods (2020-2022)

### Convert date to Date format
country_data_selected$date <- as.Date(country_data_selected$date)

### Plot time-series chart for new cases
time_series_plot <- ggplot(country_data_selected, aes(x = date, y = new_cases, color = location)) +
  geom_line() +
  labs(title = "COVID-19 New Cases Over Time",
       subtitle = "Ireland and 9 Other Countries",
       x = "Date", y = "New Cases",
       color = "Country") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 month")

print(time_series_plot)
