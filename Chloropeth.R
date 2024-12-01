library(sf)

map_data <- st_read("ne_10m_admin_0_countries.shp")


covid_data <- read.csv("owid-covid-data.csv")

covid_data <- covid_data %>%
  mutate(Country = recode(
    location,
    "United States" = "United States of America",
    "Bahamas" = "The Bahamas",
    "Antigua and Barbuda" = "Antigua and Barb.",
    "Curaçao" = "Curacao",
    "St. Kitts and Nevis" = "Saint Kitts and Nevis",
    "St. Vincent and the Grenadines" = "Saint Vincent and the Grenadines",
    "Micronesia (country)" = "Micronesia"
  ))


covid_data <- covid_data %>%
  filter(date == "2022-04-25") %>%  # Filter for relevant date
  select(Country, total_deaths_per_million, population)  # Relevant columns


map_data_filtered <- map_data %>%
  filter(CONTINENT %in% c("North America", "South America"))


merged_data <- merge(
  map_data_filtered,
  covid_data,
  by.x = "NAME",  
  by.y = "Country",
  all.x = TRUE
)

plot4 <- ggplot(data = merged_data) +
  geom_sf(aes(fill = total_deaths_per_million), color = "black", size = 0.1) +
  scale_fill_gradient(
    low = "#FDE725", high = "#440154",  # Light yellow to dark purple
    name = "Deaths Per Million"
  ) +
  coord_sf(xlim = c(-170, -30), ylim = c(-60, 90)) +  # Full Americas view
  labs(
    title = "COVID-19 Deaths Across the Americas",
    caption = "Source: ourworldindata.org. Data as of April 25th, 2022"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 13, family = "Times New Roman"),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"),
    plot.caption = element_text(hjust = 0, size = 10, family = "Times New Roman"),
    axis.text = element_blank(),       # Remove axis text
    axis.ticks = element_blank(),      # Remove axis ticks
    axis.title = element_blank(),      # Remove axis titles
    axis.line = element_blank(),       # Remove axis lines
    panel.grid = element_blank(),      # Remove panel grid
    panel.background = element_blank() # Keep the background blank
  )


map_americas


