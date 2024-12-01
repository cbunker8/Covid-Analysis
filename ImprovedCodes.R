rm(list = ls())
library(DescTools)
library(tidyverse)
library(plotfunctions)
library(tinytex)
library(lubridate)
library(ggrepel)
library(ggplot2)
library(hexbin)
library(extrafont)
library(dplyr)
library(scales)
library(sf)




master2=read_csv("del1.csv")
master3=read_csv("del3.csv")
master1.5=read_csv("master1.5.csv")
master4 = read_csv("del4.csv")
economic_status = read_csv("ec")
map_data <- st_read("ne_10m_admin_0_countries.shp")
covid_data <- read.csv("owid-covid-data.csv")



absoluteT <- table(master2$GDP_Status, exclude = NA)
propT <- prop.table(absoluteT) * 100

tableFreq <- data.frame(
  GDP_status = names(absoluteT),
  Count = as.vector(absoluteT),
  Percent = as.vector(propT)
)

custom_order <- c("Lowest", "Low", "Average", "High")
tableFreq$GDP_status <- factor(tableFreq$GDP_status, levels = custom_order)

plot1 <- ggplot(data = tableFreq, aes(x = GDP_status, y = Percent)) +
  geom_bar(stat = "identity", width = 0.75, aes(fill = GDP_status)) +
  scale_fill_manual(values = c("Lowest" = "#A0522D", "Low" = "#A0522D", "Average" = "#A0522D", "High" = "#A0522D")) +
  theme_classic() +
  labs(
    title = "The Economic Stratification of Nations Affected by COVID-19",
    subtitle = "Median Household Income By County",
    caption = "Source: ourworldindata.org",
    x = NULL,
    y = NULL
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "Georgia"),
    plot.caption = element_text(hjust = 0, family = "Georgia", size = 10),
    text = element_text(size = 14, family = "Times New Roman"),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 50)) +
  scale_x_discrete(labels = c("Under 10k USD", "10k+ USD", "20k+ USD", "35k+ USD"))

saveRDS(plot1, file = "history_plot.rds")

plot1

#########################


master1.5 <- master1.5 %>%
  mutate(death_category = case_when(
    total_deaths_per_million < 200 ~ "Under 200 Dead",
    total_deaths_per_million >= 3000 ~ "3000+ Dead",
    TRUE ~ "200 - 1999 Dead"
  ))


plot2 <- ggplot(master1.5, aes(x = total_deaths_per_million)) +
  geom_histogram(binwidth = 133, fill = "#A0522D", color = "white") + # History book color scheme
  theme_classic() +
  labs(
    title = "Distribution of Reported COVID-19 Deaths per Million",
    subtitle = "As of April 25, 2022",
    x = "Total Deaths per Million",
    y = "Number of Countries",
    caption = "Source: ourworldindata.org"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "Georgia"),
    plot.caption = element_text(hjust = 0, family = "Georgia", size = 10),
    text = element_text(size = 14, family = "Times New Roman"),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_x_continuous(labels = scales::comma_format())


saveRDS(plot2, file = "histogram_plot.rds")


plot2


################################

library(ggplot2)
library(dplyr)


Age_deaths_filtered <- Age_deaths %>%
  filter(Country != "Japan") %>%
  mutate(
    highlight = ifelse(economic_status == "High", "High GDP (≥ $35k)", "Low/Medium GDP (< $35k)")
  )


final_plot <- ggplot(Age_deaths_filtered, aes(x = median_age, y = total_cases_per_million)) +
  geom_point(aes(color = highlight), size = 5, alpha = 0.8) +
  scale_color_manual(
    values = c("High GDP (≥ $35k)" = "#8B0000", "Low/Medium GDP (< $35k)" = "gray"),
    name = "Economic Status"
  ) +
  labs(
    title = "COVID-19 Cases by Economic Status",
    x = "Median Age of Those Infected",
    y = "Total Cases per Million",
    caption = "Source: ourworldindata.org. High GDP: ≥ $35k Median Household Income\nLow/Medium GDP: < $35k Median Household Income"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"),
    axis.title = element_text(size = 14, family = "Times New Roman"),
    axis.text.x = element_text(size = 12, family = "Times New Roman"),
    axis.text.y = element_blank(), # Remove Y-axis scale
    axis.ticks.y = element_blank(), # Remove Y-axis ticks
    plot.caption = element_text(hjust = 0, size = 10, family = "Times New Roman"),
    legend.position = "top",
    legend.title = element_text(size = 12, family = "Georgia"),
    legend.text = element_text(size = 10, family = "Times New Roman")
  ) +
  scale_x_continuous(labels = scales::comma_format())


saveRDS(plot3, file = "final_plot.rds")

#######################################################



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
    axis.text = element_blank(),       
    axis.ticks = element_blank(),      
    axis.title = element_blank(),      
    axis.line = element_blank(),       
    panel.grid = element_blank(),      
    panel.background = element_blank() 
  )


# Save the map as an RDS file
saveRDS(plot4, file = "map_americas.rds")



