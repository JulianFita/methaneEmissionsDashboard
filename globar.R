# Load packages
library(tidyverse)
library(janitor)
library(highcharter)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(rsconnect)

# Load dataset
methaneEmissions <- read.csv('methane_hist_emissions.csv')

# We take a look
glimpse(methaneEmissions)

# Clean columns names
methaneEmissions.clean <- methaneEmissions %>%
  clean_names()

# Correct format
methaneEmissions.clean$x1990 <- as.double(methaneEmissions.clean$x1990)

# Convert to longer format
methaneEmissions.longer <- methaneEmissions.clean %>%
  pivot_longer(cols = starts_with('x'), names_to = 'year')

# Extract all digits from the 'year' values
methaneEmissions.longer <- methaneEmissions.longer %>%
  select(year, everything()) %>%
  mutate(year = as.integer(str_extract(methaneEmissions.longer$year, "\\d+")))

# Any duplicate or NA?
sum(is.na(methaneEmissions.longer))
anyDuplicated(methaneEmissions.longer)

# Save 'Na' values in a new variable
methaneEmissions.NA <- methaneEmissions.longer %>%
  filter(!complete.cases(.))
