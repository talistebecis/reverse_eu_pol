# Packages ----------------------------------------------------------------
rm(list=ls())
pacman::p_load(dplyr, here, readxl)

# Import output tables
file_path <- here("data", "03 output", "03 tables", "final_tables.xlsx")
eu_table <- read_excel(file_path, sheet = "eu")
aut_table <- read_excel(file_path, sheet = "austria")

# Import Emissions codes
emissions_codes <- read.csv("data/01 source/IPCC 2006 Categories.csv") %>%
  select(IPCC, IPCC_description)

#Group specification
EU15 <- c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland",
          "France", "United Kingdom", "Ireland", "Italy", "Luxembourg", 
          "Netherlands", "Greece", "Portugal", "Sweden")

# Import All GHG database
gas <- "all_ghg"
data_full <- read.csv(paste0("Data/02 intermediary/",gas,".csv"))


# EU Table ----------------------------------------------------------------

# Group for all EU15 countries
data_EU15 <- data_full %>% 
  filter(country %in% EU15) %>% 
  group_by(category, year) %>% 
  summarise(total_emissions = sum(emissions)) %>% 
  left_join(emissions_codes, by = c("category" = "IPCC")) %>% 
  rename(sector = IPCC_description)

# Combine tables
eu_table_new <- eu_table %>%
  left_join(data_EU15, by = c("year", "sector")) %>%
  rename(absolute_emissions = total_emissions) %>% 
  mutate(absolute_emissions = absolute_emissions/1000,
         reduction = coefficient * absolute_emissions)

# Austria Table ----------------------------------------------------------

# Group for all EU15 countries
data_AUT <- data_full %>% 
  filter(country == "Austria") %>% 
  group_by(category, year) %>% 
  summarise(total_emissions = sum(emissions)) %>% 
  left_join(emissions_codes, by = c("category" = "IPCC")) %>% 
  rename(sector = IPCC_description)

# Combine tables
aut_table_new <- aut_table %>%
  left_join(data_EU15, by = c("year", "sector")) %>%
  rename(absolute_emissions = total_emissions) %>% 
  mutate(absolute_emissions = absolute_emissions/1000,
         reduction = coefficient * absolute_emissions)
