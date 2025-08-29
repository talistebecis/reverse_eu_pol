# Setup -------------------------------------------------------
rm(list=ls())
pacman::p_load(dplyr, readr)

data_full <- read.csv("Data/03 output/03 tables/all_structural_breaks.csv")

gas_files <- c("all_ghg", "co2", "ch4", "n2o", "fgas")
emissions_list <- lapply(gas_files, function(file) {
  # Import each file
  data <- read_csv(paste0("Data/02 intermediary/", file, ".csv"))
  
  # Add the "gas" variable based on the file name
  data <- data %>%
    mutate(gas = file)
  
  return(data)
})

# Combine all emissions datasets into one
emissions_data <- bind_rows(emissions_list)

ipcc_categories <- read_csv("Data/01 Source/IPCC 2006 Categories.csv") %>%
  rename(category = IPCC, IPCC_description = IPCC_description) %>% 
  select(category, IPCC_description)

emissions_data <- emissions_data %>%
  left_join(ipcc_categories, by = "category")

# Load the GWP dataset
gwp_data <- read_csv("Data/01 Source/GWP.csv") %>%
  rename(gas = Gas)  # Match the column name 'gas' in final_data


# Analysis ----------------------------------------------------------------

AUT_data <- data_full %>% 
  filter(country == "Austria",
         coef < 0) %>% 
  group_by(sector.description, gas, year) %>% 
  summarise(mean_coef = mean(coef))

final_data <- AUT_data %>%
  left_join(
    emissions_data %>%
      filter(country == "Austria") %>%
      select(IPCC_description, year, gas, emissions),
    by = c("sector.description" = "IPCC_description", "year", "gas")
  )

# Join GWP data with the final_data dataset and calculate emissions in GWP
final_data_with_gwp <- final_data %>%
  left_join(gwp_data, by = "gas") %>%
  mutate(emissions_gwp = emissions * GWP,
         emissions_reduction_tonnes = emissions_gwp * mean_coef * 1000)

write_csv(final_data_with_gwp, "Data/03 output/03 tables/AUT_data_with_emissions_gwp.csv")


