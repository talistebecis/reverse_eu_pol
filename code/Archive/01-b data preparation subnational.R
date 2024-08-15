# prep --------------------------------------------------------------------

rm(list=ls())

# load libraries
pacman::p_load(data.table, dplyr, tidyr, readxl, stringr, gets, here, ggplot2, devtools, eurostat)

# create dataset ----------------------------------------------------------

#### Import and merge Data

id <- "nama_10r_2gdp"
gdp <- get_eurostat(id, time_format = "num", stringsAsFactors = TRUE) %>% 
  filter(unit == "MIO_EUR",
         str_length(geo) == 4) %>% 
  transmute(country=geo,
            year=TIME_PERIOD,
            gdp=values)

id <- "demo_r_d2jan"
pop <- get_eurostat(id,
                    filters = list(sex = "T", age = "TOTAL"),
                    time_format = "num",
                    stringsAsFactors = TRUE) %>% 
  filter(str_length(geo) == 4,
         time > 1999,
         !geo == "EU28") %>% 
  transmute(country=geo,
            year=time,
            pop=values)


################################################################################
# ALL GHG EMISSIONS
################################################################################

EU15_codes <- "AT|BE|DE|DK|ES|FI|FR|IE|IT|LU|NL|EL|PT|SE"

emissions_total <- read_excel("Data/01 source/subnat/EDGARv8.0_total_GHG_GWP100_AR5_NUTS2_1990_2022.xlsx",
                              range = "GHG by NUTS2 and Sector!A6:AM1545") %>%
  pivot_longer(7:39, names_to = "year") %>%
  transmute(country = NUTS_2,
            year=as.numeric(str_remove(year, "Y_")),
            emissions=as.numeric(value),
            category=Sector) %>% 
  group_by(country, category, year) %>%
  summarise(emissions = sum(emissions, na.rm = TRUE)) %>% 
  filter(str_length(country) == 4,
         grepl(EU15_codes, country),
         year > 1999,
         year < 2022)

#remove zero and negative values in emissions to avoid log(0) problem
emissions_total[,"emissions"][emissions_total[,"emissions"] <= 0] <- 0.000001

data <- left_join(emissions_total, pop, c("country", "year")) %>%
  left_join(gdp, c("country", "year")) %>% 
  na.omit()

#### Transform Variables
data$lgdp <- log(data$gdp)
data$lpop <- log(data$pop)
data$lemissions <- log(data$emissions)
data$const <- 1
data$lgdp_sq <- data$lgdp^2
data$emissions_pc <- data$emissions/data$pop
data$lemissions_pc <- log(data$emissions_pc)

#### Output
write.csv(data, here("Data/02 intermediary/all_subnat.csv"), row.names = F)
