# prep --------------------------------------------------------------------

rm(list=ls())

# load libraries
pacman::p_load(data.table, dplyr, tidyr, readxl, stringr, gets, here, ggplot2, devtools)
# devtools::install_github("moritzpschwarz/getspanel")
library(getspanel)


# create dataset ----------------------------------------------------------

#### Countries
OECD_EU <- c("Austria", "Belgium", "Czechia", "Germany", "Denmark", "Spain",
          "Estonia", "Finland", "France", "United Kingdom", "Greece",
          "Hungary", "Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia",
          "Netherlands", "Poland", "Portugal", "Slovak Republic", "Slovenia",
          "Sweden", "Switzerland", "Iceland", "Norway", "Australia", "Canada",
          "Chile", "Colombia", "Costa Rica", "Israel", "Japan",
          "Korea, Rep.", "Mexico", "New Zealand", "Turkiye",
          "United States", "Bulgaria", "Croatia", "Cyprus", "Malta", "Romania")

#### Import and merge Data
gdp <- read.csv("Data/01 source/WB_gdpconst_2023.csv", skip = 3) %>%
  filter(Country.Name %in% OECD_EU) %>%
  select(c(1,14:67)) %>%
  pivot_longer(2:55, names_to = "year") %>%
  transmute(country=Country.Name, year=as.numeric(str_remove(year, "X")),
            gdp=value)

pop <- read.csv("Data/01 source/WB_totpop_2023.csv", skip=3) %>%
  filter(Country.Name %in% OECD_EU) %>%
  select(c(1,14:67)) %>%
  pivot_longer(2:55, names_to = "year") %>%
  transmute(country=Country.Name, year=as.numeric(str_remove(year, "X")),
            pop=value)

emissions_total <- read_excel("Data/01 source/EDGAR_AR5_GHG_1970_2022.xlsx",
                              range = "IPCC 2006!A10:BI7140") %>% 
  mutate(Name = ifelse(Name=="Slovakia", "Slovak Republic",
                       ifelse(Name=="Czech Republic","Czechia",
                              ifelse(Name=="Korea, Republic of","Korea, Rep.",
                                     ifelse(Name=="Turkey","Turkiye",Name))))) %>%
  filter(Name %in% OECD_EU) %>% 
  pivot_longer(9:61, names_to = "year") %>%
  transmute(country=Name,
            year=as.numeric(str_remove(year, "Y_")),
            emissions=as.numeric(value),
            category=ipcc_code_2006_for_standard_report) %>% 
  group_by(country, category, year) %>%
  summarise(emissions = sum(emissions, na.rm = TRUE))

#remove zero and negative values in emissions to avoid log(0) problem
emissions_total[,"emissions"][emissions_total[,"emissions"] <= 0] <- 0.000001


data_temp <- left_join(gdp, pop, c("country", "year")) %>%
  left_join(emissions_total, c("country", "year"))


#### create EU15 and EU27 data points

EU15 <- c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland",
          "France", "United Kingdom", "Ireland", "Italy", "Luxembourg", 
          "Netherlands", "Greece", "Portugal", "Sweden")
EU16 <- c("Croatia", "Bulgaria", "Cyprus", "Czechia", "Estonia", 
          "Hungary", "Lithuania", "Latvia", "Malta", "Poland", "Romania", 
          "Slovak Republic", "Slovenia", "Switzerland", "Iceland", 
          "Norway")
EU31 <- c(EU15, EU16)

EU15_total <- data_temp %>% 
  filter(country %in% EU15) %>% 
  group_by(category, year) %>% 
  summarise(pop = sum(pop),
            gdp = sum(gdp),
            emissions = sum(emissions)) %>% 
  mutate(country = "EU15_total") %>% 
  relocate(country, year, gdp, pop, category, emissions)

EU31_total <- data_temp %>% 
  filter(country %in% EU31) %>% 
  group_by(category, year) %>% 
  summarise(pop = sum(pop),
            gdp = sum(gdp),
            emissions = sum(emissions)) %>% 
  mutate(country = "EU28_total") %>% 
  relocate(country, year, gdp, pop, category, emissions)

data <- data_temp %>%
  bind_rows(EU15_total, EU31_total)


#### Transform Variables
data$lgdp <- log(data$gdp)
data$lpop <- log(data$pop)
data$lemissions <- log(data$emissions)
data$const <- 1
data$lgdp_sq <- data$lgdp^2
data$emissions_pc <- data$emissions/data$pop
data$lemissions_pc <- log(data$emissions_pc)


#### Output
write.csv(data, here("Data/02 intermediary/data_level3.csv"), row.names = F)


############## need to add EU15 and EU27 countries here

# # Level 2 sectors ---------------------------------------------------------
# 
# category_codes <- read.csv("Data/01 source/IPCC 2006 Categories.csv")
# 
# emissions_level2 <- emissions_total %>% 
#   left_join(category_codes, c("category" = "IPCC")) %>% 
#   group_by(country, year, Level.2) %>% 
#   summarise(emissions = sum(emissions)) %>% 
#   rename(category_name = Level.2)
# 
# data_level2 <- left_join(gdp, pop, c("country", "year")) %>%
#   left_join(emissions_level2, c("country", "year"))
# 
# #### Transform Variables
# data_level2$lgdp <- log(data_level2$gdp)
# data_level2$lpop <- log(data_level2$pop)
# data_level2$lemissions <- log(data_level2$emissions)
# data_level2$const <- 1
# data_level2$lgdp_sq <- data_level2$lgdp^2
# data_level2$emissions_pc <- data_level2$emissions/data_level2$pop
# data_level2$lemissions_pc <- log(data_level2$emissions_pc)
# 
# #### Output
# write.csv(data_level2, here("Data/02 intermediary/data_level2.csv"), row.names = F)
# 
# 
# # Level 1 sectors ------------------------------------------------------
# 
# category_codes <- read.csv("Data/01 source/IPCC 2006 Categories.csv")
# 
# emissions_level1 <- emissions_total %>% 
#   left_join(category_codes, c("category" = "IPCC")) %>% 
#   group_by(country, year, Level.1) %>% 
#   summarise(emissions = sum(emissions)) %>% 
#   rename(category_name = Level.1)
# 
# data_level1 <- left_join(gdp, pop, c("country", "year")) %>%
#   left_join(emissions_level1, c("country", "year"))
# 
# #### Transform Variables
# data_level1$lgdp <- log(data_level1$gdp)
# data_level1$lpop <- log(data_level1$pop)
# data_level1$lemissions <- log(data_level1$emissions)
# data_level1$const <- 1
# data_level1$lgdp_sq <- data_level1$lgdp^2
# data_level1$emissions_pc <- data_level1$emissions/data_level1$pop
# data_level1$lemissions_pc <- log(data_level1$emissions_pc)
# 
# #### Output
# write.csv(data_level1, here("Data/02 intermediary/data_level1.csv"), row.names = F)

