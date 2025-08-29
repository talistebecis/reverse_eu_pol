# TALIS OLD

# prep --------------------------------------------------------------------

rm(list=ls())

# load libraries
pacman::p_load(data.table, dplyr, tidyr, readxl, stringr, gets, here, ggplot2, devtools)


# create dataset ----------------------------------------------------------

#### Countries - EU + OECD (43 total countries)
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

################################################################################
# ALL GHG EMISSIONS
################################################################################

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


data <- left_join(gdp, pop, c("country", "year")) %>%
  left_join(emissions_total, c("country", "year"))

#### Transform Variables
data$lgdp <- log(data$gdp)
data$lpop <- log(data$pop)
data$lemissions <- log(data$emissions)
data$const <- 1
data$lgdp_sq <- data$lgdp^2
data$emissions_pc <- data$emissions/data$pop
data$lemissions_pc <- log(data$emissions_pc)

#### Output
write.csv(data, here("Data/02 intermediary/all_ghg.csv"), row.names = F)


################################################################################
# CO2 EMISSIONS
################################################################################

CO2_total <- read_excel("Data/01 source/IEA_EDGAR_CO2_1970_2022.xlsx",
                              range = "IPCC 2006!A10:BI3516") %>% 
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
CO2_total[,"emissions"][CO2_total[,"emissions"] <= 0] <- 0.000001

data <- left_join(gdp, pop, c("country", "year")) %>%
  left_join(CO2_total, c("country", "year"))

#### Transform Variables
data$lgdp <- log(data$gdp)
data$lpop <- log(data$pop)
data$lemissions <- log(data$emissions)
data$const <- 1
data$lgdp_sq <- data$lgdp^2
data$emissions_pc <- data$emissions/data$pop
data$lemissions_pc <- log(data$emissions_pc)

#### Output
write.csv(data, here("Data/02 intermediary/co2.csv"), row.names = F)


################################################################################
# CH4 EMISSIONS
################################################################################

CH4_total <- read_excel("Data/01 source/EDGAR_CH4_1970_2022.xlsx",
                        range = "IPCC 2006!A10:BI4751") %>% 
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
CH4_total[,"emissions"][CH4_total[,"emissions"] <= 0] <- 0.000001


data <- left_join(gdp, pop, c("country", "year")) %>%
  left_join(CH4_total, c("country", "year"))

#### Transform Variables
data$lgdp <- log(data$gdp)
data$lpop <- log(data$pop)
data$lemissions <- log(data$emissions)
data$const <- 1
data$lgdp_sq <- data$lgdp^2
data$emissions_pc <- data$emissions/data$pop
data$lemissions_pc <- log(data$emissions_pc)

#### Output
write.csv(data, here("Data/02 intermediary/ch4.csv"), row.names = F)


################################################################################
# N2O EMISSIONS
################################################################################

N2O_total <- read_excel("Data/01 source/EDGAR_N2O_1970_2022.xlsx",
                        range = "IPCC 2006!A10:BI4822") %>% 
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
N2O_total[,"emissions"][N2O_total[,"emissions"] <= 0] <- 0.000001

data <- left_join(gdp, pop, c("country", "year")) %>%
  left_join(N2O_total, c("country", "year"))

#### Transform Variables
data$lgdp <- log(data$gdp)
data$lpop <- log(data$pop)
data$lemissions <- log(data$emissions)
data$const <- 1
data$lgdp_sq <- data$lgdp^2
data$emissions_pc <- data$emissions/data$pop
data$lemissions_pc <- log(data$emissions_pc)

#### Output
write.csv(data, here("Data/02 intermediary/n2o.csv"), row.names = F)


################################################################################
# F-gases EMISSIONS
################################################################################

Fgas_total <- read_excel("Data/01 source/EDGAR_F-gases_1990_2022.xlsx",
                         range = "IPCC 2006!A10:AO1738") %>% 
  mutate(Name = ifelse(Name=="Slovakia", "Slovak Republic",
                       ifelse(Name=="Czech Republic","Czechia",
                              ifelse(Name=="Korea, Republic of","Korea, Rep.",
                                     ifelse(Name=="Turkey","Turkiye",Name))))) %>%
  filter(Name %in% OECD_EU) %>% 
  pivot_longer(9:41, names_to = "year") %>%
  transmute(country=Name,
            year=as.numeric(str_remove(year, "Y_")),
            emissions=as.numeric(value),
            category=ipcc_code_2006_for_standard_report) %>% 
  group_by(country, category, year) %>%
  summarise(emissions = sum(emissions, na.rm = TRUE))

#remove zero and negative values in emissions to avoid log(0) problem
Fgas_total[,"emissions"][Fgas_total[,"emissions"] <= 0] <- 0.000001

data <- left_join(gdp, pop, c("country", "year")) %>%
  left_join(Fgas_total, c("country", "year"))

#### Transform Variables
data$lgdp <- log(data$gdp)
data$lpop <- log(data$pop)
data$lemissions <- log(data$emissions)
data$const <- 1
data$lgdp_sq <- data$lgdp^2
data$emissions_pc <- data$emissions/data$pop
data$lemissions_pc <- log(data$emissions_pc)

#### Output
write.csv(data, here("Data/02 intermediary/fgas.csv"), row.names = F)
