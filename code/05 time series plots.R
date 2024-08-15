# Packages ----------------------------------------------------------------
rm(list=ls())
pacman::p_load(data.table, dplyr, tidyr, readxl, stringr, gets, here, ggplot2, devtools, ggpubr, tidyverse)

#Group specification
EU15 <- c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland",
          "France", "United Kingdom", "Ireland", "Italy", "Luxembourg", 
          "Netherlands", "Greece", "Portugal", "Sweden")

#Emissions codes
emissions_codes <- read.csv("data/01 source/IPCC 2006 Categories.csv") %>%
  select(IPCC, IPCC_description)

#GWPs
GWPs <- read.csv("data/01 source/GWP.csv")

################################################################################
# ALL GHG EMISSIONS
################################################################################

GHG_total <- read_excel("Data/01 source/EDGAR_AR5_GHG_1970_2022.xlsx",
                              range = "IPCC 2006!A10:BI7140") %>% 
  mutate(Name = ifelse(Name=="Slovakia", "Slovak Republic",
                       ifelse(Name=="Czech Republic","Czechia",
                              ifelse(Name=="Korea, Republic of","Korea, Rep.",
                                     ifelse(Name=="Turkey","Turkiye",Name))))) %>%
  filter(Name %in% EU15) %>% 
  pivot_longer(9:61, names_to = "year") %>%
  transmute(country=Name,
            year=as.numeric(str_remove(year, "Y_")),
            emissions=as.numeric(value),
            category=ipcc_code_2006_for_standard_report,
            substance=Substance) %>% 
  group_by(country, category, substance, year) %>%
  summarise(emissions = sum(emissions, na.rm = TRUE))

################################################################################
# CO2 EMISSIONS
################################################################################

CO2_total <- read_excel("Data/01 source/IEA_EDGAR_CO2_1970_2022.xlsx",
                        range = "IPCC 2006!A10:BI3516") %>% 
  mutate(Name = ifelse(Name=="Slovakia", "Slovak Republic",
                       ifelse(Name=="Czech Republic","Czechia",
                              ifelse(Name=="Korea, Republic of","Korea, Rep.",
                                     ifelse(Name=="Turkey","Turkiye",Name))))) %>%
  filter(Name %in% EU15) %>% 
  pivot_longer(9:61, names_to = "year") %>%
  transmute(country=Name,
            year=as.numeric(str_remove(year, "Y_")),
            emissions=as.numeric(value),
            category=ipcc_code_2006_for_standard_report,
            substance=Substance) %>% 
  group_by(country, category, substance, year) %>%
  summarise(emissions = sum(emissions, na.rm = TRUE))

################################################################################
# CH4 EMISSIONS
################################################################################

CH4_total <- read_excel("Data/01 source/EDGAR_CH4_1970_2022.xlsx",
                        range = "IPCC 2006!A10:BI4751") %>% 
  mutate(Name = ifelse(Name=="Slovakia", "Slovak Republic",
                       ifelse(Name=="Czech Republic","Czechia",
                              ifelse(Name=="Korea, Republic of","Korea, Rep.",
                                     ifelse(Name=="Turkey","Turkiye",Name))))) %>%
  filter(Name %in% EU15) %>% 
  pivot_longer(9:61, names_to = "year") %>%
  transmute(country=Name,
            year=as.numeric(str_remove(year, "Y_")),
            emissions=as.numeric(value),
            category=ipcc_code_2006_for_standard_report,
            substance=Substance) %>% 
  group_by(country, category, substance, year) %>%
  summarise(emissions = sum(emissions, na.rm = TRUE))

################################################################################
# N2O EMISSIONS
################################################################################

N2O_total <- read_excel("Data/01 source/EDGAR_N2O_1970_2022.xlsx",
                        range = "IPCC 2006!A10:BI4822") %>% 
  mutate(Name = ifelse(Name=="Slovakia", "Slovak Republic",
                       ifelse(Name=="Czech Republic","Czechia",
                              ifelse(Name=="Korea, Republic of","Korea, Rep.",
                                     ifelse(Name=="Turkey","Turkiye",Name))))) %>%
  filter(Name %in% EU15) %>% 
  pivot_longer(9:61, names_to = "year") %>%
  transmute(country=Name,
            year=as.numeric(str_remove(year, "Y_")),
            emissions=as.numeric(value),
            category=ipcc_code_2006_for_standard_report,
            substance=Substance) %>% 
  group_by(country, category, substance, year) %>%
  summarise(emissions = sum(emissions, na.rm = TRUE))

################################################################################
# F-gases EMISSIONS
################################################################################

Fgas_total <- read_excel("Data/01 source/EDGAR_F-gases_1990_2022.xlsx",
                        range = "IPCC 2006!A10:AO1738") %>% 
  mutate(Name = ifelse(Name=="Slovakia", "Slovak Republic",
                       ifelse(Name=="Czech Republic","Czechia",
                              ifelse(Name=="Korea, Republic of","Korea, Rep.",
                                     ifelse(Name=="Turkey","Turkiye",Name))))) %>%
  filter(Name %in% EU15) %>% 
  pivot_longer(9:41, names_to = "year") %>%
  transmute(country=Name,
            year=as.numeric(str_remove(year, "Y_")),
            emissions=as.numeric(value),
            category=ipcc_code_2006_for_standard_report,
            substance=Substance) %>% 
  group_by(country, category, substance, year) %>%
  summarise(emissions = sum(emissions, na.rm = TRUE))


################################################################################
# COMBINE
################################################################################

data_total <- rbind(GHG_total, CO2_total, CH4_total, N2O_total, Fgas_total) %>% 
  left_join(GWPs, by = c("substance" = "Gas")) %>% 
  mutate(emissions_CO2e = emissions * GWP)


################################################################################
# PLOT - EU15
################################################################################

F_gases <- c("c-C4F8","C2F6","C3F8","C4F10","CF4","HCFC-141b","HCFC-142b",
             "HFC-125","HFC-134a","HFC-143a","HFC-152a","HFC-227ea","HFC-23",
             "HFC-245fa","HFC-32","HFC-365mfc","HFC-43-10-mee","NF3","SF6")

# select data
data <- data_total %>%
  left_join(emissions_codes,
            by = c("category" = "IPCC")) %>%
  filter(!is.na(emissions_CO2e),
         year >= 1995,
         country %in% EU15) %>%
  mutate(substance = if_else(substance %in% F_gases, "F-gases", substance),
         substance = if_else(substance == "GWP_100_AR5_GHG", "All GHGs", substance),
         emissions_CO2e = emissions_CO2e/1000) %>% 
  group_by(substance, IPCC_description, year) %>%
  summarise(emissions_CO2e = sum(emissions_CO2e)) %>% 
  rename(Gas = substance)

#plot
ggplot(data = subset(data)) +
  geom_line(mapping = aes(x = year, y = emissions_CO2e, color = Gas)) +
  facet_wrap(~ IPCC_description, scale = "free") +
  labs(x = "",
       y = "Emissions (Mt carbon dioxide equivalents)") +
  theme(axis.text = element_text(size = 6),
        strip.text = element_text(size = 7),
        legend.position = "bottom") +
  labs(color = NULL)


################################################################################
# PLOT - Austria
################################################################################

F_gases <- c("c-C4F8","C2F6","C3F8","C4F10","CF4","HCFC-141b","HCFC-142b",
             "HFC-125","HFC-134a","HFC-143a","HFC-152a","HFC-227ea","HFC-23",
             "HFC-245fa","HFC-32","HFC-365mfc","HFC-43-10-mee","NF3","SF6")

# select data
data <- data_total %>%
  left_join(emissions_codes,
            by = c("category" = "IPCC")) %>%
  filter(!is.na(emissions_CO2e),
         year >= 1995,
         country == "Austria") %>%
  mutate(substance = if_else(substance %in% F_gases, "F-gases", substance),
         substance = if_else(substance == "GWP_100_AR5_GHG", "All GHGs", substance)) %>% 
  group_by(substance, IPCC_description, year) %>%
  summarise(emissions_CO2e = sum(emissions_CO2e)) %>% 
  rename(Gas = substance)

#plot
ggplot(data = subset(data)) +
  geom_line(mapping = aes(x = year, y = emissions_CO2e, color = Gas)) +
  facet_wrap(~ IPCC_description, scale = "free") +
  labs(x = "",
       y = "Emissions (kt carbon dioxide equivalents)") +
  theme(axis.text = element_text(size = 6),
        strip.text = element_text(size = 7),
        legend.position = "bottom") +
  labs(color = NULL)

