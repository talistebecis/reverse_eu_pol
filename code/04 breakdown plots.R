# Packages ----------------------------------------------------------------
rm(list=ls())
pacman::p_load(dplyr, data.table, ggplot2, ggpubr, here, tidyverse)


# Setup -------------------------------------------------------
set.seed(1230)

#Group specification
EU15 <- c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland",
          "France", "United Kingdom", "Ireland", "Italy", "Luxembourg", 
          "Netherlands", "Greece", "Portugal", "Sweden")

OECD_non_EU15 <- c("Czechia","Estonia", "Hungary", "Lithuania", "Latvia",
                   "Poland", "Slovak Republic", "Slovenia", "Switzerland", 
                   "Iceland", "Norway", "Australia", "Canada","Chile",
                   "Colombia", "Costa Rica", "Israel", "Japan",
                   "Korea, Rep.", "Mexico", "New Zealand", "Turkiye",
                   "United States")

OECD <- c(EU15, OECD_non_EU15)

OECD_PC <- c(EU15, OECD_non_EU15, "EU15_total")


#Emissions codes
emissions_codes <- read.csv("data/01 source/IPCC 2006 Categories.csv") %>%
  select(IPCC, IPCC_description)

# Choose gas
gas <- "all_ghg"
data_full <- read.csv(paste0("Data/02 intermediary/",gas,".csv"))


################################################################################
# BREAKDOWN OF EMISSIONS
################################################################################

# Plot --------------------------------------------------------------------

data_AUT <- data_full %>% 
  filter(country == "Austria",
         year == 2022) %>% 
  group_by(category) %>% 
  summarise(total_emissions = sum(emissions)) %>% 
  left_join(emissions_codes, by = c("category" = "IPCC")) %>% 
  arrange(total_emissions) %>% 
  mutate(percent = (total_emissions/sum(total_emissions))*100,
         total_emissions = total_emissions/1000)

data %>% 
  mutate(IPCC_description=factor(IPCC_description, IPCC_description)) %>%
  ggplot(aes(x=IPCC_description, y=total_emissions)) +
  geom_segment(aes(x=IPCC_description ,xend=IPCC_description, y=0, yend=total_emissions), color="grey") +
  geom_point(size=2, color="black") +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none") +
  xlab("") +
  ylab("Total emissions (Mt CO2-equivalents)")


data_EU15 <- data_full %>% 
  filter(country %in% EU15,
         year == 2022) %>% 
  group_by(category) %>% 
  summarise(total_emissions = sum(emissions)) %>% 
  left_join(emissions_codes, by = c("category" = "IPCC")) %>% 
  arrange(total_emissions) %>% 
  mutate(percent = (total_emissions/sum(total_emissions))*100,
         total_emissions = total_emissions/1000)

################################################################################
# Time series
################################################################################
# 
# data <- data_full %>%
#   left_join(emissions_codes,
#             by = c("category" = "IPCC")) %>%
#   filter(!is.na(lemissions),
#          year >= 1995,
#          country == "Austria")
# 
# ggplot(data = subset(data, country == "Austria")) +
#   geom_line(mapping = aes(x = year, y = emissions)) +
#   facet_wrap(~ IPCC_description, scale = "free") +
#   labs(x = "Year",
#        y = "Emissions (kt carbon dioxide)") +
#   theme(axis.text = element_text(size = 6),
#         strip.text = element_text(size = 7))
# 
# 
# ################################################################################
# # Time series comparison (Austria)
# ################################################################################
# 
# # create datasets for all gases
# gas <- "all_ghg"
# data_allghg <- read.csv(paste0("Data/02 intermediary/",gas,".csv")) %>% 
#   mutate(gas = "all_ghg")
# 
# gas <- "co2"
# data_co2 <- read.csv(paste0("Data/02 intermediary/",gas,".csv")) %>% 
#   mutate(gas = "co2")
# 
# gas <- "ch4"
# data_ch4 <- read.csv(paste0("Data/02 intermediary/",gas,".csv")) %>% 
#   mutate(gas = "ch4")
# 
# # combine datasets
# data_combined <- rbind(data_allghg, data_co2, data_ch4)
# 
# # select data
# data <- data_combined %>%
#   left_join(emissions_codes,
#             by = c("category" = "IPCC")) %>%
#   filter(!is.na(lemissions),
#          year >= 1995,
#          country == "Austria")
# 
# #plot
# ggplot(data = subset(data, country == "Austria")) +
#   geom_line(mapping = aes(x = year, y = emissions, color = gas)) +
#   facet_wrap(~ IPCC_description, scale = "free") +
#   labs(x = "Year",
#        y = "Emissions (kt carbon dioxide)") +
#   theme(axis.text = element_text(size = 6),
#         strip.text = element_text(size = 7))
# 
# 
# ################################################################################
# # Time series comparison - all EU15
# ################################################################################
# 
# # create datasets for all gases
# gas <- "all_ghg"
# data_allghg <- read.csv(paste0("Data/02 intermediary/",gas,".csv")) %>% 
#   mutate(gas = "all_ghg")
# 
# gas <- "co2"
# data_co2 <- read.csv(paste0("Data/02 intermediary/",gas,".csv")) %>% 
#   mutate(gas = "co2")
# 
# gas <- "ch4"
# data_ch4 <- read.csv(paste0("Data/02 intermediary/",gas,".csv")) %>% 
#   mutate(gas = "ch4")
# 
# # combine datasets
# data_combined <- rbind(data_allghg, data_co2, data_ch4)
# 
# # select data
# data <- data_combined %>%
#   left_join(emissions_codes,
#             by = c("category" = "IPCC")) %>%
#   filter(!is.na(lemissions),
#          year >= 1995,
#          country %in% EU15) %>% 
#   group_by(gas, IPCC_description, year) %>% 
#   summarise(emissions = sum(emissions))
# 
# #plot
# ggplot(data = subset(data)) +
#   geom_line(mapping = aes(x = year, y = emissions, color = gas)) +
#   facet_wrap(~ IPCC_description, scale = "free") +
#   labs(x = "Year",
#        y = "Emissions (kt carbon dioxide)") +
#   theme(axis.text = element_text(size = 6),
#         strip.text = element_text(size = 7))
# 
# 
# ################################################################################
# # Time series - subnational
# ################################################################################
# 
# # IPCC categories
# emissions_codes <- categories <- c("Agriculture", "Buildings", "Energy", "Industry", "Transport", "Waste") %>% 
#   as.data.frame()
# 
# # Austria codes
# AUT_codes <- c("AT11", "AT12", "AT13", "AT21", "AT22", "AT31", "AT32", "AT33", "AT34")
# 
# # Choose gas
# gas <- "all_subnat"
# data <- read.csv(paste0("Data/02 intermediary/",gas,".csv")) %>% 
#   filter(country %in% AUT_codes) %>% 
#   group_by(country, category, year) %>% 
#   summarise(emissions = sum(emissions))
#   
# #plot
# ggplot(data = subset(data)) +
#   geom_line(mapping = aes(x = year, y = emissions, color = country)) +
#   facet_wrap(~ IPCC_description, scale = "free") +
#   labs(x = "Year",
#        y = "Emissions (kt carbon dioxide)") +
#   theme(axis.text = element_text(size = 6),
#         strip.text = element_text(size = 7))
