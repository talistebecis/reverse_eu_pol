# Packages ----------------------------------------------------------------
rm(list=ls())
pacman::p_load(dplyr, data.table, ggplot2, ggpubr, here, tidyverse)


# Setup -------------------------------------------------------
set.seed(1230)

#Group specification
EU15 <- c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland",
          "France", "United Kingdom", "Ireland", "Italy", "Luxembourg", 
          "Netherlands", "Greece", "Portugal", "Sweden")

EU16 <- c("Croatia", "Bulgaria", "Cyprus", "Czechia", "Estonia", 
          "Hungary", "Lithuania", "Latvia", "Malta", "Poland", "Romania", 
          "Slovak Republic", "Slovenia", "Switzerland", "Iceland", 
          "Norway")

EU31 <- c(EU15, EU16)

OECD_non_EU15 <- c("Czechia","Estonia", "Hungary", "Lithuania", "Latvia",
                   "Poland", "Slovak Republic", "Slovenia", "Switzerland", 
                   "Iceland", "Norway", "Australia", "Canada","Chile",
                   "Colombia", "Costa Rica", "Israel", "Japan",
                   "Korea, Republic of", "Mexico", "New Zealand", "Turkey",
                   "United States")

OECD <- c(EU15, OECD_non_EU15)


################################################################################
# BREAKDOWN OF EMISSIONS
################################################################################

# Plot --------------------------------------------------------------------

emissions_codes <- read.csv("data/01 source/IPCC 2006 Categories.csv") %>%
  select(IPCC, IPCC_description)

data <- read.csv("Data/02 intermediary/data_level3.csv") %>% 
  filter(country == "Austria",
         year == 2022) %>% 
  group_by(category) %>% 
  summarise(total_emissions = sum(emissions)) %>% 
  left_join(emissions_codes, by = c("category" = "IPCC")) %>% 
  arrange(total_emissions) %>% 
  mutate(percent = (total_emissions/sum(total_emissions))*100)

data %>% 
  mutate(IPCC_description=factor(IPCC_description, IPCC_description)) %>%
  ggplot(aes(x=IPCC_description, y=total_emissions)) +
  geom_segment(aes(x=IPCC_description ,xend=IPCC_description, y=0, yend=total_emissions), color="grey") +
  geom_point(size=3, color="gray") +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none") +
  xlab("") +
  ylab("Total emissions (kt carbon dioxide)")


################################################################################
# Time series
################################################################################

# Level 3 -----------------------------------------------------------------

data_full <- read.csv("Data/02 intermediary/data_level3.csv") %>%
  left_join(emissions_codes,
            by = c("category" = "IPCC")) %>%
  filter(!is.na(lemissions),
         year >= 1995,
         country == "Austria")

ggplot(data = subset(data_full, country == "Austria")) +
  geom_line(mapping = aes(x = year, y = emissions)) +
  facet_wrap(~ IPCC_description, scale = "free") +
  labs(x = "Year",
       y = "Emissions (kt carbon dioxide)") +
  theme(axis.text = element_text(size = 6),
        strip.text = element_text(size = 7))
