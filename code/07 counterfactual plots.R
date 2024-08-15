rm(list=ls())
set.seed(1230)
pacman::p_load(data.table, dplyr, tidyr, readxl, stringr, gets, here, ggplot2, devtools, cowplot)
library(getspanel)

#define vars
gas <- "co2"
EU15 <- c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland",
          "France", "United Kingdom", "Ireland", "Italy", "Luxembourg", 
          "Netherlands", "Greece", "Portugal", "Sweden")

#filter for variable of interest
emissions_code <- "1.A.1.a" # add category of interest
emissions_category <- "Main Activity Electricity and Heat Production" # add category of interest

#import data
data <- read.csv(paste0("Data/02 intermediary/",gas,".csv")) %>% 
  filter(category == emissions_code) %>% 
  select(-category)

# Analysis ----------------------------------------------------------------

# Set sample and prepare data
sample <- EU15
p.value <- .05
dat <- filter(data, country %in% sample, year>=1995)

is <- isatpanel(
  data = dat,
  formula = "lemissions_pc ~ lgdp + lgdp_sq + lpop" %>% as.formula,
  index = c("country", "year"),
  effect = "twoways",
  iis = T,
  fesis = T, 
  t.pval=p.value
)

plot_counterfactual(is)

plot_counterfactualTT(is)
