# Analysis template -------------------------------------------------------

rm(list=ls())
set.seed(1230)

# install packages
# install.packages("devtools")
# devtools::install_github("moritzpschwarz/getspanel")
pacman::p_load(data.table, dplyr, tidyr, readxl, stringr, gets, here, ggplot2, devtools, cowplot)
library(getspanel)

#import data
gas <- "co2"
data <- read.csv(paste0("Data/02 intermediary/",gas,".csv"))

#filter for variable of interest
emissions_code <- "1.A.1.a" # add category of interest
emissions_category <- "Main Activity Electricity and Heat Production" # add category of interest

data <- data %>% 
  filter(category == emissions_code) %>% 
  select(-category)

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


# Analysis ----------------------------------------------------------------

# Set sample and prepare data
sample <- EU15
dat <- filter(data, country %in% sample, year>=1995)

file_name <- paste0("data/03 output/01 analysis_output/tests/",emissions_category)

###### Analysis:

cat(
  paste0(
    "#################################################################### \n",
    "#                                                                  # \n",
    "#                 CO2 DRIVERS EU - ANALYSIS                        # \n",
    "#                                                                  # \n",
    "#################################################################### \n",
    "\n \n \n"),
  file = file_name
)

# Print Sample Header
cat(
  paste0(
    "############################## \n",
    "#  COUNTRIES = ", length(sample), " \n",
    "############################## \n",
    "\n \n "),
  file = file_name,
  append = T
)

for(p.value in c(.05, .01, .001)){
  
  # Break analysis:
  is <- isatpanel(
    data = dat,
    formula = "lemissions_pc ~ lgdp + lgdp_sq + lpop" %>% as.formula,
    index = c("country", "year"),
    effect = "twoways",
    iis = T,
    fesis = T, 
    t.pval=p.value
  )
  
  # Print analysis results
  cat(
    paste0(
      " \n ###########################", 
      " \n # p-value: ", p.value,
      " \n \n "), 
    file = file_name, 
    append = T)
  
  sink(file_name, append=T)
  print(is)
  sink()
  
  cat(" \n \n \n \n \n", 
      file = file_name, 
      append = T)
}
