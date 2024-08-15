# Setup -------------------------------------------------------

parallel::stopCluster(cl = my.cluster)
rm(list=ls())
set.seed(1230)

# install packages
# install.packages("devtools")
# devtools::install_github("moritzpschwarz/getspanel")
pacman::p_load(data.table, dplyr, tidyr, readxl, stringr, gets, here, ggplot2, devtools, cowplot)
library(getspanel)

# IPCC categories
emissions_codes <- read.csv("Data/01 source/IPCC 2006 Categories.csv") %>% 
  select(IPCC)

categories <- read.csv("Data/01 source/IPCC 2006 Categories.csv") %>% 
  select(IPCC_description)

# Group specifications
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


# Parameters --------------------------------------------------------------

# Choose sample
sample_countries <- OECD

# Choose gas
gas <- "n2o"
data_full <- read.csv(paste0("Data/02 intermediary/",gas,".csv"))

# Name sample (folder name)
sample_name <- "n2o_OECD"


# Set up parallel computing -----------------------------------------------

pacman::p_load(parallel, foreach, doParallel)
n.cores <- 7

#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)

#check cluster definition
print(my.cluster)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

#check if it is registered
foreach::getDoParRegistered()

#how many workers are available? (optional)
foreach::getDoParWorkers()


# Analysis ----------------------------------------------------------------

#run loop over all variables to produce results for all IPCC categories
x <- foreach(
  IPCC_code = 1:37,
  .combine = 'c'
) %dopar% {
  
  #load packages
  pacman::p_load(data.table, dplyr, tidyr, readxl, stringr, gets, here, ggplot2, devtools)
  library(getspanel)
  
  #set IPCC code and category
  emissions_code <- emissions_codes[IPCC_code,]
  emissions_category <- categories[IPCC_code,]
  
  #set up document
  file_name <- paste0("data/03 output/01 analysis_output/",sample_name,"/",emissions_category)
  
  #filter data
  data <- data_full %>% 
    filter(category == emissions_code) %>% 
    select(-category)
  
  #results document header
  cat(
    paste0(
      "#################################################################### \n",
      "#                                                                  # \n",
      "#                    CO2 DRIVERS - ANALYSIS                        # \n",
      "#                                                                  # \n",
      "#################################################################### \n",
      "\n \n \n"),
    file = file_name
  )

  # Prepare sample and data
  sample <- sample_countries
  dat <- filter(data, country %in% sample, year>=1995)
  
  # Print Sample Header
  cat(
    paste0(
      "############################## \n",
      "#  SAMPLE = ", sample_name, " \n",
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
}
