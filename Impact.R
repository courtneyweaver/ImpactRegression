# set working directory
setwd("~/ImpactRegression")

# download libraries
library(readxl)
library(plm)
library(dplyr)

# import data
data <- read_xlsx("Master.xlsx")

# make data balanced (baseline)
data_b <- make.pbalanced(data, balance.type = "shared.individuals")

# number of clients in data set
count <- data %>%
  summarise(n_distinct(CISCaseID))
count <- as.integer(count)
count




