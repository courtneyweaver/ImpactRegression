# set working directory
setwd("~/ImpactRegression")

# download libraries
library(readxl)
library(plm)
library(dplyr)
library(stringr)
library(broom)
library(kableExtra)
library(lmtest)
library(sandwich)
library(pastecs)
library(tibble)
library(stargazer)

# import data
data <- read_xlsx("Master.xlsx")

# rename column names
names(data)[1] <- "ID"
names(data)[2] <- "year"
names(data)[3] <- "hours"
names(data)[4] <- "employees"
names(data)[5] <- "sales"

# delete duplicates
data <- data[!duplicated(data[c("year", "ID")]),]

# delete all data with incomplete values
data <- data[!is.na(data$hours),]
data <- data[!is.na(data$employees),]

# delete zero or NA sales
data <-data[!(data$sales==0),]
data <-data[!(data$sales<=5),]
data <- data[!is.na(data$sales),]

# number of clients
count <- data %>%
  summarise(n_distinct(ID))
count <- as.integer(count)
count

# define Hours Squared 
hoursSq <- (data$hours)^2

# define log of sales
data$lnSales <- log(data$sales)

# run panel regression 
panel <- plm(lnSales ~ hours + employees + hoursSq, data=data, index=c("ID", "year"), model="within")
summary(panel)

# print summary 
stargazer(panel, type="html",
          dep.var.labels=c("Sales"),
          out="models.txt")

# print summary statistics
stargazer(as.data.frame(data[c("hours","employees","sales")]), type="text",
          summary.stat= c("n","mean", "sd", "min","max"), out = "SS.txt", digits = 2)


