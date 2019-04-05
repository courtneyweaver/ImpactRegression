# set working directory
setwd("~/ImpactRegression")

# download libraries
library(readxl)
library(plm)
library(dplyr)
library(broom)
library(kableExtra)
library(lmtest)
library(sandwich)

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
panel <- plm(lnSales ~ hours+employees+hoursSq, data=data, index=c("ID", "year"), model="within")
summary(panel)

tbl <- tidy(panel)
kable(tbl, digits=5, caption=
        "Fixed effects using 'within' with full sample")

# print summary using robust standard errors
coeftest(panel, vcov. = vcovHC, type = "HC1")










