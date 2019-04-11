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
data <- data[!is.na(data$hours),!is.na(data$employees),]
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
tbl <- tidy(panel)
kable(tbl, digits=5, caption=
        "Estimation of Sales using Fixed Effects")%>%
  kable_styling(bootstrap_options = "striped", full_width = F)


# print summary using robust standard errors
a <- coeftest(panel, vcov. = vcovHC, type = "HC1")
b <- tidy(a)
kable(b, digits=5, caption=
        "Estimation of Sales using Fixed Effects with Robust Standard Errors")%>%
  kable_styling(bootstrap_options = "striped", full_width = F)

# summary statistics on control variables
options(scipen=100)
options(digits=1)
c <- stat.desc(data)
d <-t(c)
d <- as.data.frame(d)

# drop columns
e <- d[ -c(2:3,6:8,10:12,14) ] 

# drop rows
f <- e[-c(1:2,6), ]
row <- row.names(f)
rows <- str_to_title(row)
g <- cbind(rows,f$nbr.val, f$min, prettyNum((f$max),big.mark=",", scientific=FALSE), 
    prettyNum((f$mean),big.mark=",", scientific=FALSE), prettyNum((f$std.dev),big.mark=",", scientific=FALSE))

# create table of summary statistics
kable(g, col.names = c("Variable", "Obs", "Min","Max", "Mean", "Std Dev"), 
      caption = "Summary Statistics") %>% 
  kable_styling(bootstrap_options = "basic", full_width = F, position = "center")

