setwd("C:/Users/vaini/Documents/duomenu viz 2/1 lab")
full.data <- read.csv("Future-500-3.csv")

library(naniar)

# pirminis duomenu pataisymas galiojantis visiems pjuviams
summary(full.data)

full.data$Profit <- as.numeric(as.character(full.data$Profit))

full.data$Expenses <- gsub(" Dollars","",full.data$Expenses)
full.data$Expenses <- gsub(",","",full.data$Expenses)
full.data$Expenses <- as.numeric(as.character(full.data$Expenses))
head(full.data)
str(full.data)
full.data$Revenue <- gsub("\\$","",full.data$Revenue)
full.data$Revenue <- gsub(",","",full.data$Revenue)
full.data$Revenue <- as.numeric(as.character(full.data$Revenue))
head(full.data)
full.data$Growth <- gsub("\\%","",full.data$Growth)
full.data$Growth <- as.numeric(as.character(full.data$Growth))
head(full.data, 20)

histograms <- function(full.data){
  
  par(mfrow=c(1,4))
  hist(full.data$Growth)
  hist(full.data$Profit)
  hist(full.data$Revenue)
  hist(full.data$Expenses)
}
attach(mtcars)
histograms(full.data)

# praleistų reikšmių tvarkymas
full.data[full.data == ""] <- NA

miss.data <-full.data[!complete.cases(full.data),]
nrow(miss.data)
# kadangi profit = revenue - expense
miss.data$Profit <- ifelse(is.na(miss.data$Profit), miss.data$Revenue-miss.data$Expenses, miss.data$Profit)
miss.data$Revenue <- ifelse(is.na(miss.data$Revenue), miss.data$Profit+miss.data$Expenses, miss.data$Revenue)
miss.data$Expenses <- ifelse(is.na(miss.data$Expenses), miss.data$Revenue-miss.data$Profit, miss.data$Expenses)

# kadangi valstija galima nuspeti pagal miesta
miss.data[is.na(miss.data$State) &miss.data$City=="New York", "State"] <- "NY"
miss.data[is.na(miss.data$State) &miss.data$City=="San Francisco", "State"] <- "CA"
miss.data[is.na(miss.data$State) &miss.data$City=="Des Plaines", "State"] <- "IL"
miss.data[is.na(miss.data$State) &miss.data$City=="El Segundo", "State"] <- "LA"
miss.data[is.na(miss.data$State) &miss.data$City=="Madison", "State"] <- "WI"

nrow(miss.data[!complete.cases(miss.data),])
# randamos skaitiniu kintamuju medianos pagal industrijos grupavima
library(data.table)
library(tidyverse)
library(dplyr)
summary(full.data)

full.data <-full.data %>%
  group_by(Industry) %>%
  mutate(
    Profit = impute_median(Profit),
    Growth = impute_median(Growth),
    Expenses = impute_median(Expenses),
    Revenue = impute_median(Revenue),
    Employees = impute_median(Employees)  )


summary(full.data)
histograms(full.data)
full.data
