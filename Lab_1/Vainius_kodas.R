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

#miss.data <-full.data[!complete.cases(full.data),]
miss.data <- full.data

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
full.data<-miss.data
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

library(ggstatsplot)
boxplot(full.data$Revenue, horizontal = TRUE)
boxplot(full.data$Employees, horizontal = TRUE)
boxplot(full.data$Expenses, horizontal = TRUE)
boxplot(full.data$Profit, horizontal = TRUE)
boxplot(full.data$Growth, horizontal = TRUE)


ggbetweenstats(data = full.data,
               x = Industry,
               y = Revenue            ,
               plot.type = "box", mean.plotting=FALSE,
               results.subtitle=FALSE,
               outlier.tagging = TRUE, outlier.label = "Name")
ggbetweenstats(data = full.data,
               x = Industry,
               y = Expenses                  ,
               plot.type = "box", mean.plotting=FALSE,
               results.subtitle=FALSE,
               outlier.tagging = TRUE, outlier.label = "Name")
ggbetweenstats(data = full.data,
               x = Industry,
               y = Employees                  ,
               plot.type = "box", mean.plotting=FALSE,
               results.subtitle=FALSE,
               outlier.tagging = TRUE, outlier.label = "Name")
mix.data <- full.data[full.data$Employees<6000,]

x <- full.data$Employees
boxplot(x, horizontal = TRUE)
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]

ggbetweenstats(data = mix.data,
               x = Industry,
               y = Employees                  ,
               plot.type = "box", mean.plotting=FALSE,
               results.subtitle=FALSE,
               outlier.tagging = TRUE, outlier.label = "Name")

ggbetweenstats(data = full.data,
               x = Industry,
               y = Profit                  ,
               plot.type = "box", mean.plotting=FALSE,
               results.subtitle=FALSE,
               outlier.tagging = TRUE, outlier.label = "Name")

ggbetweenstats(data = full.data,
               x = Industry,
               y = Growth                  ,
               plot.type = "box", mean.plotting=FALSE,
               results.subtitle=FALSE,
               outlier.tagging = TRUE, outlier.label = "Name")

# removing outliers
outliers <- function(x) {
  
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  
  x > upper_limit | x < lower_limit
}

remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!outliers(df[[col]]),]
  }
  df
}

temp.data.G <- full.data %>% dplyr::select(Name, Industry, Growth)
boxplot(temp.data.G$Growth, horizontal = TRUE)
ggbetweenstats(data = temp.data.G,
               x = Industry,
               y = Growth                  ,
               plot.type = "box", mean.plotting=FALSE,
               results.subtitle=FALSE,
               outlier.tagging = TRUE, outlier.label = "Name")
temp.data.G<-remove_outliers(temp.data.G, c("Growth"))
summary(temp.data.G$Growth)

temp.data.R <- full.data %>% dplyr::select(Name, Industry, Revenue)
boxplot(temp.data.R$Revenue, horizontal = TRUE)
ggbetweenstats(data = temp.data.R,
               x = Industry,
               y = Revenue,
               plot.type = "box", mean.plotting=FALSE,
               results.subtitle=FALSE,
               outlier.tagging = TRUE, outlier.label = "Name")
temp.data.R<-remove_outliers(temp.data.R, c("Revenue"))
summary(temp.data.R$Revenue)

temp.data.E <- full.data %>% dplyr::select(Name, Industry, Expenses)
boxplot(temp.data.E$Expenses, horizontal = TRUE)
ggbetweenstats(data = temp.data.R,
               x = Industry,
               y = Expenses,
               plot.type = "box", mean.plotting=FALSE,
               results.subtitle=FALSE,
               outlier.tagging = TRUE, outlier.label = "Name")
temp.data.E<-remove_outliers(temp.data.E, c("Expenses"))
summary(temp.data.E$Expenses)

temp.data.P <- full.data %>% dplyr::select(Name, Industry, Profit)
boxplot(temp.data.P$Profit, horizontal = TRUE)
ggbetweenstats(data = temp.data.P,
               x = Industry,
               y = Profit,
               plot.type = "box", mean.plotting=FALSE,
               results.subtitle=FALSE,
               outlier.tagging = TRUE, outlier.label = "Name")
temp.data.P<-remove_outliers(temp.data.P, c("Profit"))
summary(temp.data.P$Profit)

temp.data.EM <- full.data %>% dplyr::select(Name, Industry, Employees)
boxplot(temp.data.EM$Employees, horizontal = TRUE)
ggbetweenstats(data = temp.data.EM,
               x = Industry,
               y = Employees,
               plot.type = "box", mean.plotting=FALSE,
               results.subtitle=FALSE,
               outlier.tagging = TRUE, outlier.label = "Name")
temp.data.EM<-remove_outliers(temp.data.EM, c("Employees"))
summary(temp.data.EM$Employees)
#summary:
summary(temp.data.G$Growth)
summary(temp.data.R$Revenue)
summary(temp.data.E$Expenses)
summary(temp.data.P$Profit)
summary(temp.data.EM$Employees)


min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

temp.data.EM <- full.data %>% dplyr::select(Name, Industry, Employees)
norm.data.EM<-full.data %>% dplyr::select(Name, Industry, Employees)
norm.data.EM$Employees <-(norm.data.EM$Employees - mean(norm.data.EM$Employees)) / sd(norm.data.EM$Employees)
norm.data.EM
norm.data.EM$Employees <-(norm.data.EM$Employees - mean(norm.data.EM$Employees)) / sd(norm.data.EM$Employees)
norm.data.EM

boxplot(norm.data.EM$Employees, horizontal = TRUE)
hist(norm.data.EM$Employees)

library(ggplot2)

Revenue.data <- full.data %>% group_by(Industry) %>% dplyr::summarize(Mean = mean(Revenue, na.rm=FALSE))
Revenue.data$key <- "Revenue"
Profit.data <- full.data %>% group_by(Industry) %>% dplyr::summarize(Mean = mean(Profit, na.rm=FALSE))
Profit.data$key <- "Profit"

mean.data <- rbind(Revenue.data,Profit.data)
mean.data <- mean.data[complete.cases(mean.data),]

ggplot(mean.data, aes(fill=key, y=Mean, x=Industry)) + 
  geom_bar(position='identity', stat="identity", alpha = 0.5)

constr.data <- full.data[full.data$Industry == "Construction",]

tr<-ggplot(data=constr.data, aes(x=Revenue, y=Profit,
                         colour = Industry))
tr+ geom_point(aes(x=Revenue, y=Profit,
                   colour = Industry)) + geom_smooth(method = "loess", formula = y ~ x)
tr



