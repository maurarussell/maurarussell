## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(haven)


# set working directory
setwd("C:/Users/18562/OneDrive/TCNJ/sta215")

# Load the data
data <- read_delim("raw_data.csv")

#Missing data 
complete_data <- na.omit("raw_data.csv") 

# chi squared violent crime and immigration
table(data$imgrate10, data$vcrime10)
chisq.test(table(data$imgrate10, data$vcrime10))

# chi squared violent crime and poverty
table(data$pov_over_10,data$vcrime10)
chisq.test(table(data$pov_over_10,data$vcrime10))

#Box plot population over 500k and home value
boxplot(data$home_2023 ~ data$pop_over_500k_2021)
data$high_pop <- ifelse(data$pop_over_500k_2021 == 1, "yes", "no")
table(data$high_pop)
boxplot(data$home_2023 ~ data$pop_over_500k_2021, data = data,
        main = "Home Values by Population Over 500k (2021)",
        xlab = "Population Over 500k (2021)",
        ylab = "Home Value in 2023",
        outline = FALSE)  # This hides the outliers

#ANOVA test for home values and population over 500k
anova<-aov(data$home_2023 ~ data$pop_over_500k_2021, data = data)
summary(anova)

# linear regression plot for home values and population
lm(home_2023 ~ pop_2023, data = data)
summary(linear_regression)

#Scatter Plot female population male population
plot(data$pop_2023,data$home_2023)

#descriptive statistics for violent crime
summary(data$violent_crime_rate_2019)
sd(data$violent_crime_rate_2019)

#descriptive statistics for home values
summary(data$home_2023)
sd(data$home_2023)

#descriptive statistics for population over 500k
summary(data$pop_over_500k_2021)
sd(data$pop_over_500k_2021)


# descriptive statistics for violent crimes
table(data$vcrime10)
#description statistics for immigration
table(data$imgrate10)
