getwd()
setwd("/Users/sn.lili/ESG")

# Importing the data
library(readxl)
ESG <- read_excel("ESG_datadata.xlsx")

# We don't need some variables values
ESG$GC <- NULL
ESG$Similarity <- NULL
ESG$ROIC <- NULL
ESG$ROE <- NULL

# converting numeric values and factors
ESG$ESG <- as.numeric(ESG$ESG)
ESG$ROA <- as.numeric(ESG$ROA)
ESG$ROE <- as.numeric(ESG$ROE) # In case we need it
ESG$ROIC <- as.numeric(ESG$ROIC) # In case we need it
ESG$GC <- as.numeric(ESG$GC)
ESG$Industry <- as.factor(ESG$Industry)
ESG$Continent <- as.factor(ESG$Continent)
ESG$Sector <- as.factor(ESG$Sector)
ESG$Country <- as.factor(ESG$Country)

# Corrplot
library(corrplot)
M <- cor(ESG[3:7]) # only usable if no columns were removed
corrplot(M, method = 'number') 
# Strong correlation between GC and ESG, but weak (positive) association between ROA and ESG
# Almost 0 correlation between ROE, ROIC and the other variables

# Dealing with outliers and NA values
boxplot(ESG$ESG)
ESG[ESG==0] <- NA # The 0s will be converted to NAs
ESG <- ESG[complete.cases(ESG), ] # and removed from the df
summary(ESG)
# Data visualisation
esquisse::esquisser() # There is a ROA outlier (AirAsia with -5000 ROA)

library(ggplot2)

ggplot(ESG) +
 aes(x = Continent, y = ESG) +
 geom_boxplot(shape = "circle", fill = "#112446") +
 theme_minimal()

ggplot(ESG) +
 aes(x = Industry, y = ESG) +
 geom_boxplot(shape = "circle", fill = "#112446") +
 theme_minimal()

ESG <- ESG[-c(29), ] # drop the 29th row as it is a negative ROA outlier


# Check distributions
library("car")
qqPlot(ESG$ESG) # Looks normal
qqPlot(ESG$ROA) # Not normal

# KStest
ks.test(ESG$ESG, "pnorm", mean=mean(ESG$ESG), sd=sd(ESG$ESG)) # p value is greater than 0.05 <- normally distributed
ks.test(ESG$ROA, "pnorm", mean=mean(ESG$ROA), sd=sd(ESG$ROA)) # p value is smaller than 0.05 <- not normal

hist(ESG$ROA) # we should transform it or remove outliers(?)

# Descriptive statistics
summary <- summary(ESG) # North america and Asia have the most data, there is only 1 for Australia and Africa (consider deleting them?)

# Model

# First we regress the ESG on ROA
model_1 <- lm(ESG ~ ROA, data = ESG)
summary(model_1)

model_2 <- lm(ESG ~ ROA + Industry, data = ESG)
summary(model_2)

model_3 <- lm(ESG ~ ROA + Industry + Continent, data = ESG)
summary(model_3)
# The R^2 is still quite low idk what to do :/ add size of firm or age of firm (?) or sample



# Adding dummies for mandatory ESG reporting (after Krueger et al. 2021)
ESG$mand <- ifelse(ESG$Country == "Argentina" | ESG$Country == "Australia" 
                   | ESG$Country == "Austria"| ESG$Country == "Canada" | ESG$Country == "Chile" 
                   | ESG$Country == "China" | ESG$Country == "Germany" | ESG$Country == "Greece"| 
                           ESG$Country == "Hong Kong"| ESG$Country == "Hungary"| ESG$Country == "Indonesia"| 
                           ESG$Country == "Ireland"| ESG$Country == "Italy"| ESG$Country == "India"|
                           ESG$Country == "Malaysia"| ESG$Country == "Netherlands"
                   | ESG$Country == "Norway" | ESG$Country == "Pakistan" | ESG$Country == "Peru"
                   | ESG$Country == "Philippines" | ESG$Country == "Poland"| ESG$Country == "Portugal"
                   | ESG$Country == "Singapore" | ESG$Country == "Slovenia" | ESG$Country == "South Africa"
                   | ESG$Country == "Spain"| ESG$Country == "Turkey"| ESG$Country == "United Kingdom"
                   | ESG$Country == "Taiwan, Province of China", 1, 0)

model_mand <- lm(ESG ~ ROA + mand + Industry + Continent, data = ESG)
summary(model_mand) # it has negative explanatory power? why :O


electronics <- ESG[ which(ESG$Industry =='Electronic Technology'), ]
model_electronics <- lm(ESG ~ ROA + Country + mand, data = electronics)
summary(model_electronics) # The R2 is 0.3, amazing

finance <- ESG[ which(ESG$Industry =='Finance'), ]
model_finance <- lm(ESG ~ ROA + Country + mand, data = finance)
summary(model_finance) # ooh wow

europe <- ESG[ which(ESG$Continent =='Europe'), ]
model_europe <- lm(ESG ~ ROA + mand + Country + Industry, data = europe)
summary(model_europe) # overspecified

northam <- ESG[ which(ESG$Continent =='North America'), ]
model_na <- lm(ESG ~ ROA + Sector + mand, data = northam)
summary(model_na)
