getwd()
setwd("/Users/sn.lili/ESG")

# Importing the data
library(readxl)
ESG <- read_excel("ESG_datadata.xlsx")

# converting numeric values
ESG$GC <- as.numeric(ESG$GC)
ESG$ESG <- as.numeric(ESG$ESG)
ESG$ROA <- as.numeric(ESG$ROA)
ESG$ROE <- as.numeric(ESG$ROE)
ESG$ROIC <- as.numeric(ESG$ROIC)
ESG$Industry <- as.factor(ESG$Industry)
ESG$Continent <- as.factor(ESG$Continent)
ESG$Sector <- as.factor(ESG$Sector)
ESG$Country <- as.factor(ESG$Country)

# Dealing with outliers and NA values
boxplot(ESG$ESG) 
ESG <- ESG[complete.cases(ESG), ] # remove NA values
row_sub <-  apply(ESG, 1, function(row) all(row !=0 ))
ESG <- ESG[row_sub,]
ESG[ESG==0] <- NA # The 0s have been removed

# Data visualisation
esquisse::esquisser()

# Check distributions
library("car")
qqPlot(ESG$ESG) # Normal
qqPlot(ESG$ROA) # Not normal
qqPlot(ESG$ROE) # 262 is an outlier, should be removed
qqPlot(ESG$ROIC) # 556 is an outlier, should be removed

# KStest
ks.test(ESG$ESG, "pnorm", mean=mean_esg, sd=stdev_esg) # p value is greater than 0.05 <- normally distributed
ks.test(ESG$ROA, "pnorm", mean=mean(ESG$ROA), sd=sd(ESG$ROA)) # p value is smaller than 0.05 <- not normal
ks.test(ESG$ROE, "pnorm", mean=mean(ESG$ROE), sd=sd(ESG$ROE)) # p value is smaller than 0.05 <- not normal
ks.test(ESG$ROIC, "pnorm", mean=mean(ESG$ROIC), sd=sd(ESG$ROIC)) # p value is smaller than 0.05 <- not normal

hist(log(ESG$ROA)) # better, we should use logarithmic transformation on the financial indicators

# Descriptive statistics
desc <- summary(ESG) # North america and Asia have the most data, there is only 1 for Australia and Africa (delete them?)

# Model

# First we regress the ESG on the log of ROA
model_1 <- lm(ESG ~ ROA, data = ESG)
summary(model_1)

model_2 <- lm(ESG ~ ROA + Industry, data = ESG)
summary(model_2)

model_3 <- lm(ESG ~ Industry, data = ESG)
summary(model_3)




