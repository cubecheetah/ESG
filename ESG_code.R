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