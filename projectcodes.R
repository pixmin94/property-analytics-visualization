#load data into R
setwd("/Users/corina/Desktop/Data Science Essentials/Project")
list.files()
housingData<-read.csv("./REALIS_Oct17toOct20.csv")
names(housingData)
#head(housingData)
#tail(housingData)
#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
#install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("wesanderson")
library(wesanderson)

#renaming columns to remove some typing errors
colnames(housingData)[2] <- "Transacted.Price"
colnames(housingData)[3] <- "Area.SQFT"
colnames(housingData)[4] <- "Unit.Price.PSF"
colnames(housingData)[9] <- "Area.SQM"
colnames(housingData)[10] <- "Unit.Price.PSM"
colnames(housingData)[11] <- "Nett.Price"
#converting factor to numeric
housingData$Transacted.Price <- as.numeric(gsub(",","", housingData$Transacted.Price))
housingData$Area.SQFT <- as.numeric(gsub(",","", housingData$Area.SQFT))
housingData$Unit.Price.PSF <- as.numeric(gsub(",","", housingData$Unit.Price.PSF))
housingData$Area.SQM <- as.numeric(gsub(",","", housingData$Area.SQM))
housingData$Unit.Price.PSM <- as.numeric(gsub(",","", housingData$Unit.Price.PSM))

housingData$Transacted.Price <- housingData$Transacted.Price/1000000
boxplot(Transacted.Price ~ Property.Type, data = housingData, xlab = "Property Type", ylab = "Price(M)", main = "Price by Property Type", ylim =c(0,20),col=brewer.pal(n = 6, name = "Spectral"))
type2 <-table(housingData$Type.of.Sale,housingData$Property.Type)
barplot(type2,col=brewer.pal(n = 6, name = "Set3"),ylab="Count",xlab="Property Types",legend=rownames(type2))
pie(table(housingData$Planning.Region),housingData$Planning.Region,main="Number of transactions by region")
hist(housingData$Unit.Price.PSF,main="", xlab="Unit Price PSF",breaks=5,col=brewer.pal(n = 5, name = "Pastel2"))


#data transformation
#divide transacted price by number of units for enbloc outliers
housingData$Price.Per.Unit <- housingData$Transacted.Price/housingData$Number.of.Units
housingData$Price.Per.Unit
housingData$Area.SQFT <- housingData$Area.SQFT/housingData$Number.of.Units
housingData$Area.SQFT

#get remaining tenure and add a new column
lease<- as.numeric(str_extract(housingData$Tenure, regex("[:digit:][:digit:]+"))) #extract 99 years leases
startdate<- as.numeric(str_extract(housingData$Tenure, regex("[:digit:][:digit:][:digit:][:digit:]"))) #extract tenure start year
housingData$Remaining.Tenure <- lease+startdate-2021
housingData <- housingData %>% mutate(Remaining.Tenure=ifelse(is.na(Remaining.Tenure),999,Remaining.Tenure)) #change freehold to 999years
housingData <- housingData %>% mutate(Remaining.Tenure=ifelse(Remaining.Tenure>1000,999,Remaining.Tenure))#change any tenure >1000 to 999 years
housingData$Remaining.Tenure



#correlation table
corr_data <- housingData %>% select(Transacted.Price,Price.Per.Unit,Unit.Price.PSF,Area.SQFT,Postal.District,Postal.Sector,Remaining.Tenure)
corr_table <- cor(corr_data)
#install.packages("corrplot")
library(corrplot)
corrplot(corr_table,method="number")


#linear regression
lmReTenure = lm(Transacted.Price~Remaining.Tenure, data = housingData)
summary(lmReTenure)
ggplot(housingData, aes(Remaining.Tenure, Transacted.Price)) +
geom_point() +
stat_smooth(method = lm)

lmArea = lm(Transacted.Price~Area.SQFT, data = housingData)
summary(lmArea)
ggplot(housingData, aes(Area.SQFT, Transacted.Price)) +
geom_point() +
stat_smooth(method = lm)

#multiple linear regression
lmA = lm(Transacted.Price~Area.SQFT+Postal.Sector+Price.Per.Unit+Remaining.Tenure+Unit.Price.PSF, data = housingData)
summary(lmA)

#clustering
housingCluster <- housingData %>% select(Transacted.Price,Area.SQFT,Postal.Sector,Price.Per.Unit,Remaining.Tenure,Unit.Price.PSF)
names(housingCluster)


normalize <- function(x) {
  return ((x - min(x))/ (max(x) - min(x))) }
#Normalize Data
z <- housingCluster
normalizeHousingCluster <- cbind(as.data.frame(lapply(z[,1:3],normalize)))

#Run K-means cluster
set.seed(120) #Set the random order

kmc <- kmeans(normalizeHousingCluster, 6)
kmc
plot(Price.Per.Unit~Area.SQFT, housingCluster, col=kmc$cluster)
plot(Transacted.Price~Postal.Sector, housingCluster, col=kmc$cluster)
plot(Transacted.Price~Unit.Price.PSF, housingCluster, col=kmc$cluster)
