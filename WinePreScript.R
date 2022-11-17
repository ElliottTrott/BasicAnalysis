# preprocessing script

# necessary imports
library(readr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(Hmisc)
library(corrplot)

# load the data in
WineData <- read_csv("~/Desktop/WineAnalysisET/WineAnalysis/data/WineQualityRed.csv")

## BASIC ANALYSIS
head(WineData)
# successfully loaded in - dims? 1599 x 12
dim(WineData) 
# listing headers - "alcohol" "chlorides"   "citric acid"    "density"  "fixed acidity" "total sulfur dioxide" "volatile acidity"
# "free sulfur dioxide"  "pH" "quality"  "residual sugar" "sulphates" 
ls(WineData)
# remove any repeats
WineData <- WineData %>% distinct()
# dims of new data set
dim(WineData)

# summary of data set
summary(WineData)

## RANGES
# range of data for quality - 3 - 8
range(WineData$quality)
# alcohol - 8.4 - 14.9
range(WineData$alcohol)
# ph 2.74 - 4.01 - could split these into increments of .2/3
range(WineData$pH)
# density - 0.99007 1.00369 - split into x denser than water
range(WineData$density)
# fixed acidity 4.6 - 15.9 - split into whole numbers 
range(WineData$`fixed acidity`)
# volatile acidity 0.12 1.58
range(WineData$`volatile acidity`)
# citric acid - 0/1 - is this binary?
range(WineData$`citric acid`)
table(WineData$`citric acid`) # not binary
# sugar 0.9 - 15.5
range(WineData$`residual sugar`)
# chlorides 0.012 - 0.611
range(WineData$chlorides)
# f SO2 1 -72
range(WineData$`free sulfur dioxide`)
# t SO2 6 - 289
range(WineData$`total sulfur dioxide`)
# sulphates 0.33 -  2.00
range(WineData$sulphates)

# plot histograms of each column?
# plotting histogram of fixed acidity
ggplot(WineData, aes(x = `fixed acidity`)) + geom_histogram(binwidth = 0.5)

# plot all histograms using tools
hist.data.frame(WineData)
# saving as vector for report
DFHist <- hist.data.frame(WineData)

# could also add in a correlation plot, explaining the relationships at 'face value'
# need to compute the correlation matrix first by setting values within a range 
CorWineData <- cor(WineData)
# plotting the correlation using method 'colour'
corrplot(CorWineData, method = 'color')
# saving as a vector
CorDF <- corrplot(CorWineData, method = 'color')



# the y value will be the quality of the wine. ranging from 3-8.

## BASIC PLOTS
# numbers of each individual quality
table(WineData$quality)
## Alcohol vs Quality
# could line plot alcohol (x) vs quality (y) as a scatter plot
ggplot(WineData, aes(x = quality, y = alcohol)) + geom_point()
# bound by the y axis, view as boxplot, grouping by quality so each value has its own boxplot.
ggplot(WineData, aes(x=quality, y=alcohol, group=quality)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=2)
# storing the plot to call within the report
PWineAlcoholBox <-  ggplot(WineData, aes(x=quality, y=alcohol, group=quality)) + 
  geom_boxplot(fill='#A4A4A4', color="black") + theme_classic()
PWineAlcoholBox


## pH Vs Quality
# boxplot of relationship
ggplot(WineData, aes(x=quality, y=pH, group=quality)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=2)

ggplot(WineData, aes(x= frequency(quality) , y = quality )) + 
  geom_bar(stat="identity", colour="black", fill="white") + 
  xlab("") + ylab("")


## use mean averages for different qualities to assess the significance of changing variables?



## CLASSIFICATION / REGRESSION
# want to see which factors effect the quality the most

# single regression for alcohol to quality
alcohol.qual.lm <- lm(quality ~ alcohol, data = WineData)
# summary
summary(alcohol.qual.lm)

# plotting
par(mfrow=c(2,2))
plot(alcohol.qual.lm)
# there are biasis within the data and outliers - the data doesn't fit the assumption of homoscedasticity



# multiple regression
multiple1.lm <- lm(quality ~ `fixed acidity` + `volatile acidity` + `citric acid` + `residual sugar`, data = WineData)
# summary
summary(multiple1.lm)
plot(multiple1.lm)

# adding a linear regression to a scatter graph
ggplot(WineData, aes(x = quality, y = alcohol)) + geom_point() + geom_smooth(method = "lm", col = "black")



## OTHER INVESTIGATIONS

# Residual Sugars 
# breakdown/ distribution of wines by column residual sugar
table(WineData$`residual sugar`)
range(WineData$`residual sugar`)
# range is 0.9 - 15.5, Mostly concentrated around 1-3, could split them up by 0.5s until x > 3.0
# create a new df with new column for res.sugar split into the new ranges?


# need to apply this same technique with ranges
# applying the same with wine data and range:
res.sugar.df <- WineData %>% mutate(res.sugar = case_when(.$`residual sugar` >= 3.0 ~ "x > 3.0",
                                                          .$`residual sugar` >= 2.5 ~ "2.5-3.0",
                                                          .$`residual sugar` >= 2.0 ~ "2.0-2.5",
                                                          .$`residual sugar` >= 1.5 ~ "1.5-2.0",
                                                          TRUE ~ "0.8-1.5"))

# check if this has worked
table(res.sugar.df$res.sugar)
# worked as desired.

# filter by residual sugars?

# firstly produce a boxplot?
ggplot(res.sugar.df, aes(x=res.sugar, y=quality, group=res.sugar)) + 
  geom_boxplot(fill='#A4A4A4', color="black") + theme_classic()

ggplot(res.sugar.df, aes(x=quality, y=res.sugar)) + geom_point() + geom_smooth(method = "lm", col = "black")
# there is no relationship between residual sugar and quality



# filter using dplyr to show density for selected qualities.
density.mean.qual.8 <- WineData %>% select(quality, density) %>% 
  filter(quality == "8") %>% summarise(average.density = mean(density))

# do the same for quality 3
density.mean.qual.3 <- WineData %>% select(quality, density) %>% 
  filter(quality == "3") %>% summarise(average.density = mean(density))

# all qualities other than 8
density.mean.qual.not8 <- WineData %>% select(quality, density) %>% 
  filter(quality <= 8) %>% summarise(average.density = mean(density))


## bar chart of average density
# first state means using group by function
MeanDensityWD <- WineData %>% group_by(quality) %>% summarise(MeDense = mean(density))
# plot bar chart of densities against quality
ggplot(MeanDensityWD, aes(x = quality, y = MeDense)) + geom_bar(stat = "identity")

# the chart is too grouped together:
# perhaps need to perform logarithmic statistics here to get any form of separation.
# checking the output from log of mean
log(mean(WineData$density))
log(WineData$density)

# performing same action of group by with logs
LogDensity <- WineData %>% group_by(quality) %>% summarise(LogMeDense = log(mean(density)))
# plotting with logs
ggplot(LogDensity, aes(x = quality, y = LogMeDense)) + geom_bar(stat = "identity")
# there are clear differences in density by quality here, a general trend of a decreasing density as the quality increases
# experimenting with the plot - adding a linear regression over it.
PlotLogDensity <- ggplot(LogDensity, aes(x = quality, y = LogMeDense)) + geom_bar(stat = "identity", fill='#A4A4A4') + 
  geom_smooth(method = "lm", col = "black")




