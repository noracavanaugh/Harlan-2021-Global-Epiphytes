#AppylingGAMstoGBIF,WorldClim&GrowthForm

#access tidyverse
library(tidyverse)
#access mgcv
library(mgcv)


#open readr to access Github SpClim csv
library (readr)
urlfile="https://raw.githubusercontent.com/AmyZanne/epiphytes/main/data/gbif_data/species_clim_s2.csv?token=APXLNLX3CXVOXDQYW7ZVX33BDE57Y"
spclim<-read_csv(url(urlfile))

#inspect spclim
str(spclim)
View(spclim)

#load consensus growth form data
consensus <- read_csv("data/growth_form_data/consensus_species_growthform.csv")

#join consensus data and spclim data
climform<-left_join(spclim, consensus,by = c("species" = "sp"))
View(climform)
str(climform)
table(climform$support)

#check NA values
sum(is.na(climform$support))
# Filter out NA
filter(climform, !is.na(support)) -> climform

#rename epiphytes
recode(climform$support, E = 1, C = 0, A = 0, F = 0, H = 0, M = 0, P = 0) -> climform$support

#make a new object for 2.5% quantiles that filters out number of obs under 50
only_big <- dplyr::filter(climform,number_of_obs>50)


#MEDIAN LATITUDE UNIVARIATE GAM
#fit a logistic gam predicting support by median latitude
log_climformlatmed <- gam(support ~ s(lat.50), data = climform,family = binomial,method = "REML")

#plot on probability scale centered around the intercept with intercept related uncertainty
plot(log_climformlatmed, pages = 1, trans = plogis, shift = coef(log_climformlatmed)[1], seWithMean = TRUE, shade = TRUE, shade.col = "yellowgreen", xlab = "Median Latitide (\u00B0)", ylab = "Proportion Epiphytic Species")

#summarize median lat GAM
summary(log_climformlatmed)

#check model
gam.check(log_climformlatmed)


#ABSOLUTE VALUE MAX LATITUDE UNIVARIATE GAM
#make new variable of abs value of lat2.5 column
climform$lat2.5abs <- abs(climform$lat2.5)

#make new variable of abs value of lat97.5 column
climform$lat97.5abs <- abs(climform$lat.975)

#new variable of max abs value 
climform$latmax <- apply(climform[,17:18], 1, max)

#fit a logistic GAM predicting support using max abs val latitude
log_climformlatmax <- gam(support ~ s(latmax), data = climform,family = binomial,method = "REML")

#plot on probability scale centered around the intercept with intercept related uncertainty
plot(log_climformlatmax, pages = 1, trans = plogis, shift = coef(log_climformlatmax)[1], seWithMean = TRUE, shade = TRUE, shade.col = "yellowgreen", xlab = "Maximum Absolute Latitide (\u00B0)", ylab = "Proportion Epiphytic Species")

#summarize abs val GAM
summary(log_climformlatmax)

#check model
gam.check(log_climformlatmax)


#MINTEMP2.5 UNIVARIATE GAM
#fit a logistic GAM predicting support using 2.5% quantile of min temp
log_climformmintemp2.5 <- gam(support ~ s(bio6.025), data = climform,family = binomial,method = "REML")

#plot on probability scale centered around the intercept with intercept related uncertainty
plot(log_climformmintemp2.5, pages = 1, trans = plogis, shift = coef(log_climformmintemp2.5)[1], seWithMean = TRUE)

#add a column to dataframe that is mintemp2.5 divided by 10 to get units in Celsius
climform$mintemp2.5degC <- climform$bio6.025/10

#make same model as above but with temp (x axis) in degrees Celsius
log_climformmintemp2.5 <- gam(support ~ s(mintemp2.5degC), data = only_big,family = binomial,method = "REML")

#plot on probability scale centered around the intercept with intercept related uncertainty
plot(log_climformmintemp2.5, pages = 1, trans = plogis, shift = coef(log_climformmintemp2.5)[1], seWithMean = TRUE, xlim = c(-20,25), shade = TRUE, shade.col = "yellowgreen", xlab = "Minimum Temperature of Coldest Month (\u00B0C)", ylab = "Proportion Epiphytic Species")

#summarize mintemp2.5 as sole predictor
summary(log_climformmintemp2.5)

#check model
gam.check(log_climformmintemp2.5)


#ANNPRECIP2.5 UNIVARIATE GAM
#fit a logistic GAM predicting support using 2.5% quantile of annual precipitation
log_climformannprecip2.5 <- gam(support ~ s(bio12.025), data = only_big,family = binomial,method = "REML")

#plot on probability scale centered around the intercept with intercept related uncertainty
plot(log_climformannprecip2.5, pages = 1, trans = plogis, shift = coef(log_climformannprecip2.5)[1], seWithMean = TRUE, shade = TRUE, shade.col = "yellowgreen", xlab = "Minimum Annual Precipitation (mm)", ylab = "Proportion Epiphytic Species")

#summarize annprecip2.5 as sole predictor
summary(log_climformannprecip2.5)

#check model
gam.check(log_climformannprecip2.5)

