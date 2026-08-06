1. You may want to filter the birdphenology data to only focus on cases where the species is bluti, and there is data for FED and FKI and the visit frequency column doesn’t have text in it - this means that you’d be focusing on data where we know the first egg-date precisely.

#you already have a dataset called filteredpheno2

2. You could work out what the average duration is between first egg date and first known incubation. Then for each nestbox the focal time period is the period between FED and the end of this period.

#The average duration from fed to fki is 10.16

#The average duration from fki to hatching_first_recorded is 13.32 

#The average duration from hatching_first_recorded to fledging is 18

3. The next step is much trickier. You need to open the temperatures dataset (in the site folder of master data) and for each nest you need to match (i) the site, (ii) the year and (iii) the days of the year over which you want to select the temperature data for that site. The columns of the temperature data are the days and fractions of days (hourly) on which temperatures were recorded. For most site - year combinations there are two rows of data and you’ll want to take the average. 
All of step 3 will probably require a loop. You also need to decide what temperature data you want to extract - maybe mean temperature over the focal period for each nest, or maybe you want to select only night time temperatures. Or you could try both. 
- I don’t expect you to get beyond this stage but am laying the ideas out here.

loggerdata<-read.csv("~/Library/CloudStorage/Dropbox/transect/master_data/site/temperatures.csv")

#Read in the logger data, each row is a logger and each column is an hour of tempertaure

#create a vector to save your temperature metric, e.g., tempmetric<-c()

#set up a loop that goes through all the nextboxes with fed, fki and suc data

{

#for nestbox 1 in the loop you will.....
	
	
	#obtain the fed and fed + 10.16 days
	
	#find the columns of the loggerdata that corresponds to this range
	
	#find the right site and year in the logger data
	
	#select the temperature data and if there's two rows then take the hourly average
	
	#decide and calculate your temperature metric  - e.g., mean of daily minima, minumum, mean, median etc.
	
	#save your tempertaure metric in the correct location e..g., tempmetric[x]<-
	
	#end loop
}

# NICOLE LOOP ATTEMPT :0

##### Load datasets and refine #####

# nest data
nestdata <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Thesis/Data play/Bird_Phenology.csv")

# remove clutch swap treatment
nestdata <- nestdata %>%
  filter(
    is.na(clutch.swap.treatment) |
      clutch.swap.treatment == "" |
      clutch.swap.treatment == "unmanipulated"
  )

# temp data
loggerdata <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Thesis/Data play/temperatures.csv")
# remove columns without temp measurements
loggerdata <- loggerdata[, !(names(loggerdata) %in% c("logger_id", "logger_res"))]

##### Making temp min loops #####

## fed data ##
# create storage vector of NAs which get filled as loop runs 
tempmin_fed <- rep(NA, nrow(nestdata))
meta.cols <- c("site","year")
temp.cols <- setdiff(names(loggerdata), meta.cols)
temp.times <- as.numeric(gsub("^X", "", temp.cols))

# make fed loop
for(x in 1:nrow(nestdata)) {
  # select a nest
  site.i <- nestdata$site[x]
  year.i <- nestdata$year[x]
  box.i <- nestdata$box[x]
  # select time window
  fed.start <- nestdata$fed[x]
  fed.end <- fed.start + 10.16
  # skip if missing fed
  if(is.na(fed.start)) next
  # find temps in time window 
  cols.use <- which(temp.times >= fed.start &
                      temp.times <= fed.end)
  # skip if no matching temps
  if(length(cols.use) == 0) next
  # find matching site-year rows
  rows.use <- which(loggerdata$site == site.i &
                      loggerdata$year == year.i)
  # skip is no matching loggers 
  if(length(rows.use) == 0) next
  # extract temp subset
  temp.sub <- loggerdata[rows.use, temp.cols[cols.use]]
  # convert to matrix
  temp.sub <- as.matrix(temp.sub)
  # calc temp logger averages for each site-year
  if(nrow(temp.sub) > 1) {
    hourly.temp <- colMeans(temp.sub, na.rm = TRUE)
  } else {
    # when just one temp logger  
    hourly.temp <- as.numeric(temp.sub)
  }
  # for time window
  selected.times <- temp.times[cols.use]
  # make into whole day not hour temps
  day.id <- floor(selected.times)
  
  # absolute minimum temperature
  tempmin_fed[x] <- min(hourly.temp, na.rm = TRUE)
}

nestdata$tempmin_fed <- tempmin_fed

summary(tempmin_fed)

## fki data ##
# create storage vector of NAs which get filled as loop runs 
tempmin_fki <- rep(NA, nrow(nestdata))
meta.cols <- c("site","year")
temp.cols <- setdiff(names(loggerdata), meta.cols)
temp.times <- as.numeric(gsub("^X", "", temp.cols))

# make fki loop
for(x in 1:nrow(nestdata)) {
  # select a nest
  site.i <- nestdata$site[x]
  year.i <- nestdata$year[x]
  box.i <- nestdata$box[x]
  # select time window
  fki.start <- nestdata$fki[x]
  fki.end <- fki.start + 13.32
  # skip if missing fed
  if(is.na(fki.start)) next
  # find temps in time window 
  cols.use <- which(temp.times >= fki.start &
                      temp.times <= fki.end)
  # skip if no matching temps
  if(length(cols.use) == 0) next
  # find matching site-year rows
  rows.use <- which(loggerdata$site == site.i &
                      loggerdata$year == year.i)
  # skip is no matching loggers 
  if(length(rows.use) == 0) next
  # extract temp subset
  temp.sub <- loggerdata[rows.use, temp.cols[cols.use]]
  # convert to matrix
  temp.sub <- as.matrix(temp.sub)
  # calc temp logger averages for each site-year
  if(nrow(temp.sub) > 1) {
    hourly.temp <- colMeans(temp.sub, na.rm = TRUE)
  } else {
    # when just one temp logger  
    hourly.temp <- as.numeric(temp.sub)
  }
  # for time window
  selected.times <- temp.times[cols.use]
  # make into whole day not hour temps
  day.id <- floor(selected.times)
  
  # absolute minimum temperature 
  tempmin_fki[x] <- min(hourly.temp, na.rm = TRUE)
}

nestdata$tempmin_fki <- tempmin_fki

summary(tempmin_fki)

## hatch data ##
# create storage vector of NAs which get filled as loop runs 
tempmin_hatch <- rep(NA, nrow(nestdata))
meta.cols <- c("site","year")
temp.cols <- setdiff(names(loggerdata), meta.cols)
temp.times <- as.numeric(gsub("^X", "", temp.cols))

# make hatch loop
for(x in 1:nrow(nestdata)) {
  # select a nest
  site.i <- nestdata$site[x]
  year.i <- nestdata$year[x]
  box.i <- nestdata$box[x]
  # select time window
  hatch.start <- nestdata$hatching_first_recorded[x]
  hatch.end <- hatch.start + 18
  # skip if missing fed
  if(is.na(hatch.start)) next
  # find temps in time window 
  cols.use <- which(temp.times >= hatch.start &
                      temp.times <= hatch.end)
  # skip if no matching temps
  if(length(cols.use) == 0) next
  # find matching site-year rows
  rows.use <- which(loggerdata$site == site.i &
                      loggerdata$year == year.i)
  # skip is no matching loggers 
  if(length(rows.use) == 0) next
  # extract temp subset
  temp.sub <- loggerdata[rows.use, temp.cols[cols.use]]
  # convert to matrix
  temp.sub <- as.matrix(temp.sub)
  # calc temp logger averages for each site-year
  if(nrow(temp.sub) > 1) {
    hourly.temp <- colMeans(temp.sub, na.rm = TRUE)
  } else {
    # when just one temp logger  
    hourly.temp <- as.numeric(temp.sub)
  }
  # for time window
  selected.times <- temp.times[cols.use]
  # make into whole day not hour temps
  day.id <- floor(selected.times)
  
  # absolute minimum temperature 
  tempmin_hatch[x] <- min(hourly.temp, na.rm = TRUE)
}

nestdata$tempmin_hatch <- tempmin_hatch

summary(tempmin_hatch)

##### Making temp mean of minima loops #####

## fed data ##
# create storage vector of NAs which get filled as loop runs 
tempmeanmin_fed <- rep(NA, nrow(nestdata))
meta.cols <- c("site","year")
temp.cols <- setdiff(names(loggerdata), meta.cols)
temp.times <- as.numeric(gsub("^X", "", temp.cols))

# make fed loop
for(x in 1:nrow(nestdata)) {
  # select a nest
  site.i <- nestdata$site[x]
  year.i <- nestdata$year[x]
  box.i <- nestdata$box[x]
  # select time window
  fed.start <- nestdata$fed[x]
  fed.end <- fed.start + 10.16
  # skip if missing fed
  if(is.na(fed.start)) next
  # find temps in time window 
  cols.use <- which(temp.times >= fed.start &
                      temp.times <= fed.end)
  # skip if no matching temps
  if(length(cols.use) == 0) next
  # find matching site-year rows
  rows.use <- which(loggerdata$site == site.i &
                      loggerdata$year == year.i)
  # skip is no matching loggers 
  if(length(rows.use) == 0) next
  # extract temp subset
  temp.sub <- loggerdata[rows.use, temp.cols[cols.use]]
  # convert to matrix
  temp.sub <- as.matrix(temp.sub)
  # calc temp logger averages for each site-year
  if(nrow(temp.sub) > 1) {
    hourly.temp <- colMeans(temp.sub, na.rm = TRUE)
  } else {
  # when just one temp logger  
    hourly.temp <- as.numeric(temp.sub)
  }
  # for time window
  selected.times <- temp.times[cols.use]
  # make into whole day not hour temps
  day.id <- floor(selected.times)
  
  # Daily minimum temperatures
  daily.min <- tapply(hourly.temp,
                      day.id,
                      min)

  # Mean of the daily minima 
  tempmeanmin_fed[x] <- mean(daily.min)
}

nestdata$tempmeanmin_fed <- tempmeanmin_fed

summary(tempmeanmin_fed)

## fki data ##
# create storage vector of NAs which get filled as loop runs 
tempmeanmin_fki <- rep(NA, nrow(nestdata))
meta.cols <- c("site","year")
temp.cols <- setdiff(names(loggerdata), meta.cols)
temp.times <- as.numeric(gsub("^X", "", temp.cols))

# make fki loop
for(x in 1:nrow(nestdata)) {
  # select a nest
  site.i <- nestdata$site[x]
  year.i <- nestdata$year[x]
  box.i <- nestdata$box[x]
  # select time window
  fki.start <- nestdata$fki[x]
  fki.end <- fki.start + 13.32
  # skip if missing fed
  if(is.na(fki.start)) next
  # find temps in time window 
  cols.use <- which(temp.times >= fki.start &
                      temp.times <= fki.end)
  # skip if no matching temps
  if(length(cols.use) == 0) next
  # find matching site-year rows
  rows.use <- which(loggerdata$site == site.i &
                      loggerdata$year == year.i)
  # skip is no matching loggers 
  if(length(rows.use) == 0) next
  # extract temp subset
  temp.sub <- loggerdata[rows.use, temp.cols[cols.use]]
  # convert to matrix
  temp.sub <- as.matrix(temp.sub)
  # calc temp logger averages for each site-year
  if(nrow(temp.sub) > 1) {
    hourly.temp <- colMeans(temp.sub, na.rm = TRUE)
  } else {
    # when just one temp logger  
    hourly.temp <- as.numeric(temp.sub)
  }
  # for time window
  selected.times <- temp.times[cols.use]
  # make into whole day not hour temps
  day.id <- floor(selected.times)
  
  # Daily minimum temperatures
  daily.min <- tapply(hourly.temp,
                      day.id,
                      min)
  # Mean of the daily minima 
  tempmeanmin_fki[x] <- mean(daily.min)
  
}

nestdata$tempmeanmin_fki <- tempmeanmin_fki

summary(tempmeanmin_fki)


## hatch data ##
# create storage vector of NAs which get filled as loop runs 
tempmeanmin_hatch <- rep(NA, nrow(nestdata))
meta.cols <- c("site","year")
temp.cols <- setdiff(names(loggerdata), meta.cols)
temp.times <- as.numeric(gsub("^X", "", temp.cols))

# make hatch loop
for(x in 1:nrow(nestdata)) {
  # select a nest
  site.i <- nestdata$site[x]
  year.i <- nestdata$year[x]
  box.i <- nestdata$box[x]
  # select time window
  hatch.start <- nestdata$hatching_first_recorded[x]
  hatch.end <- hatch.start + 18
  # skip if missing hatch
  if(is.na(hatch.start)) next
  # find temps in time window 
  cols.use <- which(temp.times >= hatch.start &
                      temp.times <= hatch.end)
  # skip if no matching temps
  if(length(cols.use) == 0) next
  # find matching site-year rows
  rows.use <- which(loggerdata$site == site.i &
                      loggerdata$year == year.i)
  # skip is no matching loggers 
  if(length(rows.use) == 0) next
  # extract temp subset
  temp.sub <- loggerdata[rows.use, temp.cols[cols.use]]
  # convert to matrix
  temp.sub <- as.matrix(temp.sub)
  # calc temp logger averages for each site-year
  if(nrow(temp.sub) > 1) {
    hourly.temp <- colMeans(temp.sub, na.rm = TRUE)
  } else {
    # when just one temp logger  
    hourly.temp <- as.numeric(temp.sub)
  }
  # for time window
  selected.times <- temp.times[cols.use]
  # make into whole day not hour temps
  day.id <- floor(selected.times)
  
  # Daily minimum temperatures
  daily.min <- tapply(hourly.temp,
                      day.id,
                      min)
  # Mean of the daily minima 
  tempmeanmin_hatch[x] <- mean(daily.min)
  
}

nestdata$tempmeanmin_hatch <- tempmeanmin_hatch

summary(tempmeanmin_hatch)

##### Making temp mean loops #####

## fed data ##
# create storage vector of NAs which get filled as loop runs 
tempmean_fed <- rep(NA, nrow(nestdata))
meta.cols <- c("site","year")
temp.cols <- setdiff(names(loggerdata), meta.cols)
temp.times <- as.numeric(gsub("^X", "", temp.cols))

# make fed loop
for(x in 1:nrow(nestdata)) {
  # select a nest
  site.i <- nestdata$site[x]
  year.i <- nestdata$year[x]
  box.i <- nestdata$box[x]
  # select time window
  fed.start <- nestdata$fed[x]
  fed.end <- fed.start + 10.16
  # skip if missing fed
  if(is.na(fed.start)) next
  # find temps in time window 
  cols.use <- which(temp.times >= fed.start &
                      temp.times <= fed.end)
  # skip if no matching temps
  if(length(cols.use) == 0) next
  # find matching site-year rows
  rows.use <- which(loggerdata$site == site.i &
                      loggerdata$year == year.i)
  # skip is no matching loggers 
  if(length(rows.use) == 0) next
  # extract temp subset
  temp.sub <- loggerdata[rows.use, temp.cols[cols.use]]
  # convert to matrix
  temp.sub <- as.matrix(temp.sub)
  # calc temp logger averages for each site-year
  if(nrow(temp.sub) > 1) {
    hourly.temp <- colMeans(temp.sub, na.rm = TRUE)
  } else {
    # when just one temp logger  
    hourly.temp <- as.numeric(temp.sub)
  }
  # for time window
  selected.times <- temp.times[cols.use]
  # make into whole day not hour temps
  day.id <- floor(selected.times)
  
  # Mean temp
  tempmean_fed[x] <- mean(hourly.temp, na.rm = TRUE)
}

nestdata$tempmean_fed <- tempmean_fed

summary(tempmean_fed)

## fki data ##
# create storage vector of NAs which get filled as loop runs 
tempmean_fki <- rep(NA, nrow(nestdata))
meta.cols <- c("site","year")
temp.cols <- setdiff(names(loggerdata), meta.cols)
temp.times <- as.numeric(gsub("^X", "", temp.cols))

# make fki loop
for(x in 1:nrow(nestdata)) {
  # select a nest
  site.i <- nestdata$site[x]
  year.i <- nestdata$year[x]
  box.i <- nestdata$box[x]
  # select time window
  fki.start <- nestdata$fki[x]
  fki.end <- fki.start + 13.32
  # skip if missing fed
  if(is.na(fki.start)) next
  # find temps in time window 
  cols.use <- which(temp.times >= fki.start &
                      temp.times <= fki.end)
  # skip if no matching temps
  if(length(cols.use) == 0) next
  # find matching site-year rows
  rows.use <- which(loggerdata$site == site.i &
                      loggerdata$year == year.i)
  # skip is no matching loggers 
  if(length(rows.use) == 0) next
  # extract temp subset
  temp.sub <- loggerdata[rows.use, temp.cols[cols.use]]
  # convert to matrix
  temp.sub <- as.matrix(temp.sub)
  # calc temp logger averages for each site-year
  if(nrow(temp.sub) > 1) {
    hourly.temp <- colMeans(temp.sub, na.rm = TRUE)
  } else {
    # when just one temp logger  
    hourly.temp <- as.numeric(temp.sub)
  }
  # for time window
  selected.times <- temp.times[cols.use]
  # make into whole day not hour temps
  day.id <- floor(selected.times)
  
  # Mean temp
  tempmean_fki[x] <- mean(hourly.temp, na.rm = TRUE)
}

nestdata$tempmean_fki <- tempmean_fki

summary(tempmean_fki)

## hatch data ##
# create storage vector of NAs which get filled as loop runs 
tempmean_hatch <- rep(NA, nrow(nestdata))
meta.cols <- c("site","year")
temp.cols <- setdiff(names(loggerdata), meta.cols)
temp.times <- as.numeric(gsub("^X", "", temp.cols))

# make hatch loop
for(x in 1:nrow(nestdata)) {
  # select a nest
  site.i <- nestdata$site[x]
  year.i <- nestdata$year[x]
  box.i <- nestdata$box[x]
  # select time window
  hatch.start <- nestdata$hatching_first_recorded[x]
  hatch.end <- hatch.start + 18
  # skip if missing fed
  if(is.na(hatch.start)) next
  # find temps in time window 
  cols.use <- which(temp.times >= hatch.start &
                      temp.times <= hatch.end)
  # skip if no matching temps
  if(length(cols.use) == 0) next
  # find matching site-year rows
  rows.use <- which(loggerdata$site == site.i &
                      loggerdata$year == year.i)
  # skip is no matching loggers 
  if(length(rows.use) == 0) next
  # extract temp subset
  temp.sub <- loggerdata[rows.use, temp.cols[cols.use]]
  # convert to matrix
  temp.sub <- as.matrix(temp.sub)
  # calc temp logger averages for each site-year
  if(nrow(temp.sub) > 1) {
    hourly.temp <- colMeans(temp.sub, na.rm = TRUE)
  } else {
    # when just one temp logger  
    hourly.temp <- as.numeric(temp.sub)
  }
  # for time window
  selected.times <- temp.times[cols.use]
  # make into whole day not hour temps
  day.id <- floor(selected.times)
  
  # Mean temp
  tempmean_hatch[x] <- mean(hourly.temp, na.rm = TRUE)
}

nestdata$tempmean_hatch <- tempmean_hatch

summary(tempmean_hatch)

# each tempmetric value corresponds to one nest
# NA in tempmetric means no fed was recorded 

# look at the mean of the min, average, max for the temperatures 
# edit for the 3 different time windows 
	

4. Then the final step is to run a statistical model with suc (# fledged) as the response and temperature as the predictor - probably as a linear and quadratic term. For this model you’ll also need to consider your random terms

#Then you can set up a linear mixed effects model with suc as the response - and the temperature metric as a predictor and some key random effects. 

# random effects = site, year, site*year, bird ID (new for unknown ID)
# model thoughts = poisson for suc (can critique as limitation later), make line graph quadratic with confidence intervals either side

  
# NICOLE MODEL ATTEMPT :D
  
##### Load datasets and refine #####

# get packages  
install.packages("Matrix")
install.packages("lme4")
library(lme4)
library(Matrix)
install.packages("dplyr")
library(dplyr)
install.packages("ggeffects")
install.packages("ggplot2")
library(ggeffects)
library(ggplot2)
install.packages("patchwork")
library(patchwork)

# add in ring dataset
adultdata <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Thesis/Data play/Adults.csv")

# make dataset only contain F in spring
adultdata<-subset(adultdata,sex=="F")
adultdata<-subset(adultdata,season=="spring")

# make things factors
adultdata$ring <- as.factor(adultdata$ring)
adultdata$site <- as.factor(adultdata$site)
adultdata$year <- as.factor(adultdata$year)
nestdata$site <- as.factor(nestdata$site)
nestdata$year <- as.factor(nestdata$year)

# make site-year interaction
nestdata$site_year <- interaction(nestdata$site, nestdata$year)

# create unique identifier for box, site, yr in each dataset then link
nestdata$ID <- paste(nestdata$site, nestdata$year, nestdata$box, sep = "_")
adultdata$ID <- paste(adultdata$site, adultdata$year, adultdata$box, sep = "_")

nestdata$female<-1:nrow(nestdata)
nestdata$female<-as.character(adultdata$ring[pmatch(nestdata$ID,adultdata$ID)])
nestdata$female[which(is.na(nestdata$female)==TRUE)]<-1:length(which(is.na(nestdata$female)==TRUE))

# filter to entries with hatching_first_recorded
hatchednestdata <- nestdata %>%
  filter(!is.na(hatching_first_recorded),
         hatching_first_recorded !="")

jointdata<-hatchednestdata

# remove any negative values
jointdata$suc[jointdata$suc < 0] <- NA

# create centred temp variable to fit ^2 better
jointdata$temp_c_hatch <- scale(jointdata$tempmeanmin_hatch, center = TRUE, scale = FALSE)
jointdata$temp_c_fki <- scale(jointdata$tempmeanmin_fki, center = TRUE, scale = FALSE)
jointdata$temp_c_fed <- scale(jointdata$tempmeanmin_fed, center = TRUE, scale = FALSE)

jointdata$temp_m_hatch <- scale(jointdata$tempmean_hatch, center = TRUE, scale = FALSE)
jointdata$temp_m_fki <- scale(jointdata$tempmean_fki, center = TRUE, scale = FALSE)
jointdata$temp_m_fed <- scale(jointdata$tempmean_fed, center = TRUE, scale = FALSE)

jointdata$temp_a_hatch <- scale(jointdata$tempmin_hatch, center = TRUE, scale = FALSE)
jointdata$temp_a_fki <- scale(jointdata$tempmin_fki, center = TRUE, scale = FALSE)
jointdata$temp_a_fed <- scale(jointdata$tempmin_fed, center = TRUE, scale = FALSE)

names(jointdata)

# make big jointdataset
# List every variable used across all models (response, all temp predictors, random effects)
vars_needed <- c(
  "suc",
  "temp_c_fed", "temp_m_fed", "temp_a_fed", "tempmean_fed", "tempmeanmin_fed", "tempmin_fed",      # fed predictors - adjust names
  "temp_c_fki", "temp_m_fki", "temp_a_fki", "tempmean_fki", "tempmeanmin_fki", "tempmin_fki",     # fki predictors - adjust names
  "temp_c_hatch", "temp_m_hatch", "temp_a_hatch", "tempmean_hatch",  "tempmeanmin_hatch", "tempmin_hatch", # hatch predictors
  "site", "year", "site_year", "female")

# Check all exist
vars_needed[!vars_needed %in% names(jointdata)]

# Create common complete-case dataset
jointdata_common <- jointdata[complete.cases(jointdata[, vars_needed]), ]

##### Making temp min models #####

## make fed model
fed_min_model <- glmer(
  suc ~ temp_a_fed + I(temp_a_fed^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_common,
  family = poisson(link = "log")
)

summary(fed_min_model)

## make fed graph

# make fed model predictions
preds <- ggpredict(fed_min_model, terms = "temp_a_fed [all]")
# convert back to real temp
temp_min_fed <- mean(jointdata_common$tempmin_fed, na.rm = TRUE)
preds$x_real_fed <- preds$x + temp_min_fed

# make fed plot
fed_min_plot <- ggplot(preds, aes(x = x_real_fed, y = predicted)) +
  geom_line(linewidth = 1, colour = "#2C7FB8") +
  coord_cartesian(xlim=c(-4,18), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(-6,18,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2C7FB8") +
  geom_rug(data = jointdata_common, aes(x = tempmin_fed), inherit.aes = FALSE, sides = "b", colour = "#2C7FB8") +
  labs(
    x = "Min temperature (°C)",
    y = "Fledging success",
    title = "A"
  ) +
  theme_classic()

fed_min_plot

## make fki model
fki_min_model <- glmer(
  suc ~ temp_a_fki + I(temp_a_fki^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_common,
  family = poisson(link = "log")
)

summary(fki_min_model)

## make fki graph

# make fki model predictions
preds <- ggpredict(fki_min_model, terms = "temp_a_fki [all]")
# convert back to real temp
temp_min_fki <- mean(jointdata_common$tempmin_fki, na.rm = TRUE)
preds$x_real_fki <- preds$x + temp_min_fki

# make fki plot
fki_min_plot <- ggplot(preds, aes(x = x_real_fki, y = predicted)) +
  geom_line(linewidth = 1, colour = "#FDD835") +
  coord_cartesian(xlim=c(-4,18), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(-6,18,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#FDD835") +
  geom_rug(data = jointdata_common, aes(x = tempmin_fki), inherit.aes = FALSE, sides = "b", colour = "#FDD835") +
  labs(
    x = "Min temperature (°C)",
    y = "Fledging success",
    title = "B"
  ) +
  theme_classic()

fki_min_plot

## make hatch model
hatch_min_model <- glmer(
  suc ~ temp_a_hatch + I(temp_a_hatch^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_common,
  family = poisson(link = "log")
)

summary(hatch_min_model)

## make hatch graph

# make fed model predictions
preds <- ggpredict(hatch_min_model, terms = "temp_a_hatch [all]")
# convert back to real temp
temp_min_hatch <- mean(jointdata_common$tempmin_hatch, na.rm = TRUE)
preds$x_real_hatch <- preds$x + temp_min_hatch

# make fed plot
hatch_min_plot <- ggplot(preds, aes(x = x_real_hatch, y = predicted)) +
  geom_line(linewidth = 1, colour = "#41AB5D") +
  coord_cartesian(xlim=c(-4,18), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(-6,18,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#41AB5D") +
  geom_rug(data = jointdata_common, aes(x = tempmin_hatch), inherit.aes = FALSE, sides = "b", colour = "#41AB5D") +
  labs(
    x = "Min temperature (°C)",
    y = "Fledging success",
    title = "C"
  ) +
  theme_classic()

hatch_min_plot

fed_min_plot + fki_min_plot + hatch_min_plot

##### Making temp mean of minima models #####

## make fed model
fed_mean_of_minima_model <- glmer(
  suc ~ temp_c_fed + I(temp_c_fed^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_common,
  family = poisson(link = "log")
)

summary(fed_mean_of_minima_model)

## make fed graph

# make fed model predictions
preds <- ggpredict(fed_mean_of_minima_model, terms = "temp_c_fed [all]")
# convert back to real temp
temp_meanmin_fed <- mean(jointdata_common$tempmeanmin_fed, na.rm = TRUE)
preds$x_real_meanmin_fed <- preds$x + temp_meanmin_fed

# make fed plot
fed_mean_of_minima_plot <- ggplot(preds, aes(x = x_real_meanmin_fed, y = predicted)) +
  geom_line(linewidth = 1, colour = "#2C7FB8") +
  coord_cartesian(xlim=c(-4,18), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(-6,18,by=2)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2C7FB8") +
  geom_rug(data = jointdata_common, aes(x = tempmeanmin_fed), inherit.aes = FALSE, sides = "b", colour = "#2C7FB8") +
  labs(
    x = "Mean of the minima temperature (°C)",
    y = "Fledging success",
    title = "D"
  ) +
  theme_classic()

fed_mean_of_minima_plot

# make fki model
fki_mean_of_minima_model <- glmer(
  suc ~ temp_c_fki + I(temp_c_fki^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_common,
  family = poisson(link = "log")
)

summary(fki_mean_of_minima_model)

# make fki graph

# make model predictions
preds <- ggpredict(fki_mean_of_minima_model, terms = "temp_c_fki [all]")
# convert back to real temp
temp_meanmin_fki <- mean(nestdata$tempmeanmin_fki, na.rm = TRUE)
preds$x_real_meanmin_fki <- preds$x + temp_meanmin_fki

# make fki plot
fki_mean_of_minima_plot <- ggplot(preds, aes(x = x_real_meanmin_fki, y = predicted)) +
  geom_line(linewidth = 1, colour = "#FDD835") +
  coord_cartesian(xlim=c(-4,18), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(-6,18,by=2)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#FDD835") +
  geom_rug(data = jointdata_common, aes(x = tempmeanmin_fki), inherit.aes = FALSE, sides = "b", colour = "#FDD835") +
  labs(
    x = "Mean of the minima temperature (°C)",
    y = "Fledging success",
    title = "E"
  ) +
  theme_classic()

fki_mean_of_minima_plot

# make hatch model
hatch_mean_of_minima_model <- glmer(
  suc ~ temp_c_hatch + I(temp_c_hatch^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_common,
  family = poisson(link = "log")
)

summary(hatch_mean_of_minima_model)

# make hatch graph

# make model predictions
preds <- ggpredict(hatch_mean_of_minima_model, terms = "temp_c_hatch [all]")
# convert back to real temp
temp_meanmin_hatch <- mean(nestdata$tempmeanmin_hatch, na.rm = TRUE)
preds$x_real_meanmin_hatch <- preds$x + temp_meanmin_hatch

# make hatch plot
hatch_mean_of_minima_plot <- ggplot(preds, aes(x = x_real_meanmin_hatch, y = predicted)) +
  geom_line(linewidth = 1, colour = "#41AB5D") +
  coord_cartesian(xlim=c(-4,18), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(-6,18,by=2)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#41AB5D") +
  geom_rug(data = jointdata_common, aes(x = tempmeanmin_hatch), inherit.aes = FALSE, sides = "b", colour = "#41AB5D") +
  labs(
    x = "Mean of the minima temperature (°C)",
    y = "Fledging success",
    title = "F"
  ) +
  theme_classic()

hatch_mean_of_minima_plot

fed_mean_of_minima_plot + fki_mean_of_minima_plot + hatch_mean_of_minima_plot

##### Making temp mean models #####

## make fed model
fed_mean_model <- glmer(
  suc ~ temp_m_fed + I(temp_m_fed^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_common,
  family = poisson(link = "log")
)

summary(fed_mean_model)

## make fed graph

# make fed model predictions
preds <- ggpredict(fed_mean_model, terms = "temp_m_fed [all]")
# convert back to real temp
temp_mean_fed <- mean(jointdata_common$tempmean_fed, na.rm = TRUE)
preds$x_real_mean_fed <- preds$x + temp_mean_fed

# make fed plot
fed_mean_plot <- ggplot(preds, aes(x = x_real_mean_fed, y = predicted)) +
  geom_line(linewidth = 1, colour = "#2C7FB8") +
  coord_cartesian(xlim=c(-4,18), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(-6,18,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2C7FB8") +
  geom_rug(data = jointdata_common, aes(x = tempmean_fed), inherit.aes = FALSE, sides = "b", colour = "#2C7FB8") +
  labs(
    x = "Mean temperature (°C)",
    y = "Fledging success",
    title = "G"
  ) +
  theme_classic()

fed_mean_plot

## make fki model
fki_mean_model <- glmer(
  suc ~ temp_m_fki + I(temp_m_fki^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_common,
  family = poisson(link = "log")
)

summary(fki_mean_model)

## make fki graph

# make fki model predictions
preds <- ggpredict(fki_mean_model, terms = "temp_m_fki [all]")
# convert back to real temp
temp_mean_fki <- mean(jointdata_common$tempmean_fki, na.rm = TRUE)
preds$x_real_mean_fki <- preds$x + temp_mean_fki

# make fki plot
fki_mean_plot <- ggplot(preds, aes(x = x_real_mean_fki, y = predicted)) +
  geom_line(linewidth = 1, colour = "#FDD835") +
  coord_cartesian(xlim=c(-4,18), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2))+
  scale_x_continuous(breaks=seq(-6,18,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#FDD835") +
  geom_rug(data = jointdata_common, aes(x = tempmean_fki), inherit.aes = FALSE, sides = "b", colour = "#FDD835") +
  labs(
    x = "Mean temperature (°C)",
    y = "Fledging success",
    title = "H"
  ) +
  theme_classic()

fki_mean_plot

## make hatch model
hatch_mean_model <- glmer(
  suc ~ temp_m_hatch + I(temp_m_hatch^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_common,
  family = poisson(link = "log")
)

summary(hatch_mean_model)

## make hatch graph

# make hatch model predictions
preds <- ggpredict(hatch_mean_model, terms = "temp_m_hatch [all]")
# convert back to real temp
temp_mean_hatch <- mean(jointdata_common$tempmean_hatch, na.rm = TRUE)
preds$x_real_mean_hatch <- preds$x + temp_mean_hatch

# make hatch plot
hatch_mean_plot <- ggplot(preds, aes(x = x_real_mean_hatch, y = predicted)) +
  geom_line(linewidth = 1, colour = "#41AB5D") +
  coord_cartesian(xlim=c(-4,18), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2))+
  scale_x_continuous(breaks=seq(-6,18,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#41AB5D") +
  geom_rug(data = jointdata_common, aes(x = tempmean_hatch), inherit.aes = FALSE, sides = "b", colour = "#41AB5D") +
  labs(
    x = "Mean temperature (°C)",
    y = "Fledging success",
    title = "I"
  ) +
  theme_classic()

hatch_mean_plot

fed_mean_plot + fki_mean_plot + hatch_mean_plot

##### Comparing temp AIC #####

AIC(fed_min_model, fed_mean_of_minima_model, fed_mean_model)
AIC(fki_min_model, fki_mean_of_minima_model, fki_mean_model)
AIC(hatch_min_model, hatch_mean_of_minima_model, hatch_mean_model)

install.packages("gt")
library(gt)

# aic for each temp
make_aic_table <- function(...) {
  models <- list(...)
  model_names <- sapply(substitute(list(...))[-1], as.character)
  aic_vals <- sapply(models, AIC)
  df <- data.frame(Model = model_names, AIC = aic_vals)
  df$ΔAIC <- df$AIC - min(df$AIC)
  df
}

fed_table   <- make_aic_table(fed_min_model, fed_mean_of_minima_model, fed_mean_model)
fki_table   <- make_aic_table(fki_min_model, fki_mean_of_minima_model, fki_mean_model)
hatch_table <- make_aic_table(hatch_min_model, hatch_mean_of_minima_model, hatch_mean_model)

# Add a grouping label and combine
fed_table$Period   <- "Egg laying"
fki_table$Period   <- "Incubation"
hatch_table$Period <- "Hatching"

combined_table <- bind_rows(fed_table, fki_table, hatch_table)
combined_table

gt(combined_table)

# compare aic for each period
best_fed   <- min(fed_table$AIC)
best_fki   <- min(fki_table$AIC)
best_hatch <- min(hatch_table$AIC)

period_comparison <- data.frame(
  Period = c("Egg laying", "Incubation", "Hatching"),
  AIC = c(best_fed, best_fki, best_hatch)
)
period_comparison$ΔAIC <- period_comparison$AIC - min(period_comparison$AIC)
period_comparison

gt(period_comparison)

##### Making a temp model with 3 variables #####

# make model (with altered optimizer to find the optimal with more iterations)
combined_model <- glmer(
  suc ~ temp_c_fed + I(temp_c_fed^2) +
    temp_c_fki + I(temp_c_fki^2) +
    temp_a_hatch + I(temp_a_hatch^2) +
    (1 | site) + 
    (1 | year) + 
    (1 | site_year) + 
    (1 | female),
  data = jointdata_common,
  family = poisson(link = "log"),
    control = glmerControl( 
      optimizer = "bobyqa",
      optCtrl = list(maxfun = 2e5)      
    )
)

summary(combined_model)

# make predictions
# for fed
pred_fed <- as.data.frame(
  ggpredict(
    combined_model,
    terms = "temp_c_fed [all]",
    condition = c(
      temp_c_fki = mean(jointdata_common$temp_c_fki, na.rm = TRUE),
      temp_a_hatch = mean(jointdata_common$temp_a_hatch, na.rm = TRUE)
    )
  )
)
# convert back to real temp
temp_meanmin_fed <- mean(jointdata_common$tempmeanmin_fed, na.rm = TRUE)
pred_fed$x_real_fed <- pred_fed$x + temp_meanmin_fed

# making combined fed model
combined_stages_fed_plot <- ggplot(pred_fed, aes(x = x_real_fed, y = predicted)) +
  geom_line(linewidth = 1, colour = "#2C7FB8") +
  geom_rug(data = jointdata_common,
           aes(x = tempmeanmin_fed),
           inherit.aes = FALSE,
           sides = "b",
           alpha = 0.3,
           colour = "#2C7FB8") +
  geom_rug(data = jointdata_common,
           aes(x = tempmeanmin_fki),
           inherit.aes = FALSE,
           sides = "b",
           alpha = 0.3,
           colour = "#FDD835") +
  geom_rug(data = jointdata_common,
           aes(x = tempmin_hatch),
           inherit.aes = FALSE,
           sides = "b",
           alpha = 0.3,
           colour = "#41AB5D") +
  coord_cartesian(xlim=c(-2,12), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(-4,18,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2C7FB8") +
  labs(
    x = "Standardised temperature (SD units)",
    y = "Fledging success",
    title = "J"
  ) +
  theme_classic()

combined_stages_fed_plot

# for fki
pred_fki <- as.data.frame(
  ggpredict(
    combined_model,
    terms = "temp_c_fki [all]",
    condition = c(
      temp_c_fed = mean(jointdata_common$temp_c_fed, na.rm = TRUE),
      temp_a_hatch = mean(jointdata_common$temp_a_hatch, na.rm = TRUE)
    )
  )
)
# convert back to real temp
temp_meanmin_fki <- mean(jointdata_common$tempmeanmin_fki, na.rm = TRUE)
pred_fki$x_real_fki <- pred_fki$x + temp_meanmin_fki

# making combined fed model
combined_stages_fki_plot <- ggplot(pred_fki, aes(x = x_real_fki, y = predicted)) +
  geom_line(linewidth = 1, colour = "#FDD835") +
  geom_rug(data = jointdata_common,
           aes(x = tempmeanmin_fed),
           inherit.aes = FALSE,
           sides = "b",
           alpha = 0.3,
           colour = "#2C7FB8") +
  geom_rug(data = jointdata_common,
           aes(x = tempmeanmin_fki),
           inherit.aes = FALSE,
           sides = "b",
           alpha = 0.3,
           colour = "#FDD835") +
  geom_rug(data = jointdata_common,
           aes(x = tempmin_hatch),
           inherit.aes = FALSE,
           sides = "b",
           alpha = 0.3,
           colour = "#41AB5D") +
  coord_cartesian(xlim=c(-2,12), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(-4,18,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#FDD835") +
  labs(
    x = "Standardised temperature (SD units)",
    y = "Fledging success",
    title = "K"
  ) +
  theme_classic()

combined_stages_fki_plot

# for hatch
pred_hatch <- as.data.frame(
  ggpredict(
    combined_model,
    terms = "temp_a_hatch [all]",
    condition = c(
      temp_c_fki = mean(jointdata_common$temp_c_fki, na.rm = TRUE),
      temp_c_fed = mean(jointdata_common$temp_c_fed, na.rm = TRUE)
    )
  )
)
# convert back to real temp
temp_min_hatch <- mean(jointdata_common$tempmin_hatch, na.rm = TRUE)
pred_hatch$x_real_hatch <- pred_hatch$x + temp_min_hatch

# making combined fed model
combined_stages_hatch_plot <- ggplot(pred_hatch, aes(x = x_real_hatch, y = predicted)) +
  geom_line(linewidth = 1, colour = "#41AB5D") +
  geom_rug(data = jointdata_common,
           aes(x = tempmeanmin_fed),
           inherit.aes = FALSE,
           sides = "b",
           alpha = 0.3,
           colour = "#2C7FB8") +
  geom_rug(data = jointdata_common,
           aes(x = tempmeanmin_fki),
           inherit.aes = FALSE,
           sides = "b",
           alpha = 0.3,
           colour = "#FDD835") +
  geom_rug(data = jointdata_common,
           aes(x = tempmin_hatch),
           inherit.aes = FALSE,
           sides = "b",
           alpha = 0.3,
           colour = "#41AB5D") +
  coord_cartesian(xlim=c(-2,12), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(-4,18,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#41AB5D") +
  labs(
    x = "Standardised temperature (SD units)",
    y = "Fledging success",
    title = "L"
  ) +
  theme_classic()

combined_stages_hatch_plot

combined_stages_fed_plot + combined_stages_fki_plot + combined_stages_hatch_plot

##### Making the big temp plot #####
a <- fed_min_plot
b <- fki_min_plot
c <- hatch_min_plot
d <- fed_mean_of_minima_plot
e <- fki_mean_of_minima_plot
f <- hatch_mean_of_minima_plot
g <- fed_mean_plot
h <- fki_mean_plot
i <- hatch_mean_plot
j <- combined_stages_fed_plot
k <- combined_stages_fki_plot
l <- combined_stages_hatch_plot

big_temp_plot <- (
  a + b + c + d +
    e + f + g + h +
    i + j + k + l
) +
  plot_layout(nrow = 4, ncol = 3)

big_temp_plot

##### Adding caterpillars in #####

# load dataset
caterpillardata <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Thesis/Data play/Branch_Beating.csv")
summary(caterpillardata)

# what problem - it was <0.01 and >0.1 values
class(caterpillardata$caterpillar_mass)
unique(caterpillardata$caterpillar_mass)[!grepl("^[0-9.]*$", unique(caterpillardata$caterpillar_mass))]

# correct for this problem
caterpillardata <- caterpillardata %>%
  mutate(caterpillar_mass = case_when(
    caterpillar_mass == "<0.01" ~ 0.005,
    caterpillar_mass == ">0.1"  ~ 0.1,
    TRUE ~ as.numeric(caterpillar_mass)
  )) 

# group caterpillars
site_year_totals <- caterpillardata %>%
  group_by(site, year) %>%
  summarise(
    n_samples = n(),
    total_caterpillar = sum(caterpillars, na.rm=TRUE),
    total_biomass = sum(caterpillar_mass, na.rm=TRUE),
    .groups = "drop"
  )

# see how many caterpillars are per site per year
summary(site_year_totals$total_caterpillar)
summary(site_year_totals$total_biomass)

# filter to site-years with enough data (median total_caterpillars is 8, median total_biomass is 0.175)
caterpillardata_filtered <- caterpillardata %>%
  semi_join(
    site_year_totals %>% filter(total_caterpillar >= 8),  
    by = c("site", "year")
  )

# make weighted mean for caterpillar biomass
caterpillar_mean_date <- caterpillardata_filtered %>%
  group_by(site, year) %>%
  summarise(
    caterpillar_date = weighted.mean(date, w = caterpillar_mass, na.rm = TRUE),
    .groups = "drop"
  )

# get hatch date data
hatch_date <- jointdata_common %>%
  group_by(site, year) %>%
  summarise(mean_hatch_date = mean(hatching_first_recorded, na.rm = TRUE),
            .groups = "drop")

# getting data into correct forms
hatch_date <- hatch_date %>%
  mutate(
    site = as.character(site),
    year = as.integer(as.character(year))
  )

jointdata_common <- jointdata_common %>%
  mutate(
    site = as.character(site),
    year = as.integer(as.character(year))
  )

site_year_totals <- site_year_totals %>%
  mutate(site = as.character(site), year = as.integer(as.character(year)))

caterpillardata <- caterpillardata %>%
  mutate(site = as.character(site), year = as.integer(as.character(year)))

caterpillar_mean_date <- caterpillar_mean_date %>%
  mutate(site = as.character(site), year = as.integer(as.character(year)))

asynchrony_data <- asynchrony_data %>%
  mutate(site = as.character(site), year = as.integer(as.character(year)))

# calc asynchrony
asynchrony_data <- hatch_date %>%
  left_join(caterpillar_mean_date, by = c("site", "year")) %>%
  mutate(
    chick_peak = mean_hatch_date + 10,
    asynchrony        = caterpillar_date - chick_peak
  )

# merge datasets
all_async <- jointdata_common %>%
  left_join(asynchrony_data %>% select(site, year, asynchrony),
            by = c("site", "year")) %>%
  filter(!is.na(asynchrony))

# remove any negative values
all_async$suc[all_async$suc < 0] <- NA

# centre and scale all_async
all_async <- all_async %>%
  mutate(asynchrony_c  = as.numeric(scale(asynchrony)),
         asynchrony_c2 = asynchrony_c^2)

##### Making caterpillar model #####
async_model <- glmer(
  suc ~ asynchrony_c + asynchrony_c2 + 
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = all_async,
  family = poisson(link = "log")
)

summary(async_model)

# make plot
pred_range <- data.frame(
  asynchrony_c = seq(min(all_async$asynchrony_c, na.rm = TRUE),
                     max(all_async$asynchrony_c, na.rm = TRUE),
                     length.out = 100)
) %>%
  mutate(asynchrony_c2 = asynchrony_c^2)

pred_range$fit <- predict(async_model, newdata = pred_range, re.form = NA, type="response")

async_plot <- ggplot(all_async, aes(x=asynchrony_c, y=suc)) +
  geom_line(data = pred_range, aes(asynchrony_c, fit), linewidth = 1) +
  geom_rug(data = all_async, aes(x = asynchrony_c), inherit.aes = FALSE, sides = "b") +
  coord_cartesian(xlim=c(-3,3), ylim=c(4,6)) +
  scale_y_continuous(breaks=seq(3,7,by=0.5)) +
  scale_x_continuous(breaks=seq(-3,3,by=1))+
  labs(
    x = "Asynchrony (standardised)", 
    y = "Fledging success",
    title = "A") +
  theme_classic()

async_plot

##### Making caterpillar and temp model #####
async_temp_model <- glmer(
  suc ~ temp_c_fed + I(temp_c_fed^2) +
    asynchrony_c + asynchrony_c2 + 
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = all_async,
  family = poisson(link = "log")
)

summary(async_temp_model)

# make plot
pred_range <- data.frame(
  asynchrony_c = seq(min(all_async$asynchrony_c, na.rm = TRUE),
                     max(all_async$asynchrony_c, na.rm = TRUE),
                     length.out = 100)
) %>%
  mutate(
    asynchrony_c2 = asynchrony_c^2,
    temp_c_fed = mean(all_async$temp_c_fed, na.rm = TRUE)
    )

pred_range$fit <- predict(
  async_temp_model, 
  newdata = pred_range, 
  re.form = NA, 
  type="response"
  )

async_temp_plot <- ggplot(all_async, aes(x=asynchrony_c, y=suc)) +
  geom_line(data = pred_range, aes(asynchrony_c, fit), linewidth = 1) +
  geom_rug(data = all_async, aes(x = asynchrony_c), inherit.aes = FALSE, sides = "b") +
  coord_cartesian(xlim=c(-3,3), ylim=c(4,6)) +
  scale_y_continuous(breaks=seq(3,7,by=0.5)) +
  scale_x_continuous(breaks=seq(-3,3,by=1))+
  labs(
    x = "Asynchrony (standardised)", 
    y = "Fledging success",
    title = "B") +
  theme_classic()

async_temp_plot

async_plot + async_temp_plot

##### Load datasets and refine #####

raindata <- read.delim("C:/Users/Swinn/Documents/EDI MASTERS/Thesis/Data play/daily_precipitation.txt", header=TRUE)
summary(raindata)
names(raindata)[1:10]

raindata[raindata$site == "EDI", c("year","X3")]

##### Making rain min loops #####

## fed data ##
# create storage vector of NAs which get filled as loop runs 
rainmin_fed <- rep(NA, nrow(nestdata))
meta.cols <- c("site","year")
rain.cols <- setdiff(names(raindata), meta.cols)
rain.times <- as.numeric(gsub("^X", "", rain.cols))

# make fed loop
for(x in 1:nrow(nestdata)) {
  # select a nest
  site.i <- nestdata$site[x]
  year.i <- nestdata$year[x]
  box.i <- nestdata$box[x]
  # select time window
  fed.start <- nestdata$fed[x]
  fed.end <- fed.start + 10.16
  # skip if missing fed
  if(is.na(fed.start)) next
  # find rain in time window 
  cols.use <- which(rain.times >= fed.start &
                      rain.times <= fed.end)
  # skip if no matching rain
  if(length(cols.use) == 0) next
  # find matching site-year rows
  rows.use <- which(raindata$site == site.i &
                      raindata$year == year.i)
  # skip is no matching loggers 
  if(length(rows.use) == 0) next
  # extract rain subset
  rain.sub <- raindata[rows.use, rain.cols[cols.use]]
  # convert to matrix
  rain.sub <- as.matrix(rain.sub)
  # calc rain logger averages for each site-year
  if(nrow(rain.sub) > 1) {
    hourly.rain <- colMeans(rain.sub, na.rm = TRUE)
  } else {
    # when just one rain logger  
    hourly.rain <- as.numeric(rain.sub)
  }
  # for time window
  selected.times <- rain.times[cols.use]
  # make into whole day not hour rain
  day.id <- floor(selected.times)
  
  # minimum rain
  rainmin_fed[x] <- min(hourly.rain, na.rm = TRUE)
}

nestdata$rainmin_fed <- rainmin_fed

summary(rainmin_fed)

## fki data ##
# create storage vector of NAs which get filled as loop runs 
rainmin_fki <- rep(NA, nrow(nestdata))
meta.cols <- c("site","year")
rain.cols <- setdiff(names(raindata), meta.cols)
rain.times <- as.numeric(gsub("^X", "", rain.cols))

# make fki loop
for(x in 1:nrow(nestdata)) {
  # select a nest
  site.i <- nestdata$site[x]
  year.i <- nestdata$year[x]
  box.i <- nestdata$box[x]
  # select time window
  fki.start <- nestdata$fki[x]
  fki.end <- fki.start + 13.32
  # skip if missing fed
  if(is.na(fki.start)) next
  # find rain in time window 
  cols.use <- which(rain.times >= fki.start &
                      rain.times <= fki.end)
  # skip if no matching rain
  if(length(cols.use) == 0) next
  # find matching site-year rows
  rows.use <- which(raindata$site == site.i &
                      raindata$year == year.i)
  # skip is no matching loggers 
  if(length(rows.use) == 0) next
  # extract rain subset
  rain.sub <- raindata[rows.use, rain.cols[cols.use]]
  # convert to matrix
  rain.sub <- as.matrix(rain.sub)
  # calc rain logger averages for each site-year
  if(nrow(rain.sub) > 1) {
    hourly.rain <- colMeans(rain.sub, na.rm = TRUE)
  } else {
    # when just one rain logger  
    hourly.rain <- as.numeric(rain.sub)
  }
  # for time window
  selected.times <- rain.times[cols.use]
  # make into whole day not hour rain
  day.id <- floor(selected.times)
  
  # minimum temperature 
  rainmin_fki[x] <- min(hourly.rain, na.rm = TRUE)
}

nestdata$rainmin_fki <- rainmin_fki

summary(rainmin_fki)

## hatch data ##
# create storage vector of NAs which get filled as loop runs 
rainmin_hatch <- rep(NA, nrow(nestdata))
meta.cols <- c("site","year")
rain.cols <- setdiff(names(raindata), meta.cols)
rain.times <- as.numeric(gsub("^X", "", rain.cols))

# make hatch loop
for(x in 1:nrow(nestdata)) {
  # select a nest
  site.i <- nestdata$site[x]
  year.i <- nestdata$year[x]
  box.i <- nestdata$box[x]
  # select time window
  hatch.start <- nestdata$hatching_first_recorded[x]
  hatch.end <- hatch.start + 18
  # skip if missing fed
  if(is.na(hatch.start)) next
  # find rain in time window 
  cols.use <- which(rain.times >= hatch.start &
                      rain.times <= hatch.end)
  # skip if no matching rain
  if(length(cols.use) == 0) next
  # find matching site-year rows
  rows.use <- which(raindata$site == site.i &
                      raindata$year == year.i)
  # skip is no matching loggers 
  if(length(rows.use) == 0) next
  # extract rain subset
  rain.sub <- raindata[rows.use, rain.cols[cols.use]]
  # convert to matrix
  rain.sub <- as.matrix(rain.sub)
  # calc rain logger averages for each site-year
  if(nrow(rain.sub) > 1) {
    hourly.rain <- colMeans(rain.sub, na.rm = TRUE)
  } else {
    # when just one rain logger  
    hourly.rain <- as.numeric(rain.sub)
  }
  # for time window
  selected.times <- rain.times[cols.use]
  # make into whole day not hour temps
  day.id <- floor(selected.times)
  
  # absolute minimum temperature 
  rainmin_hatch[x] <- min(hourly.rain, na.rm = TRUE)
}

nestdata$rainmin_hatch <- rainmin_hatch

summary(rainmin_hatch)

##### Making rain mean loops #####

## fed data ##
# create storage vector of NAs which get filled as loop runs 
rainmean_fed <- rep(NA, nrow(nestdata))
meta.cols <- c("site","year")
rain.cols <- setdiff(names(raindata), meta.cols)
rain.times <- as.numeric(gsub("^X", "", rain.cols))

# make fed loop
for(x in 1:nrow(nestdata)) {
  # select a nest
  site.i <- nestdata$site[x]
  year.i <- nestdata$year[x]
  box.i <- nestdata$box[x]
  # select time window
  fed.start <- nestdata$fed[x]
  fed.end <- fed.start + 10.16
  # skip if missing fed
  if(is.na(fed.start)) next
  # find rain in time window 
  cols.use <- which(rain.times >= fed.start &
                      rain.times <= fed.end)
  # skip if no matching rain
  if(length(cols.use) == 0) next
  # find matching site-year rows
  rows.use <- which(raindata$site == site.i &
                      raindata$year == year.i)
  # skip is no matching loggers 
  if(length(rows.use) == 0) next
  # extract rain subset
  rain.sub <- raindata[rows.use, rain.cols[cols.use]]
  # convert to matrix
  rain.sub <- as.matrix(rain.sub)
  # calc rain averages for each site-year
  if(nrow(rain.sub) > 1) {
    hourly.rain <- colMeans(rain.sub, na.rm = TRUE)
  } else {
    # when just one temp logger  
    hourly.rain <- as.numeric(rain.sub)
  }
  # for time window
  selected.times <- rain.times[cols.use]
  # make into whole day not hour temps
  day.id <- floor(selected.times)
  
  # Mean rain
  rainmean_fed[x] <- mean(hourly.rain, na.rm = TRUE)
}

nestdata$rainmean_fed <- rainmean_fed

summary(rainmean_fed)

## fki data ##
# create storage vector of NAs which get filled as loop runs 
rainmean_fki <- rep(NA, nrow(nestdata))
meta.cols <- c("site","year")
rain.cols <- setdiff(names(raindata), meta.cols)
rain.times <- as.numeric(gsub("^X", "", rain.cols))

# make fki loop
for(x in 1:nrow(nestdata)) {
  # select a nest
  site.i <- nestdata$site[x]
  year.i <- nestdata$year[x]
  box.i <- nestdata$box[x]
  # select time window
  fki.start <- nestdata$fki[x]
  fki.end <- fki.start + 13.32
  # skip if missing fed
  if(is.na(fki.start)) next
  # find rain in time window 
  cols.use <- which(rain.times >= fki.start &
                      rain.times <= fki.end)
  # skip if no matching rain
  if(length(cols.use) == 0) next
  # find matching site-year rows
  rows.use <- which(raindata$site == site.i &
                      raindata$year == year.i)
  # skip is no matching loggers 
  if(length(rows.use) == 0) next
  # extract rain subset
  rain.sub <- raindata[rows.use, rain.cols[cols.use]]
  # convert to matrix
  rain.sub <- as.matrix(rain.sub)
  # calc rain logger averages for each site-year
  if(nrow(rain.sub) > 1) {
    hourly.rain <- colMeans(rain.sub, na.rm = TRUE)
  } else {
    # when just one rain logger  
    hourly.rain <- as.numeric(rain.sub)
  }
  # for time window
  selected.times <- rain.times[cols.use]
  # make into whole day not hour temps
  day.id <- floor(selected.times)

  # Mean rain
  rainmean_fki[x] <- mean(hourly.rain, na.rm = TRUE)
}

nestdata$rainmean_fki <- rainmean_fki

summary(rainmean_fki)

## hatch data ##
# create storage vector of NAs which get filled as loop runs 
rainmean_hatch <- rep(NA, nrow(nestdata))
meta.cols <- c("site","year")
rain.cols <- setdiff(names(raindata), meta.cols)
rain.times <- as.numeric(gsub("^X", "", rain.cols))

# make hatch loop
for(x in 1:nrow(nestdata)) {
  # select a nest
  site.i <- nestdata$site[x]
  year.i <- nestdata$year[x]
  box.i <- nestdata$box[x]
  # select time window
  hatch.start <- nestdata$hatching_first_recorded[x]
  hatch.end <- hatch.start + 18
  # skip if missing fed
  if(is.na(hatch.start)) next
  # find rain in time window 
  cols.use <- which(rain.times >= hatch.start &
                      rain.times <= hatch.end)
  # skip if no matching rain
  if(length(cols.use) == 0) next
  # find matching site-year rows
  rows.use <- which(raindata$site == site.i &
                      raindata$year == year.i)
  # skip is no matching loggers 
  if(length(rows.use) == 0) next
  # extract rain subset
  rain.sub <- raindata[rows.use, rain.cols[cols.use]]
  # convert to matrix
  rain.sub <- as.matrix(rain.sub)
  # calc rain logger averages for each site-year
  if(nrow(rain.sub) > 1) {
    hourly.rain <- colMeans(rain.sub, na.rm = TRUE)
  } else {
    # when just one rain logger  
    hourly.rain <- as.numeric(rain.sub)
  }
  # for time window
  selected.times <- rain.times[cols.use]
  # make into whole day not hour temps
  day.id <- floor(selected.times)
  
  # Mean temp
  rainmean_hatch[x] <- mean(hourly.rain, na.rm = TRUE)
}

nestdata$rainmean_hatch <- rainmean_hatch

summary(rainmean_hatch)

##### Making rain max loops #####

## fed data ##
# create storage vector of NAs which get filled as loop runs 
rainmax_fed <- rep(NA, nrow(nestdata))
meta.cols <- c("site","year")
rain.cols <- setdiff(names(raindata), meta.cols)
rain.times <- as.numeric(gsub("^X", "", rain.cols))

# make fed loop
for(x in 1:nrow(nestdata)) {
  # select a nest
  site.i <- nestdata$site[x]
  year.i <- nestdata$year[x]
  box.i <- nestdata$box[x]
  # select time window
  fed.start <- nestdata$fed[x]
  fed.end <- fed.start + 10.16
  # skip if missing fed
  if(is.na(fed.start)) next
  # find rain in time window 
  cols.use <- which(rain.times >= fed.start &
                      rain.times <= fed.end)
  # skip if no matching rain
  if(length(cols.use) == 0) next
  # find matching site-year rows
  rows.use <- which(raindata$site == site.i &
                      raindata$year == year.i)
  # skip is no matching loggers 
  if(length(rows.use) == 0) next
  # extract rain subset
  rain.sub <- raindata[rows.use, rain.cols[cols.use]]
  # convert to matrix
  rain.sub <- as.matrix(rain.sub)
  # calc rain logger averages for each site-year
  if(nrow(rain.sub) > 1) {
    hourly.rain <- colMeans(rain.sub, na.rm = TRUE)
  } else {
    # when just one rain logger  
    hourly.rain <- as.numeric(rain.sub)
  }
  # for time window
  selected.times <- rain.times[cols.use]
  # make into whole day not hour rain
  day.id <- floor(selected.times)
  
  # minimum rain
  rainmax_fed[x] <- max(hourly.rain, na.rm = TRUE)
}

nestdata$rainmax_fed <- rainmax_fed

summary(rainmax_fed)

## fki data ##
# create storage vector of NAs which get filled as loop runs 
rainmax_fki <- rep(NA, nrow(nestdata))
meta.cols <- c("site","year")
rain.cols <- setdiff(names(raindata), meta.cols)
rain.times <- as.numeric(gsub("^X", "", rain.cols))

# make fki loop
for(x in 1:nrow(nestdata)) {
  # select a nest
  site.i <- nestdata$site[x]
  year.i <- nestdata$year[x]
  box.i <- nestdata$box[x]
  # select time window
  fki.start <- nestdata$fki[x]
  fki.end <- fki.start + 13.32
  # skip if missing fed
  if(is.na(fki.start)) next
  # find rain in time window 
  cols.use <- which(rain.times >= fki.start &
                      rain.times <= fki.end)
  # skip if no matching rain
  if(length(cols.use) == 0) next
  # find matching site-year rows
  rows.use <- which(raindata$site == site.i &
                      raindata$year == year.i)
  # skip is no matching loggers 
  if(length(rows.use) == 0) next
  # extract rain subset
  rain.sub <- raindata[rows.use, rain.cols[cols.use]]
  # convert to matrix
  rain.sub <- as.matrix(rain.sub)
  # calc rain logger averages for each site-year
  if(nrow(rain.sub) > 1) {
    hourly.rain <- colMeans(rain.sub, na.rm = TRUE)
  } else {
    # when just one rain logger  
    hourly.rain <- as.numeric(rain.sub)
  }
  # for time window
  selected.times <- rain.times[cols.use]
  # make into whole day not hour rain
  day.id <- floor(selected.times)
  
  # minimum temperature 
  rainmax_fki[x] <- max(hourly.rain, na.rm = TRUE)
}

nestdata$rainmax_fki <- rainmax_fki

summary(rainmax_fki)

## hatch data ##
# create storage vector of NAs which get filled as loop runs 
rainmax_hatch <- rep(NA, nrow(nestdata))
meta.cols <- c("site","year")
rain.cols <- setdiff(names(raindata), meta.cols)
rain.times <- as.numeric(gsub("^X", "", rain.cols))

# make hatch loop
for(x in 1:nrow(nestdata)) {
  # select a nest
  site.i <- nestdata$site[x]
  year.i <- nestdata$year[x]
  box.i <- nestdata$box[x]
  # select time window
  hatch.start <- nestdata$hatching_first_recorded[x]
  hatch.end <- hatch.start + 18
  # skip if missing fed
  if(is.na(hatch.start)) next
  # find rain in time window 
  cols.use <- which(rain.times >= hatch.start &
                      rain.times <= hatch.end)
  # skip if no matching rain
  if(length(cols.use) == 0) next
  # find matching site-year rows
  rows.use <- which(raindata$site == site.i &
                      raindata$year == year.i)
  # skip is no matching loggers 
  if(length(rows.use) == 0) next
  # extract rain subset
  rain.sub <- raindata[rows.use, rain.cols[cols.use]]
  # convert to matrix
  rain.sub <- as.matrix(rain.sub)
  # calc rain logger averages for each site-year
  if(nrow(rain.sub) > 1) {
    hourly.rain <- colMeans(rain.sub, na.rm = TRUE)
  } else {
    # when just one rain logger  
    hourly.rain <- as.numeric(rain.sub)
  }
  # for time window
  selected.times <- rain.times[cols.use]
  # make into whole day not hour temps
  day.id <- floor(selected.times)
  
  # absolute minimum temperature 
  rainmax_hatch[x] <- max(hourly.rain, na.rm = TRUE)
}

nestdata$rainmax_hatch <- rainmax_hatch

summary(rainmax_hatch)

##### Load datasets and refine #####

# add in ring dataset
adultdata <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Thesis/Data play/Adults.csv")

# make dataset only contain F in spring
adultdata<-subset(adultdata,sex=="F")
adultdata<-subset(adultdata,season=="spring")

# make things factors
adultdata$ring <- as.factor(adultdata$ring)
adultdata$site <- as.factor(adultdata$site)
adultdata$year <- as.factor(adultdata$year)
nestdata$site <- as.factor(nestdata$site)
nestdata$year <- as.factor(nestdata$year)

# make site-year interaction
nestdata$site_year <- interaction(nestdata$site, nestdata$year)

# create unique identifier for box, site, yr in each dataset then link
nestdata$ID <- paste(nestdata$site, nestdata$year, nestdata$box, sep = "_")
adultdata$ID <- paste(adultdata$site, adultdata$year, adultdata$box, sep = "_")

nestdata$female<-1:nrow(nestdata)
nestdata$female<-as.character(adultdata$ring[pmatch(nestdata$ID,adultdata$ID)])
nestdata$female[which(is.na(nestdata$female)==TRUE)]<-1:length(which(is.na(nestdata$female)==TRUE))

# filter to entries with hatching_first_recorded
hatchednestdata <- nestdata %>%
  filter(!is.na(hatching_first_recorded),
         hatching_first_recorded !="")

jointdata<-hatchednestdata

# remove any negative values
jointdata$suc[jointdata$suc < 0] <- NA

# create centred rain variable to fit ^2 better
jointdata$temp_c_hatch <- scale(jointdata$tempmeanmin_hatch, center = TRUE, scale = FALSE)
jointdata$temp_c_fki <- scale(jointdata$tempmeanmin_fki, center = TRUE, scale = FALSE)
jointdata$temp_c_fed <- scale(jointdata$tempmeanmin_fed, center = TRUE, scale = FALSE)

jointdata$temp_m_hatch <- scale(jointdata$tempmean_hatch, center = TRUE, scale = FALSE)
jointdata$temp_m_fki <- scale(jointdata$tempmean_fki, center = TRUE, scale = FALSE)
jointdata$temp_m_fed <- scale(jointdata$tempmean_fed, center = TRUE, scale = FALSE)

jointdata$temp_a_hatch <- scale(jointdata$tempmin_hatch, center = TRUE, scale = FALSE)
jointdata$temp_a_fki <- scale(jointdata$tempmin_fki, center = TRUE, scale = FALSE)
jointdata$temp_a_fed <- scale(jointdata$tempmin_fed, center = TRUE, scale = FALSE)

jointdata$rain_a_hatch <- scale(jointdata$rainmin_hatch, center = TRUE, scale = FALSE)
jointdata$rain_a_fki <- scale(jointdata$rainmin_fki, center = TRUE, scale = FALSE)
jointdata$rain_a_fed <- scale(jointdata$rainmin_fed, center = TRUE, scale = FALSE)

jointdata$rain_b_hatch <- scale(jointdata$rainmean_hatch, center = TRUE, scale = FALSE)
jointdata$rain_b_fki <- scale(jointdata$rainmean_fki, center = TRUE, scale = FALSE)
jointdata$rain_b_fed <- scale(jointdata$rainmean_fed, center = TRUE, scale = FALSE)

jointdata$rain_c_hatch <- scale(jointdata$rainmax_hatch, center = TRUE, scale = FALSE)
jointdata$rain_c_fki <- scale(jointdata$rainmax_fki, center = TRUE, scale = FALSE)
jointdata$rain_c_fed <- scale(jointdata$rainmax_fed, center = TRUE, scale = FALSE)

names(jointdata)

# make big jointdataset
# List every variable used across all models (response, all temp predictors, random effects)
vars_needed <- c(
  "suc",
  "temp_c_fed", "temp_m_fed", "temp_a_fed", "tempmean_fed", "tempmeanmin_fed", "tempmin_fed",      # fed predictors - adjust names
  "temp_c_fki", "temp_m_fki", "temp_a_fki", "tempmean_fki", "tempmeanmin_fki", "tempmin_fki",     # fki predictors - adjust names
  "temp_c_hatch", "temp_m_hatch", "temp_a_hatch", "tempmean_hatch",  "tempmeanmin_hatch", "tempmin_hatch", # hatch predictors
  "rain_a_fed", "rain_b_fed", "rain_c_fed", "rainmean_fed", "rainmin_fed", "rainmax_fed",
  "rain_a_fki", "rain_b_fki", "rain_c_fki", "rainmean_fki", "rainmin_fki", "rainmax_fed",
  "rain_a_hatch", "rain_b_hatch", "rain_c_hatch", "rainmean_hatch", "rainmin_hatch", "rainmax_hatch",
  "site", "year", "site_year", "female")

# Check all exist
vars_needed[!vars_needed %in% names(jointdata)]

# Create common complete-case dataset
jointdata_common <- jointdata[complete.cases(jointdata[, vars_needed]), ]

##### Making rain min models #####

## make fed model
fed_rain_min_model <- glmer(
  suc ~ rain_a_fed + I(rain_a_fed^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_common,
  family = poisson(link = "log")
)

summary(fed_rain_min_model)

## make fed graph

# make fed model predictions
preds <- ggpredict(fed_rain_min_model, terms = "rain_a_fed [all]")
# convert back to real temp
temp_rain_min_fed <- mean(jointdata_common$rainmin_fed, na.rm = TRUE)
preds$x_real_rain_fed <- preds$x + temp_rain_min_fed

# make fed plot
fed_rain_min_plot <- ggplot(preds, aes(x = x_real_rain_fed, y = predicted)) +
  geom_line(linewidth = 1, colour = "#2C7FB8") +
  coord_cartesian(xlim=c(0,0.7), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(0,10,by=0.2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2C7FB8") +
  geom_rug(data = jointdata_common, aes(x = rainmin_fed), inherit.aes = FALSE, sides = "b", colour = "#2C7FB8") +
  labs(
    x = "Min rainfall (mm)",
    y = "Fledging success",
    title = "A"
  ) +
  theme_classic()

fed_rain_min_plot

## make fki model
# usee poly(rain_a_fki,2) to remove the issue with large eigenvalues when things have already been scaled
fki_rain_min_model <- glmer(
  suc ~ poly(rain_a_fki,2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_common,
  family = poisson(link = "log")
)

summary(fki_rain_min_model)

## make fki graph

# make fki model predictions
preds <- ggpredict(fki_rain_min_model, terms = "rain_a_fki [all]")
# convert back to real temp
temp_rain_min_fki <- mean(jointdata_common$rainmin_fki, na.rm = TRUE)
preds$x_real_rain_fki <- preds$x + temp_rain_min_fki

# make fki plot
fki_rain_min_plot <- ggplot(preds, aes(x = x_real_rain_fki, y = predicted)) +
  geom_line(linewidth = 1, colour = "#FDD835") +
  coord_cartesian(xlim=c(0,0.2), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(0,10,by=0.1))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#FDD835") +
  geom_rug(data = jointdata_common, aes(x = rainmin_fki), inherit.aes = FALSE, sides = "b", colour = "#FDD835") +
  labs(
    x = "Min rainfall (mm)",
    y = "Fledging success",
    title = "B"
  ) +
  theme_classic()

fki_rain_min_plot

## make hatch model
hatch_rain_min_model <- glmer(
  suc ~ poly(rain_a_hatch,2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_common,
  family = poisson(link = "log")
)

summary(hatch_rain_min_model) 

## make hatch graph

# make fed model predictions
preds <- ggpredict(hatch_rain_min_model, terms = "rain_a_hatch [all]")
# convert back to real temp
rain_min_hatch <- mean(jointdata_common$rainmin_hatch, na.rm = TRUE)
preds$x_real_rain_hatch <- preds$x + rain_min_hatch

# make fed plot
hatch_rain_min_plot <- ggplot(preds, aes(x = x_real_rain_hatch, y = predicted)) +
  geom_line(linewidth = 1, colour = "#41AB5D") +
  coord_cartesian(xlim=c(0,0.2), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(-6,18,by=0.1))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#41AB5D") +
  geom_rug(data = jointdata_common, aes(x = rainmin_hatch), inherit.aes = FALSE, sides = "b", colour = "#41AB5D") +
  labs(
    x = "Min rainfall (mm)",
    y = "Fledging success",
    title = "C"
  ) +
  theme_classic()

hatch_rain_min_plot

fed_rain_min_plot + fki_rain_min_plot + hatch_rain_min_plot

##### Making rain mean models #####

## make fed model
fed_rain_mean_model <- glmer(
  suc ~ rain_b_fed + I(rain_b_fed^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_common,
  family = poisson(link = "log")
)

summary(fed_mean_model)

## make fed graph

# make fed model predictions
preds <- ggpredict(fed_rain_mean_model, terms = "rain_b_fed [all]")
# convert back to real rain
rain_mean_fed <- mean(jointdata_common$rainmean_fed, na.rm = TRUE)
preds$x_real_rain_mean_fed <- preds$x + rain_mean_fed

# make fed plot
fed_rain_mean_plot <- ggplot(preds, aes(x = x_real_rain_mean_fed, y = predicted)) +
  geom_line(linewidth = 1, colour = "#2C7FB8") +
  coord_cartesian(xlim=c(0,10), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(-6,18,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2C7FB8") +
  geom_rug(data = jointdata_common, aes(x = rainmean_fed), inherit.aes = FALSE, sides = "b", colour = "#2C7FB8") +
  labs(
    x = "Mean rainfall (mm)",
    y = "Fledging success",
    title = "D"
  ) +
  theme_classic()

fed_rain_mean_plot

## make fki model
fki_rain_mean_model <- glmer(
  suc ~ rain_b_fki + I(rain_b_fki^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_common,
  family = poisson(link = "log")
)

summary(fki_rain_mean_model)

## make fki graph

# make fki model predictions
preds <- ggpredict(fki_rain_mean_model, terms = "rain_b_fki [all]")
# convert back to real temp
rain_mean_fki <- mean(jointdata_common$rainmean_fki, na.rm = TRUE)
preds$x_real_rain_mean_fki <- preds$x + rain_mean_fki

# make fki plot
fki_rain_mean_plot <- ggplot(preds, aes(x = x_real_rain_mean_fki, y = predicted)) +
  geom_line(linewidth = 1, colour = "#FDD835") +
  coord_cartesian(xlim=c(0,10), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2))+
  scale_x_continuous(breaks=seq(-6,18,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#FDD835") +
  geom_rug(data = jointdata_common, aes(x = rainmean_fki), inherit.aes = FALSE, sides = "b", colour = "#FDD835") +
  labs(
    x = "Mean rainfall (mm)",
    y = "Fledging success",
    title = "E"
  ) +
  theme_classic()

fki_rain_mean_plot

## make hatch model
hatch_rain_mean_model <- glmer(
  suc ~ rain_b_hatch + I(rain_b_hatch^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_common,
  family = poisson(link = "log")
)

summary(hatch_rain_mean_model)

## make hatch graph

# make hatch model predictions
preds <- ggpredict(hatch_rain_mean_model, terms = "rain_b_hatch [all]")
# convert back to real temp
rain_mean_hatch <- mean(jointdata_common$rainmean_hatch, na.rm = TRUE)
preds$x_real_rain_mean_hatch <- preds$x + rain_mean_hatch

# make hatch plot
hatch_rain_mean_plot <- ggplot(preds, aes(x = x_real_rain_mean_hatch, y = predicted)) +
  geom_line(linewidth = 1, colour = "#41AB5D") +
  coord_cartesian(xlim=c(0,10), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2))+
  scale_x_continuous(breaks=seq(-6,18,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#41AB5D") +
  geom_rug(data = jointdata_common, aes(x = rainmean_hatch), inherit.aes = FALSE, sides = "b", colour = "#41AB5D") +
  labs(
    x = "Mean rainfall (mm)",
    y = "Fledging success",
    title = "F"
  ) +
  theme_classic()

hatch_rain_mean_plot

fed_rain_mean_plot + fki_rain_mean_plot + hatch_rain_mean_plot

##### Making rain max models #####

## make fed model 
fed_rain_max_model <- glmer(
  suc ~ poly(rain_c_fed,2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_common,
  family = poisson(link = "log")
)

summary(fed_rain_max_model)

## make fed graph

# make fed model predictions
preds <- ggpredict(fed_rain_max_model, terms = "rain_c_fed [all]")
# convert back to real rain
rain_max_fed <- mean(jointdata_common$rainmax_fed, na.rm = TRUE)
preds$x_real_rain_max_fed <- preds$x + rain_max_fed

# make fed plot
fed_rain_max_plot <- ggplot(preds, aes(x = x_real_rain_max_fed, y = predicted)) +
  geom_line(linewidth = 1, colour = "#2C7FB8") +
  coord_cartesian(xlim=c(0,40), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(0,50,by=10))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2C7FB8") +
  geom_rug(data = jointdata_common, aes(x = rainmax_fed), inherit.aes = FALSE, sides = "b", colour = "#2C7FB8") +
  labs(
    x = "Max rainfall (mm)",
    y = "Fledging success",
    title = "G"
  ) +
  theme_classic()

fed_rain_max_plot

## make fki model  
fki_rain_max_model <- glmer(
  suc ~ poly(rain_c_fki,2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_common,
  family = poisson(link = "log")
)

summary(fki_rain_max_model)

## make fki graph

# make fki model predictions
preds <- ggpredict(fki_rain_max_model, terms = "rain_c_fki [all]")
# convert back to real temp
rain_max_fki <- mean(jointdata_common$rainmax_fki, na.rm = TRUE)
preds$x_real_rain_max_fki <- preds$x + rain_max_fki

# make fki plot
fki_rain_max_plot <- ggplot(preds, aes(x = x_real_rain_max_fki, y = predicted)) +
  geom_line(linewidth = 1, colour = "#FDD835") +
  coord_cartesian(xlim=c(0,60), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2))+
  scale_x_continuous(breaks=seq(0,70,by=10))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#FDD835") +
  geom_rug(data = jointdata_common, aes(x = rainmax_fki), inherit.aes = FALSE, sides = "b", colour = "#FDD835") +
  labs(
    x = "Max rainfall (mm)",
    y = "Fledging success",
    title = "H"
  ) +
  theme_classic()

fki_rain_max_plot

## make hatch model 
hatch_rain_max_model <- glmer(
  suc ~ poly(rain_c_hatch,2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_common,
  family = poisson(link = "log")
)

summary(hatch_rain_max_model)

## make hatch graph

# make hatch model predictions
preds <- ggpredict(hatch_rain_max_model, terms = "rain_c_hatch [all]")
# convert back to real temp
rain_max_hatch <- mean(jointdata_common$rainmax_hatch, na.rm = TRUE)
preds$x_real_rain_max_hatch <- preds$x + rain_max_hatch

# make hatch plot
hatch_rain_max_plot <- ggplot(preds, aes(x = x_real_rain_max_hatch, y = predicted)) +
  geom_line(linewidth = 1, colour = "#41AB5D") +
  coord_cartesian(xlim=c(0,74), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2))+
  scale_x_continuous(breaks=seq(0,80,by=10))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#41AB5D") +
  geom_rug(data = jointdata_common, aes(x = rainmax_hatch), inherit.aes = FALSE, sides = "b", colour = "#41AB5D") +
  labs(
    x = "Max rainfall (mm)",
    y = "Fledging success",
    title = "I"
  ) +
  theme_classic()

hatch_rain_max_plot

fed_rain_max_plot + fki_rain_max_plot + hatch_rain_max_plot

##### Making rain model with 3 variables #####

# make model (with altered optimizer to find the optimal with more iterations)
combined_model <- glmer(
  suc ~ rain_b_fed + I(rain_b_fed^2) +
    poly(rain_c_fki,2) +
    poly(rain_c_hatch,2) +
    (1 | site) + 
    (1 | year) + 
    (1 | site_year) + 
    (1 | female),
  data = jointdata_common,
  family = poisson(link = "log"),
  control = glmerControl( 
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)      
  )
)

summary(combined_model)

# make predictions
# for fed
pred_fed <- as.data.frame(
  ggpredict(
    combined_model,
    terms = "rain_b_fed [all]",
    condition = c(
      rain_c_fki = mean(jointdata_common$rain_c_fki, na.rm = TRUE),
      rain_c_hatch = mean(jointdata_common$rain_c_hatch, na.rm = TRUE)
    )
  )
)
# convert back to real temp
rain_mean_fed <- mean(jointdata_common$rainmean_fed, na.rm = TRUE)
pred_fed$x_real_rain_fed <- pred_fed$x + rain_mean_fed

# making combined fed model
combined_stages_rain_fed_plot <- ggplot(pred_fed, aes(x = x_real_rain_fed, y = predicted)) +
  geom_line(linewidth = 1, colour = "#2C7FB8") +
  geom_rug(data = jointdata_common,
           aes(x = rainmean_fed),
           inherit.aes = FALSE,
           sides = "b",
           alpha = 0.3,
           colour = "#2C7FB8") +
  geom_rug(data = jointdata_common,
           aes(x = rainmax_fki),
           inherit.aes = FALSE,
           sides = "b",
           alpha = 0.3,
           colour = "#FDD835") +
  geom_rug(data = jointdata_common,
           aes(x = rainmax_hatch),
           inherit.aes = FALSE,
           sides = "b",
           alpha = 0.3,
           colour = "#41AB5D") +
  coord_cartesian(xlim=c(0,10), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(-4,18,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2C7FB8") +
  labs(
    x = "Standardised rainfall (SD units)",
    y = "Fledging success",
    title = "J"
  ) +
  theme_classic()

combined_stages_rain_fed_plot

# for fki
pred_fki <- as.data.frame(
  ggpredict(
    combined_model,
    terms = "rain_c_fki [all]",
    condition = c(
      rain_b_fed = mean(jointdata_common$rain_b_fed, na.rm = TRUE),
      rain_c_hatch = mean(jointdata_common$rain_c_hatch, na.rm = TRUE)
    )
  )
)
# convert back to real temp
rain_max_fki <- mean(jointdata_common$rainmax_fki, na.rm = TRUE)
pred_fki$x_real_rain_fki <- pred_fki$x + rain_max_fki

# making combined fed model
combined_stages_rain_fki_plot <- ggplot(pred_fki, aes(x = x_real_rain_fki, y = predicted)) +
  geom_line(linewidth = 1, colour = "#FDD835") +
  geom_rug(data = jointdata_common,
           aes(x = rainmean_fed),
           inherit.aes = FALSE,
           sides = "b",
           alpha = 0.3,
           colour = "#2C7FB8") +
  geom_rug(data = jointdata_common,
           aes(x = rainmax_fki),
           inherit.aes = FALSE,
           sides = "b",
           alpha = 0.3,
           colour = "#FDD835") +
  geom_rug(data = jointdata_common,
           aes(x = rainmax_hatch),
           inherit.aes = FALSE,
           sides = "b",
           alpha = 0.3,
           colour = "#41AB5D") +
  coord_cartesian(xlim=c(0,60), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(0,60,by=10))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#FDD835") +
  labs(
    x = "Standardised rainfall (SD units)",
    y = "Fledging success",
    title = "K"
  ) +
  theme_classic()

combined_stages_rain_fki_plot

# for hatch
pred_hatch <- as.data.frame(
  ggpredict(
    combined_model,
    terms = "rain_c_hatch [all]",
    condition = c(
      rain_c_fki = mean(jointdata_common$rain_c_fki, na.rm = TRUE),
      rain_b_fed = mean(jointdata_common$rain_b_fed, na.rm = TRUE)
    )
  )
)
# convert back to real temp
rain_max_hatch <- mean(jointdata_common$rainmax_hatch, na.rm = TRUE)
pred_hatch$x_real_rain_hatch <- pred_hatch$x + rain_max_hatch

# making combined fed model
combined_stages_rain_hatch_plot <- ggplot(pred_hatch, aes(x = x_real_rain_hatch, y = predicted)) +
  geom_line(linewidth = 1, colour = "#41AB5D") +
  geom_rug(data = jointdata_common,
           aes(x = rainmean_fed),
           inherit.aes = FALSE,
           sides = "b",
           alpha = 0.3,
           colour = "#2C7FB8") +
  geom_rug(data = jointdata_common,
           aes(x = rainmax_fki),
           inherit.aes = FALSE,
           sides = "b",
           alpha = 0.3,
           colour = "#FDD835") +
  geom_rug(data = jointdata_common,
           aes(x = rainmax_hatch),
           inherit.aes = FALSE,
           sides = "b",
           alpha = 0.3,
           colour = "#41AB5D") +
  coord_cartesian(xlim=c(0,74), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(0,70,by=10))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#41AB5D") +
  labs(
    x = "Standardised rainfall (SD units)",
    y = "Fledging success",
    title = "L"
  ) +
  theme_classic()

combined_stages_rain_hatch_plot

combined_stages_rain_fed_plot + combined_stages_rain_fki_plot + combined_stages_rain_hatch_plot

##### Making the big rain plot #####
a <- fed_rain_min_plot
b <- fki_rain_min_plot
c <- hatch_rain_min_plot
d <- fed_rain_mean_plot
e <- fki_rain_mean_plot
f <- hatch_rain_mean_plot
g <- fed_rain_max_plot
h <- fki_rain_max_plot
i <- hatch_rain_max_plot
j <- combined_stages_rain_fed_plot
k <- combined_stages_rain_fki_plot
l <- combined_stages_rain_hatch_plot

big_rain_plot <- (
  a + b + c + d + e + f + g + h + i + j + k + l
) +
  plot_layout(nrow = 4, ncol = 3)

big_rain_plot

##### Comparing rain AIC #####

AIC(fed_rain_min_model, fed_rain_mean_model, fed_rain_max_model)
AIC(fki_rain_min_model, fki_rain_mean_model, fki_rain_max_model)
AIC(hatch_rain_min_model, hatch_rain_mean_model, hatch_rain_max_model)

library(gt)

# aic for each temp
make_aic_table <- function(...) {
  models <- list(...)
  model_names <- sapply(substitute(list(...))[-1], as.character)
  aic_vals <- sapply(models, AIC)
  df <- data.frame(Model = model_names, AIC = aic_vals)
  df$ΔAIC <- df$AIC - min(df$AIC)
  df
}

fed_rain_table   <- make_aic_table(fed_rain_min_model, fed_rain_mean_model, fed_rain_max_model)
fki_rain_table   <- make_aic_table(fki_rain_min_model, fki_rain_mean_model, fki_rain_max_model)
hatch_rain_table <- make_aic_table(hatch_rain_min_model, hatch_rain_mean_model, hatch_rain_max_model)

# Add a grouping label and combine
fed_rain_table$Period   <- "Egg laying"
fki_rain_table$Period   <- "Incubation"
hatch_rain_table$Period <- "Hatching"

combined_rain_table <- bind_rows(fed_rain_table, fki_rain_table, hatch_rain_table)
combined_rain_table

gt(combined_rain_table)

# compare aic for each period
best_rain_fed   <- min(fed_rain_table$AIC)
best_rain_fki   <- min(fki_rain_table$AIC)
best_rain_hatch <- min(hatch_rain_table$AIC)

period_rain_comparison <- data.frame(
  Period = c("Egg laying", "Incubation", "Hatching"),
  AIC = c(best_rain_fed, best_rain_fki, best_rain_hatch)
)
period_rain_comparison$ΔAIC <- period_rain_comparison$AIC - min(period_rain_comparison$AIC)
period_rain_comparison

gt(period_rain_comparison)

