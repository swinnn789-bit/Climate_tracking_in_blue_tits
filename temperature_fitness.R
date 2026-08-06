1. You may want to filter the birdphenology data to only focus on cases where the species is bluti, and there is data for FED and FKI and the visit frequency column doesn’t have text in it - this means that you’d be focusing on data where we know the first egg-date precisely.

#you already have a dataset called filteredpheno2

2) You could work out what the average duration is between first egg date and first known incubation. Then for each nestbox the focal time period is the period between FED and the end of this period.

#The average duration from fed to fki is 10.16

#The average duration from fki to hatching_first_recorded is 13.32 

#The average duration from hatching_first_recorded to fledging is 18

pheno <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Thesis/Data play/Bird_Phenology.csv")

# filtering
filterpheno <- pheno %>%
  filter(
    species == "bluti",
    !is.na(fed),
    !is.na(fki),
    grepl("[A-Za-z]", visit_frequency)
  )

# average duration between fed and fki
filterpheno2 <- filterpheno %>%
  mutate(
    fed = as.Date(fed),
    fki = as.Date(fki),
    duration = as.numeric(fki-fed)
  )

average_duration <- mean(filterpheno2$duration, na.rm = TRUE)
min_duration <- min(filterpheno2$duration, na.rm = TRUE)
max_duration <- max(filterpheno2$duration, na.rm = TRUE)

average_duration
min_duration
max_duration
sd(filterpheno2$duration)

# average duration between hatch and fki
filterpheno3 <- filterpheno %>%
  mutate(
    fki = as.Date(fki),
    hatching_first_recorded = as.Date(hatching_first_recorded),
    duration = as.numeric(hatching_first_recorded-fki)
  )

average_duration2 <- mean(filterpheno3$duration, na.rm = TRUE)
average_duration2min <- min(filterpheno3$duration, na.rm = TRUE)
average_duration2max <- max(filterpheno3$duration, na.rm = TRUE)

average_duration2
average_duration2min
average_duration2max
sd(filterpheno3$duration)

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

# Load packages, datasets and refine -----

# get packages  
install.packages("Matrix")
install.packages("lme4")
install.packages("dplyr")
install.packages("ggeffects")
install.packages("ggplot2")
install.packages("patchwork")
install.packages("gt")
library(lme4)
library(Matrix)
library(dplyr)
library(ggeffects)
library(ggplot2)
library(patchwork)

## Adult dataset -----
adultdata <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Thesis/Data play/Adults.csv")

# make dataset only contain F in spring
adultdata<-subset(adultdata,sex=="F")
adultdata<-subset(adultdata,season=="spring")

## Nest dataset -----
nestdata <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Thesis/Data play/Bird_Phenology.csv")

# remove clutch swap treatment
nestdata <- nestdata %>%
  filter(
    is.na(clutch.swap.treatment) |
      clutch.swap.treatment == "" |
      clutch.swap.treatment == "unmanipulated"
  )

## Temp dataset -----
loggerdata <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Thesis/Data play/temperatures.csv")

# remove columns without temp measurements
loggerdata <- loggerdata[, !(names(loggerdata) %in% c("logger_id", "logger_res"))]

## Rainfall dataset -----
raindata <- read.delim("C:/Users/Swinn/Documents/EDI MASTERS/Thesis/Data play/daily_precipitation.txt", header=TRUE)

# Making temp loops -----
## Making temp min loops -----
### fed data -----
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

### fki data -----
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

### hatch data -----
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


## Making temp mean of minima loops -----
### fed data -----
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

### fki data -----
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

### hatch data -----
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

## Making temp mean loops -----
### fed data -----
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

### fki data -----
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

### hatch data -----
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

# Making rain loops -----
## Making rain min loops -----
### fed data -----
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
  
  # minimum rain
  rainmin_fed[x] <- min(rain.sub, na.rm = TRUE)
}

nestdata$rainmin_fed <- rainmin_fed
summary(rainmin_fed)

### fki data -----
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
  
  # minimum temperature 
  rainmin_fki[x] <- min(rain.sub, na.rm = TRUE)
}

nestdata$rainmin_fki <- rainmin_fki

summary(rainmin_fki)

### hatch data -----
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
  
  # absolute minimum temperature 
  rainmin_hatch[x] <- min(rain.sub, na.rm = TRUE)
}

nestdata$rainmin_hatch <- rainmin_hatch

summary(rainmin_hatch)

## Making rain mean loops -----
### fed data -----
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
  
  # Mean rain
  rainmean_fed[x] <- mean(rain.sub, na.rm = TRUE)
}

nestdata$rainmean_fed <- rainmean_fed

summary(rainmean_fed)

### fki data -----
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
  
  # Mean rain
  rainmean_fki[x] <- mean(rain.sub, na.rm = TRUE)
}

nestdata$rainmean_fki <- rainmean_fki

summary(rainmean_fki)

### hatch data -----
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
  
  # Mean temp
  rainmean_hatch[x] <- mean(rain.sub, na.rm = TRUE)
}

nestdata$rainmean_hatch <- rainmean_hatch

summary(rainmean_hatch)

## Making rain max loops -----
### fed data -----
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
  
  # minimum rain
  rainmax_fed[x] <- max(rain.sub, na.rm = TRUE)
}

nestdata$rainmax_fed <- rainmax_fed

summary(rainmax_fed)

### fki data -----
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
  
  # minimum temperature 
  rainmax_fki[x] <- max(rain.sub, na.rm = TRUE)
}

nestdata$rainmax_fki <- rainmax_fki

summary(rainmax_fki)

### hatch data -----
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
  
  # absolute minimum temperature 
  rainmax_hatch[x] <- max(rain.sub, na.rm = TRUE)
}

nestdata$rainmax_hatch <- rainmax_hatch

summary(rainmax_hatch)

## Making rain heavy loops -----

##### edit so that u calculate the number of days that is > 10 rainfall 
### fed data -----
# create storage vector of NAs which get filled as loop runs 
rainheavy_fed <- rep(NA, nrow(nestdata))
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
  
  # count number of days with heavy rainfall > 10
  rainheavy_fed[x] <- sum(rain.sub > 10, na.rm = TRUE)
  
}

nestdata$rainheavy_fed <- rainheavy_fed

summary(rainheavy_fed)

### fki data -----
# create storage vector of NAs which get filled as loop runs 
rainheavy_fki <- rep(NA, nrow(nestdata))
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
  
  # count number of days with heavy rainfall > 10
  rainheavy_fki[x] <- sum(rain.sub > 10, na.rm = TRUE)
}

nestdata$rainheavy_fki <- rainheavy_fki

summary(rainheavy_fki)


### hatch data -----
# create storage vector of NAs which get filled as loop runs 
rainheavy_hatch <- rep(NA, nrow(nestdata))
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
  
  # count number of days with heavy rainfall > 10
  rainheavy_hatch[x] <- sum(rain.sub > 10, na.rm = TRUE)
}

nestdata$rainheavy_hatch <- rainheavy_hatch

summary(rainheavy_hatch)
	

4. Then the final step is to run a statistical model with suc (# fledged) as the response and temperature as the predictor - probably as a linear and quadratic term. For this model you’ll also need to consider your random terms

#Then you can set up a linear mixed effects model with suc as the response - and the temperature metric as a predictor and some key random effects. 

# random effects = site, year, site*year, bird ID (new for unknown ID)
# model thoughts = poisson for suc (can critique as limitation later), make line graph quadratic with confidence intervals either side

  
# NICOLE MODEL ATTEMPT :D
  
# Preparing model dataset -----

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

# create female ID
nestdata$female<-1:nrow(nestdata)
nestdata$female<-as.character(adultdata$ring[pmatch(nestdata$ID,adultdata$ID)])
nestdata$female[which(is.na(nestdata$female)==TRUE)]<-1:length(which(is.na(nestdata$female)==TRUE))

# filter to entries with hatching_first_recorded
jointdata <- nestdata %>%
  filter(!is.na(hatching_first_recorded),
         hatching_first_recorded !="")

# remove any negative values
jointdata$suc[jointdata$suc < 0] <- NA

## Centre predictors -----
jointdata$temp_a_hatch <- scale(jointdata$tempmin_hatch, center = TRUE, scale = FALSE)
jointdata$temp_a_fki <- scale(jointdata$tempmin_fki, center = TRUE, scale = FALSE)
jointdata$temp_a_fed <- scale(jointdata$tempmin_fed, center = TRUE, scale = FALSE)

jointdata$temp_b_hatch <- scale(jointdata$tempmeanmin_hatch, center = TRUE, scale = FALSE)
jointdata$temp_b_fki <- scale(jointdata$tempmeanmin_fki, center = TRUE, scale = FALSE)
jointdata$temp_b_fed <- scale(jointdata$tempmeanmin_fed, center = TRUE, scale = FALSE)

jointdata$temp_c_hatch <- scale(jointdata$tempmean_hatch, center = TRUE, scale = FALSE)
jointdata$temp_c_fki <- scale(jointdata$tempmean_fki, center = TRUE, scale = FALSE)
jointdata$temp_c_fed <- scale(jointdata$tempmean_fed, center = TRUE, scale = FALSE)

jointdata$rain_a_hatch <- scale(jointdata$rainmin_hatch, center = TRUE, scale = FALSE)
jointdata$rain_a_fki <- scale(jointdata$rainmin_fki, center = TRUE, scale = FALSE)
jointdata$rain_a_fed <- scale(jointdata$rainmin_fed, center = TRUE, scale = FALSE)

jointdata$rain_b_hatch <- scale(jointdata$rainmean_hatch, center = TRUE, scale = FALSE)
jointdata$rain_b_fki <- scale(jointdata$rainmean_fki, center = TRUE, scale = FALSE)
jointdata$rain_b_fed <- scale(jointdata$rainmean_fed, center = TRUE, scale = FALSE)

jointdata$rain_c_hatch <- scale(jointdata$rainmax_hatch, center = TRUE, scale = FALSE)
jointdata$rain_c_fki <- scale(jointdata$rainmax_fki, center = TRUE, scale = FALSE)
jointdata$rain_c_fed <- scale(jointdata$rainmax_fed, center = TRUE, scale = FALSE)

jointdata$rain_d_hatch <- scale(jointdata$rainheavy_hatch, center = TRUE, scale = FALSE)
jointdata$rain_d_fki <- scale(jointdata$rainheavy_fki, center = TRUE, scale = FALSE)
jointdata$rain_d_fed <- scale(jointdata$rainheavy_fed, center = TRUE, scale = FALSE)

names(jointdata)

# Making the big modelling dataset -----
# List every variable used across all models

vars_temp <- c(
  "suc",
  "temp_b_fed", "temp_c_fed", "temp_a_fed", "tempmean_fed", "tempmeanmin_fed", "tempmin_fed",      # fed predictors - adjust names
  "temp_b_fki", "temp_c_fki", "temp_a_fki", "tempmean_fki", "tempmeanmin_fki", "tempmin_fki",     # fki predictors - adjust names
  "temp_b_hatch", "temp_c_hatch", "temp_a_hatch", "tempmean_hatch",  "tempmeanmin_hatch", "tempmin_hatch", # hatch predictors
  "site", "year", "site_year", "female")

vars_rain <- c(
  "suc",
  "rain_a_fed", "rain_b_fed", "rain_c_fed", "rain_d_fed", "rainmean_fed", "rainmin_fed", "rainmax_fed", "rainheavy_fed",
  "rain_a_fki", "rain_b_fki", "rain_c_fki", "rain_d_fki", "rainmean_fki", "rainmin_fki", "rainmax_fki", "rainheavy_fki",
  "rain_a_hatch", "rain_b_hatch", "rain_c_hatch", "rain_d_hatch","rainmean_hatch", "rainmin_hatch", "rainmax_hatch", "rainheavy_hatch",
  "site", "year", "site_year", "female")

## check all variables exist
vars_temp[!vars_temp %in% names(jointdata)]  
vars_rain[!vars_rain %in% names(jointdata)]

# Create common complete-case dataset
jointdata_temp <- jointdata[complete.cases(jointdata[, vars_temp]), ]
jointdata_rain <- jointdata[complete.cases(jointdata[, vars_rain]), ]

# Making temp models -----
## Making temp min models ----- 
### fed model -----
fed_min_model <- glmer(
  suc ~ temp_a_fed + I(temp_a_fed^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_temp,
  family = poisson(link = "log")
)

summary(fed_min_model)

VarCorr(fed_min_model)

## make fed graph

# make fed model predictions
preds <- ggpredict(fed_min_model, terms = "temp_a_fed [all]")
# convert back to real temp
temp_min_fed <- mean(jointdata$tempmin_fed, na.rm = TRUE)
preds$x_real_fed <- preds$x + temp_min_fed

# make fed plot
fed_min_plot <- ggplot(preds, aes(x = x_real_fed, y = predicted)) +
  geom_line(linewidth = 1, colour = "#2C7FB8") +
  coord_cartesian(xlim=c(-4,10), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(-6,18,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2C7FB8") +
  geom_rug(data = jointdata_temp, aes(x = tempmin_fed), inherit.aes = FALSE, sides = "b", colour = "#2C7FB8") +
  labs(
    x = "Minimum temperature (°C)",
    y = "Fledging success (number of chicks)",
    title = "A"
  ) +
  theme_classic(base_size = 20)

fed_min_plot

### fki model -----
fki_min_model <- glmer(
  suc ~ temp_a_fki + I(temp_a_fki^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_temp,
  family = poisson(link = "log")
)

summary(fki_min_model)

## make fki graph

# make fki model predictions
preds <- ggpredict(fki_min_model, terms = "temp_a_fki [all]")
# convert back to real temp
temp_min_fki <- mean(jointdata$tempmin_fki, na.rm = TRUE)
preds$x_real_fki <- preds$x + temp_min_fki

# make fki plot
fki_min_plot <- ggplot(preds, aes(x = x_real_fki, y = predicted)) +
  geom_line(linewidth = 1, colour = "#FDD835") +
  coord_cartesian(xlim=c(-4,10), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(-6,18,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#FDD835") +
  geom_rug(data = jointdata_temp, aes(x = tempmin_fki), inherit.aes = FALSE, sides = "b", colour = "#FDD835") +
  labs(
    x = "Minimum temperature (°C)",
    y = "Fledging success (number of chicks)",
    title = "B"
  ) +
  theme_classic(base_size = 20)

fki_min_plot

### hatch model -----
hatch_min_model <- glmer(
  suc ~ temp_a_hatch + I(temp_a_hatch^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_temp,
  family = poisson(link = "log")
)

summary(hatch_min_model)

## make hatch graph

# make fed model predictions
preds <- ggpredict(hatch_min_model, terms = "temp_a_hatch [all]")
# convert back to real temp
temp_min_hatch <- mean(jointdata$tempmin_hatch, na.rm = TRUE)
preds$x_real_hatch <- preds$x + temp_min_hatch

# make fed plot
hatch_min_plot <- ggplot(preds, aes(x = x_real_hatch, y = predicted)) +
  geom_line(linewidth = 1, colour = "#41AB5D") +
  coord_cartesian(xlim=c(-2,14), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(-6,18,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#41AB5D") +
  geom_rug(data = jointdata_temp, aes(x = tempmin_hatch), inherit.aes = FALSE, sides = "b", colour = "#41AB5D") +
  labs(
    x = "Minimum temperature (°C)",
    y = "Fledging success (number of chicks)",
    title = "C"
  ) +
  theme_classic(base_size = 20)

hatch_min_plot

fed_min_plot + fki_min_plot + hatch_min_plot

## Making temp mean of minima models -----
### fed model -----
fed_mean_of_minima_model <- glmer(
  suc ~ temp_b_fed + I(temp_b_fed^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_temp,
  family = poisson(link = "log")
)

summary(fed_mean_of_minima_model)

## make fed graph

# make fed model predictions
preds <- ggpredict(fed_mean_of_minima_model, terms = "temp_b_fed [all]")
# convert back to real temp
temp_meanmin_fed <- mean(jointdata$tempmeanmin_fed, na.rm = TRUE)
preds$x_real_meanmin_fed <- preds$x + temp_meanmin_fed

# make fed plot
fed_mean_of_minima_plot <- ggplot(preds, aes(x = x_real_meanmin_fed, y = predicted)) +
  geom_line(linewidth = 1, colour = "#2C7FB8") +
  coord_cartesian(xlim=c(0,12), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(-6,18,by=2)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2C7FB8") +
  geom_rug(data = jointdata_temp, aes(x = tempmeanmin_fed), inherit.aes = FALSE, sides = "b", colour = "#2C7FB8") +
  labs(
    x = "Mean daily minimum temperature (°C)",
    y = "Fledging success (number of chicks)",
    title = "D"
  ) +
  theme_classic(base_size = 20)

fed_mean_of_minima_plot

### fki model -----
fki_mean_of_minima_model <- glmer(
  suc ~ temp_b_fki + I(temp_b_fki^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_temp,
  family = poisson(link = "log")
)

summary(fki_mean_of_minima_model)

# make fki graph

# make model predictions
preds <- ggpredict(fki_mean_of_minima_model, terms = "temp_b_fki [all]")
# convert back to real temp
temp_meanmin_fki <- mean(jointdata$tempmeanmin_fki, na.rm = TRUE)
preds$x_real_meanmin_fki <- preds$x + temp_meanmin_fki

# make fki plot
fki_mean_of_minima_plot <- ggplot(preds, aes(x = x_real_meanmin_fki, y = predicted)) +
  geom_line(linewidth = 1, colour = "#FDD835") +
  coord_cartesian(xlim=c(0,12), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(-6,18,by=2)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#FDD835") +
  geom_rug(data = jointdata_temp, aes(x = tempmeanmin_fki), inherit.aes = FALSE, sides = "b", colour = "#FDD835") +
  labs(
    x = "Mean daily minimum temperature (°C)",
    y = "Fledging success (number of chicks)",
    title = "E"
  ) +
  theme_classic(base_size = 20)

fki_mean_of_minima_plot

### hatch model -----
hatch_mean_of_minima_model <- glmer(
  suc ~ temp_b_hatch + I(temp_b_hatch^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_temp,
  family = poisson(link = "log")
)

summary(hatch_mean_of_minima_model)

# make hatch graph

# make model predictions
preds <- ggpredict(hatch_mean_of_minima_model, terms = "temp_b_hatch [all]")
# convert back to real temp
temp_meanmin_hatch <- mean(jointdata$tempmeanmin_hatch, na.rm = TRUE)
preds$x_real_meanmin_hatch <- preds$x + temp_meanmin_hatch

# make hatch plot
hatch_mean_of_minima_plot <- ggplot(preds, aes(x = x_real_meanmin_hatch, y = predicted)) +
  geom_line(linewidth = 1, colour = "#41AB5D") +
  coord_cartesian(xlim=c(2,14), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(-6,18,by=2)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#41AB5D") +
  geom_rug(data = jointdata_temp, aes(x = tempmeanmin_hatch), inherit.aes = FALSE, sides = "b", colour = "#41AB5D") +
  labs(
    x = "Mean daily minimum temperature (°C)",
    y = "Fledging success (number of chicks)",
    title = "F"
  ) +
  theme_classic(base_size = 20)

hatch_mean_of_minima_plot

fed_mean_of_minima_plot + fki_mean_of_minima_plot + hatch_mean_of_minima_plot

## Making temp mean models -----
### fed model -----
fed_mean_model <- glmer(
  suc ~ temp_c_fed + I(temp_c_fed^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_temp,
  family = poisson(link = "log")
)

summary(fed_mean_model)

## make fed graph

# make fed model predictions
preds <- ggpredict(fed_mean_model, terms = "temp_c_fed [all]")
# convert back to real temp
temp_mean_fed <- mean(jointdata$tempmean_fed, na.rm = TRUE)
preds$x_real_mean_fed <- preds$x + temp_mean_fed

# make fed plot
fed_mean_plot <- ggplot(preds, aes(x = x_real_mean_fed, y = predicted)) +
  geom_line(linewidth = 1, colour = "#2C7FB8") +
  coord_cartesian(xlim=c(4,18), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(-6,18,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2C7FB8") +
  geom_rug(data = jointdata_temp, aes(x = tempmean_fed), inherit.aes = FALSE, sides = "b", colour = "#2C7FB8") +
  labs(
    x = "Mean temperature (°C)",
    y = "Fledging success (number of chicks)",
    title = "G"
  ) +
  theme_classic(base_size = 20)

fed_mean_plot

### fki model -----
fki_mean_model <- glmer(
  suc ~ temp_c_fki + I(temp_c_fki^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_temp,
  family = poisson(link = "log")
)

summary(fki_mean_model)

## make fki graph

# make fki model predictions
preds <- ggpredict(fki_mean_model, terms = "temp_c_fki [all]")
# convert back to real temp
temp_mean_fki <- mean(jointdata$tempmean_fki, na.rm = TRUE)
preds$x_real_mean_fki <- preds$x + temp_mean_fki

# make fki plot
fki_mean_plot <- ggplot(preds, aes(x = x_real_mean_fki, y = predicted)) +
  geom_line(linewidth = 1, colour = "#FDD835") +
  coord_cartesian(xlim=c(4,18), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2))+
  scale_x_continuous(breaks=seq(-6,18,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#FDD835") +
  geom_rug(data = jointdata_temp, aes(x = tempmean_fki), inherit.aes = FALSE, sides = "b", colour = "#FDD835") +
  labs(
    x = "Mean temperature (°C)",
    y = "Fledging success (number of chicks)",
    title = "H"
  ) +
  theme_classic(base_size = 20)

fki_mean_plot

### hatch model -----
hatch_mean_model <- glmer(
  suc ~ temp_c_hatch + I(temp_c_hatch^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_temp,
  family = poisson(link = "log")
)

summary(hatch_mean_model)

## make hatch graph

# make hatch model predictions
preds <- ggpredict(hatch_mean_model, terms = "temp_c_hatch [all]")
# convert back to real temp
temp_mean_hatch <- mean(jointdata$tempmean_hatch, na.rm = TRUE)
preds$x_real_mean_hatch <- preds$x + temp_mean_hatch

# make hatch plot
hatch_mean_plot <- ggplot(preds, aes(x = x_real_mean_hatch, y = predicted)) +
  geom_line(linewidth = 1, colour = "#41AB5D") +
  coord_cartesian(xlim=c(8,18), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2))+
  scale_x_continuous(breaks=seq(-6,18,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#41AB5D") +
  geom_rug(data = jointdata_temp, aes(x = tempmean_hatch), inherit.aes = FALSE, sides = "b", colour = "#41AB5D") +
  labs(
    x = "Mean temperature (°C)",
    y = "Fledging success (number of chicks)",
    title = "I"
  ) +
  theme_classic(base_size = 20)

hatch_mean_plot

fed_mean_plot + fki_mean_plot + hatch_mean_plot

## Making a null temp model ----- 
null_temp_model <- glmer(
  suc ~ 1 +
    (1 | site) + (1 | year) + (1 | site_year) + (1 | female),
  data = jointdata_temp,
  family = poisson(link = "log")
)

summary(null_temp_model)
VarCorr(null_temp_model)

# Comparing temp AIC -----
# aic for each temp
make_aic_table <- function(...) {
  models <- list(...)
  model_names <- sapply(substitute(list(...))[-1], as.character)
  aic_vals <- sapply(models, AIC)
  df <- data.frame(Model = model_names, AIC = aic_vals)
  df$ΔAIC <- df$AIC - min(df$AIC)
  df
}

fed_table   <- make_aic_table(null_temp_model, fed_min_model, fed_mean_of_minima_model, fed_mean_model)
fki_table   <- make_aic_table(null_temp_model, fki_min_model, fki_mean_of_minima_model, fki_mean_model)
hatch_table <- make_aic_table(null_temp_model, hatch_min_model, hatch_mean_of_minima_model, hatch_mean_model)

# Add a grouping label and combine
fed_table$Period   <- "Egg laying"
fki_table$Period   <- "Incubation"
hatch_table$Period <- "Hatching"

combined_table <- bind_rows(fed_table, fki_table, hatch_table)
combined_table

gt(combined_table)

# check these all return the same number to confirm same size dataset 
nobs(null_temp_model)
nobs(fed_mean_of_minima_model)
nobs(fki_mean_model)
nobs(hatch_min_model)

# compare aic for each period
best_fed   <- min(fed_table$AIC)
best_fki   <- min(fki_table$AIC)
best_hatch <- min(hatch_table$AIC)
null_aic   <- AIC(null_temp_model)

period_comparison <- data.frame(
  Period = c("Null", "Egg laying", "Incubation", "Hatching"),
  AIC = c(null_aic, best_fed, best_fki, best_hatch)
)
period_comparison$ΔAIC <- period_comparison$AIC - min(period_comparison$AIC)
period_comparison

gt(period_comparison)

## Making a temp model with 3 variables -----

# make model (with altered optimizer to find the optimal with more iterations)
combined_model <- glmer(
  suc ~ temp_b_fed + I(temp_b_fed^2) +
    temp_b_fki + I(temp_b_fki^2) +
    temp_a_hatch + I(temp_a_hatch^2) +
    (1 | site) + 
    (1 | year) + 
    (1 | site_year) + 
    (1 | female),
  data = jointdata_temp,
  family = poisson(link = "log"),
    control = glmerControl( 
      optimizer = "bobyqa",
      optCtrl = list(maxfun = 2e5)      
    )
)

summary(combined_model)

### fed plot -----
# make predictions
pred_fed <- as.data.frame(
  ggpredict(
    combined_model,
    terms = "temp_b_fed [all]",
    condition = c(
      temp_b_fki = mean(jointdata_temp$temp_b_fki, na.rm = TRUE),
      temp_a_hatch = mean(jointdata_temp$temp_a_hatch, na.rm = TRUE)
    )
  )
)
# convert back to real temp
temp_meanmin_fed <- mean(jointdata$tempmeanmin_fed, na.rm = TRUE)
pred_fed$x_real_fed <- pred_fed$x + temp_meanmin_fed

# making combined fed model
combined_stages_fed_plot <- ggplot(pred_fed, aes(x = x_real_fed, y = predicted)) +
  geom_line(linewidth = 1, colour = "#2C7FB8") +
  geom_rug(data = jointdata_temp,
           aes(x = tempmeanmin_fed),
           inherit.aes = FALSE,
           sides = "b",
           alpha = 0.3,
           colour = "#2C7FB8") +
  coord_cartesian(xlim=c(0,12), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(-4,18,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2C7FB8") +
  labs(
    x = "Mean daily minimum temperature (°C)",
    y = "Fledging success (number of chicks)",
    title = "J"
  ) +
  theme_classic(base_size = 20)

combined_stages_fed_plot

### fki plot -----
# make predictions
pred_fki <- as.data.frame(
  ggpredict(
    combined_model,
    terms = "temp_b_fki [all]",
    condition = c(
      temp_b_fed = mean(jointdata_temp$temp_b_fed, na.rm = TRUE),
      temp_a_hatch = mean(jointdata_temp$temp_a_hatch, na.rm = TRUE)
    )
  )
)
# convert back to real temp
temp_meanmin_fki <- mean(jointdata$tempmeanmin_fki, na.rm = TRUE)
pred_fki$x_real_fki <- pred_fki$x + temp_meanmin_fki

# making combined fki model
combined_stages_fki_plot <- ggplot(pred_fki, aes(x = x_real_fki, y = predicted)) +
  geom_line(linewidth = 1, colour = "#FDD835") +
  geom_rug(data = jointdata_temp,
           aes(x = tempmeanmin_fki),
           inherit.aes = FALSE,
           sides = "b",
           alpha = 0.3,
           colour = "#FDD835") +
  coord_cartesian(xlim=c(0,12), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(-4,18,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#FDD835") +
  labs(
    x = "Mean daily minimum temperature (°C)",
    y = "Fledging success (number of chicks)",
    title = "K"
  ) +
  theme_classic(base_size = 20)

combined_stages_fki_plot

### hatch plot -----
# make predictions
pred_hatch <- as.data.frame(
  ggpredict(
    combined_model,
    terms = "temp_a_hatch [all]",
    condition = c(
      temp_b_fki = mean(jointdata_temp$temp_b_fki, na.rm = TRUE),
      temp_b_fed = mean(jointdata_temp$temp_b_fed, na.rm = TRUE)
    )
  )
)
# convert back to real temp
temp_min_hatch <- mean(jointdata$tempmin_hatch, na.rm = TRUE)
pred_hatch$x_real_hatch <- pred_hatch$x + temp_min_hatch

# making combined fed model
combined_stages_hatch_plot <- ggplot(pred_hatch, aes(x = x_real_hatch, y = predicted)) +
  geom_line(linewidth = 1, colour = "#41AB5D") +
  geom_rug(data = jointdata_temp,
           aes(x = tempmin_hatch),
           inherit.aes = FALSE,
           sides = "b",
           alpha = 0.3,
           colour = "#41AB5D") +
  coord_cartesian(xlim=c(-2,14), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(-4,18,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#41AB5D") +
  labs(
    x = "Minimum temperature (°C)",
    y = "Fledging success (number of chicks)",
    title = "L"
  ) +
  theme_classic(base_size = 20)

combined_stages_hatch_plot

combined_stages_fed_plot + combined_stages_fki_plot + combined_stages_hatch_plot

## Making the big temp plot -----
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

# Making rain models -----
## Making rain min models -----
### fed model -----
fed_rain_min_model <- glmer(
  suc ~ rain_a_fed + I(rain_a_fed^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_rain,
  family = poisson(link = "log")
)

summary(fed_rain_min_model)

# how important are all these random effects
VarCorr(fed_rain_min_model)

## make fed graph

# make fed model predictions
preds <- ggpredict(fed_rain_min_model, terms = "rain_a_fed [all]")
# convert back to real temp
temp_rain_min_fed <- mean(jointdata$rainmin_fed, na.rm = TRUE)
preds$x_real_rain_fed <- preds$x + temp_rain_min_fed

# make fed plot
fed_rain_min_plot <- ggplot(preds, aes(x = x_real_rain_fed, y = predicted)) +
  geom_line(linewidth = 1, colour = "#2C7FB8") +
  coord_cartesian(xlim=c(0,0.65), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(0,10,by=0.2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2C7FB8") +
  geom_rug(data = jointdata_rain, aes(x = rainmin_fed), inherit.aes = FALSE, sides = "b", colour = "#2C7FB8") +
  labs(
    x = "Minimum rainfall (mm)",
    y = "Fledging success (number of chicks)",
    title = "A"
  ) +
  theme_classic(base_size = 20)

fed_rain_min_plot

### fki model -----
# use poly(rain_a_fki,2) to remove the issue with large eigenvalues when things have already been scaled
fki_rain_min_model <- glmer(
  suc ~ poly(rain_a_fki,2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_rain,
  family = poisson(link = "log")
)

summary(fki_rain_min_model)

# how important are all these random effects
VarCorr(fki_rain_min_model)

## make fki graph

# make fki model predictions
preds <- ggpredict(fki_rain_min_model, terms = "rain_a_fki [all]")
# convert back to real temp
temp_rain_min_fki <- mean(jointdata$rainmin_fki, na.rm = TRUE)
preds$x_real_rain_fki <- preds$x + temp_rain_min_fki

# make fki plot
fki_rain_min_plot <- ggplot(preds, aes(x = x_real_rain_fki, y = predicted)) +
  geom_line(linewidth = 1, colour = "#FDD835") +
  coord_cartesian(xlim=c(0,0.15), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(0,10,by=0.05))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#FDD835") +
  geom_rug(data = jointdata_rain, aes(x = rainmin_fki), inherit.aes = FALSE, sides = "b", colour = "#FDD835") +
  labs(
    x = "Minimum rainfall (mm)",
    y = "Fledging success (number of chicks)",
    title = "B"
  ) +
  theme_classic(base_size = 20)

fki_rain_min_plot

### hatch model -----
hatch_rain_min_model <- glmer(
  suc ~ rain_a_hatch + I(rain_a_hatch^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_rain,
  family = poisson(link = "log")
)

summary(hatch_rain_min_model) 

# how important are all these random effects
VarCorr(hatch_rain_min_model)

## make hatch graph

# make fed model predictions
# AS THIS DATA HAS 1 DATA POINT AT 0.4 HAVE TO SPECIFY RANGE FOR PREDS TO MAKE NICE EVEN GRID
rain_seq <- seq(
  min(jointdata_rain$rain_a_hatch, na.rm = TRUE), 
  max(jointdata_rain$rain_a_hatch, na.rm = TRUE), 
  length.out = 200
)

preds <- ggpredict(
  hatch_rain_min_model, 
  terms = list(rain_a_hatch = rain_seq)
)

# convert back to real temp
rain_min_hatch <- mean(jointdata$rainmin_hatch, na.rm = TRUE)
preds$x_real_rain_hatch <- preds$x + rain_min_hatch

# make fed plot
hatch_rain_min_plot <- ggplot(preds, aes(x = x_real_rain_hatch, y = predicted)) +
  geom_line(linewidth = 1, colour = "#41AB5D") +
  coord_cartesian(xlim=c(0,0.45), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(-6,18,by=0.1))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#41AB5D") +
  geom_rug(data = jointdata_rain, aes(x = rainmin_hatch), inherit.aes = FALSE, sides = "b", colour = "#41AB5D") +
  labs(
    x = "Minimum rainfall (mm)",
    y = "Fledging success (number of chicks)",
    title = "C"
  ) +
  theme_classic(base_size = 20)

hatch_rain_min_plot

fed_rain_min_plot + fki_rain_min_plot + hatch_rain_min_plot

## Making rain mean models -----
### fed model -----
fed_rain_mean_model <- glmer(
  suc ~ rain_b_fed + I(rain_b_fed^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_rain,
  family = poisson(link = "log")
)

summary(fed_rain_mean_model)

# how important are all these random effects
VarCorr(fed_rain_mean_model)

## make fed graph
# make fed model predictions
preds <- ggpredict(fed_rain_mean_model, terms = "rain_b_fed [all]")
# convert back to real rain
rain_mean_fed <- mean(jointdata$rainmean_fed, na.rm = TRUE)
preds$x_real_rain_mean_fed <- preds$x + rain_mean_fed

# make fed plot
fed_rain_mean_plot <- ggplot(preds, aes(x = x_real_rain_mean_fed, y = predicted)) +
  geom_line(linewidth = 1, colour = "#2C7FB8") +
  coord_cartesian(xlim=c(0,9), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(-6,18,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2C7FB8") +
  geom_rug(data = jointdata_rain, aes(x = rainmean_fed), inherit.aes = FALSE, sides = "b", colour = "#2C7FB8") +
  labs(
    x = "Mean rainfall (mm)",
    y = "Fledging success (number of chicks)",
    title = "D"
  ) +
  theme_classic(base_size = 20)

fed_rain_mean_plot

### fki model -----
fki_rain_mean_model <- glmer(
  suc ~ rain_b_fki + I(rain_b_fki^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_rain,
  family = poisson(link = "log"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(fki_rain_mean_model)

## make fki graph

# make fki model predictions
preds <- ggpredict(fki_rain_mean_model, terms = "rain_b_fki [all]")
# convert back to real temp
rain_mean_fki <- mean(jointdata$rainmean_fki, na.rm = TRUE)
preds$x_real_rain_mean_fki <- preds$x + rain_mean_fki

# make fki plot
fki_rain_mean_plot <- ggplot(preds, aes(x = x_real_rain_mean_fki, y = predicted)) +
  geom_line(linewidth = 1, colour = "#FDD835") +
  coord_cartesian(xlim=c(0,8), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2))+
  scale_x_continuous(breaks=seq(-6,18,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#FDD835") +
  geom_rug(data = jointdata_rain, aes(x = rainmean_fki), inherit.aes = FALSE, sides = "b", colour = "#FDD835") +
  labs(
    x = "Mean rainfall (mm)",
    y = "Fledging success (number of chicks)",
    title = "E"
  ) +
  theme_classic(base_size = 20)

fki_rain_mean_plot

### hatch model -----
hatch_rain_mean_model <- glmer(
  suc ~ poly(rain_b_hatch,2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_rain,
  family = poisson(link = "log"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(hatch_rain_mean_model)

VarCorr(hatch_rain_mean_model)

## make hatch graph

# make hatch model predictions
preds <- ggpredict(hatch_rain_mean_model, terms = "rain_b_hatch [all]")
# convert back to real temp
rain_mean_hatch <- mean(jointdata$rainmean_hatch, na.rm = TRUE)
preds$x_real_rain_mean_hatch <- preds$x + rain_mean_hatch

# make hatch plot
hatch_rain_mean_plot <- ggplot(preds, aes(x = x_real_rain_mean_hatch, y = predicted)) +
  geom_line(linewidth = 1, colour = "#41AB5D") +
  coord_cartesian(xlim=c(0,8), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2))+
  scale_x_continuous(breaks=seq(-6,18,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#41AB5D") +
  geom_rug(data = jointdata_rain, aes(x = rainmean_hatch), inherit.aes = FALSE, sides = "b", colour = "#41AB5D") +
  labs(
    x = "Mean rainfall (mm)",
    y = "Fledging success (number of chicks)",
    title = "F"
  ) +
  theme_classic(base_size = 20)

hatch_rain_mean_plot

fed_rain_mean_plot + fki_rain_mean_plot + hatch_rain_mean_plot

## Making rain max models -----

### fed model ----- 
fed_rain_max_model <- glmer(
  suc ~ poly(rain_c_fed,2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_rain,
  family = poisson(link = "log"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(fed_rain_max_model)

VarCorr(fed_rain_max_model)

## make fed graph

# make fed model predictions
preds <- ggpredict(fed_rain_max_model, terms = "rain_c_fed [all]")
# convert back to real rain
rain_max_fed <- mean(jointdata$rainmax_fed, na.rm = TRUE)
preds$x_real_rain_max_fed <- preds$x + rain_max_fed

# make fed plot
fed_rain_max_plot <- ggplot(preds, aes(x = x_real_rain_max_fed, y = predicted)) +
  geom_line(linewidth = 1, colour = "#2C7FB8") +
  coord_cartesian(xlim=c(0,40), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(0,80,by=10))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2C7FB8") +
  geom_rug(data = jointdata_rain, aes(x = rainmax_fed), inherit.aes = FALSE, sides = "b", colour = "#2C7FB8") +
  labs(
    x = "Maximum rainfall (mm)",
    y = "Fledging success (number of chicks)",
    title = "G"
  ) +
  theme_classic(base_size = 20)

fed_rain_max_plot

### fki model -----  
fki_rain_max_model <- glmer(
  suc ~ poly(rain_c_fki,2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_rain,
  family = poisson(link = "log")
)

summary(fki_rain_max_model)

VarCorr(fki_rain_max_model)

## make fki graph

# make fki model predictions
preds <- ggpredict(fki_rain_max_model, terms = "rain_c_fki [all]")
# convert back to real temp
rain_max_fki <- mean(jointdata$rainmax_fki, na.rm = TRUE)
preds$x_real_rain_max_fki <- preds$x + rain_max_fki

# make fki plot
fki_rain_max_plot <- ggplot(preds, aes(x = x_real_rain_max_fki, y = predicted)) +
  geom_line(linewidth = 1, colour = "#FDD835") +
  coord_cartesian(xlim=c(0,70), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2))+
  scale_x_continuous(breaks=seq(0,80,by=10))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#FDD835") +
  geom_rug(data = jointdata_rain, aes(x = rainmax_fki), inherit.aes = FALSE, sides = "b", colour = "#FDD835") +
  labs(
    x = "Maximum rainfall (mm)",
    y = "Fledging success (number of chicks)",
    title = "H"
  ) +
  theme_classic(base_size = 20)

fki_rain_max_plot

### hatch model ----- 
hatch_rain_max_model <- glmer(
  suc ~ poly(rain_c_hatch,2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_rain,
  family = poisson(link = "log"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(hatch_rain_max_model)

VarCorr(hatch_rain_max_model)

## make hatch graph

# make hatch model predictions
preds <- ggpredict(hatch_rain_max_model, terms = "rain_c_hatch [all]")
# convert back to real temp
rain_max_hatch <- mean(jointdata$rainmax_hatch, na.rm = TRUE)
preds$x_real_rain_max_hatch <- preds$x + rain_max_hatch

# make hatch plot
hatch_rain_max_plot <- ggplot(preds, aes(x = x_real_rain_max_hatch, y = predicted)) +
  geom_line(linewidth = 1, colour = "#41AB5D") +
  coord_cartesian(xlim=c(0,70), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2))+
  scale_x_continuous(breaks=seq(0,80,by=10))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#41AB5D") +
  geom_rug(data = jointdata_rain, aes(x = rainmax_hatch), inherit.aes = FALSE, sides = "b", colour = "#41AB5D") +
  labs(
    x = "Maximum rainfall (mm)",
    y = "Fledging success (number of chicks)",
    title = "I"
  ) +
  theme_classic(base_size = 20)

hatch_rain_max_plot

fed_rain_max_plot + fki_rain_max_plot + hatch_rain_max_plot


## Making rain heavy models -----
### fed model ----- 
fed_rain_heavy_model <- glmer(
  suc ~ rain_d_fed + I(rain_d_fed^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_rain,
  family = poisson(link = "log")
)

summary(fed_rain_heavy_model)

VarCorr(fed_rain_heavy_model)

## make fed graph

# make fed model predictions
preds <- ggpredict(fed_rain_heavy_model, terms = "rain_d_fed [all]")
# convert back to real rain
rain_heavy_fed <- mean(jointdata$rainheavy_fed, na.rm = TRUE)
preds$x_real_rain_heavy_fed <- preds$x + rain_heavy_fed

# make fed plot
fed_rain_heavy_plot <- ggplot(preds, aes(x = x_real_rain_heavy_fed, y = predicted)) +
  geom_line(linewidth = 1, colour = "#2C7FB8") +
  coord_cartesian(xlim=c(0,5), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(0,50,by=1))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2C7FB8") +
  geom_rug(data = jointdata_rain, aes(x = rainheavy_fed), inherit.aes = FALSE, sides = "b", colour = "#2C7FB8") +
  labs(
    x = "Number of heavy rainfall days",
    y = "Fledging success (number of chicks)",
    title = "J"
  ) +
  theme_classic(base_size = 20)

fed_rain_heavy_plot

### fki model -----  
fki_rain_heavy_model <- glmer(
  suc ~ rain_d_fki + I(rain_d_fki^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_rain,
  family = poisson(link = "log")
)

summary(fki_rain_heavy_model)

VarCorr(fki_rain_heavy_model)

## make fki graph

# make fki model predictions
preds <- ggpredict(fki_rain_heavy_model, terms = "rain_d_fki [all]")
# convert back to real temp
rain_heavy_fki <- mean(jointdata$rainheavy_fki, na.rm = TRUE)
preds$x_real_rain_heavy_fki <- preds$x + rain_heavy_fki

# make fki plot
fki_rain_heavy_plot <- ggplot(preds, aes(x = x_real_rain_heavy_fki, y = predicted)) +
  geom_line(linewidth = 1, colour = "#FDD835") +
  coord_cartesian(xlim=c(0,5), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2))+
  scale_x_continuous(breaks=seq(0,70,by=1))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#FDD835") +
  geom_rug(data = jointdata_rain, aes(x = rainheavy_fki), inherit.aes = FALSE, sides = "b", colour = "#FDD835") +
  labs(
    x = "Number of heavy rainfall days",
    y = "Fledging success (number of chicks)",
    title = "K"
  ) +
  theme_classic(base_size = 20)

fki_rain_heavy_plot

### hatch model ----- 
hatch_rain_heavy_model <- glmer(
  suc ~ rain_d_hatch + I(rain_d_hatch^2) +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_rain,
  family = poisson(link = "log")
)

summary(hatch_rain_heavy_model)

VarCorr(hatch_rain_heavy_model)

## make hatch graph

# make hatch model predictions
preds <- ggpredict(hatch_rain_heavy_model, terms = "rain_d_hatch [all]")
# convert back to real temp
rain_heavy_hatch <- mean(jointdata$rainheavy_hatch, na.rm = TRUE)
preds$x_real_rain_heavy_hatch <- preds$x + rain_heavy_hatch

# make hatch plot
hatch_rain_heavy_plot <- ggplot(preds, aes(x = x_real_rain_heavy_hatch, y = predicted)) +
  geom_line(linewidth = 1, colour = "#41AB5D") +
  coord_cartesian(xlim=c(0,5), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2))+
  scale_x_continuous(breaks=seq(0,80,by=1))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#41AB5D") +
  geom_rug(data = jointdata_rain, aes(x = rainheavy_hatch), inherit.aes = FALSE, sides = "b", colour = "#41AB5D") +
  labs(
    x = "Number of heavy rainfall days",
    y = "Fledging success (number of chicks)",
    title = "L"
  ) +
  theme_classic(base_size = 20)

hatch_rain_heavy_plot

fed_rain_heavy_plot + fki_rain_heavy_plot + hatch_rain_heavy_plot

## Making a null rain model ----- 
null_rain_model <- glmer(
  suc ~ 1 +
    (1 | site) + (1 | year) + (1 | site_year) + (1 | female),
  data = jointdata_rain,
  family = poisson(link = "log")
)

summary(null_rain_model)
VarCorr(null_rain_model)

## Comparing rain AIC -----

AIC(fed_rain_min_model, fed_rain_mean_model, fed_rain_max_model)
AIC(fki_rain_min_model, fki_rain_mean_model, fki_rain_max_model)
AIC(hatch_rain_min_model, hatch_rain_mean_model, hatch_rain_max_model)

# aic for each rain
make_aic_table <- function(...) {
  models <- list(...)
  model_names <- sapply(substitute(list(...))[-1], as.character)
  aic_vals <- sapply(models, AIC)
  df <- data.frame(Model = model_names, AIC = aic_vals)
  df$ΔAIC <- df$AIC - min(df$AIC)
  df
}

fed_rain_table   <- make_aic_table(null_rain_model, fed_rain_min_model, fed_rain_mean_model, fed_rain_max_model, fed_rain_heavy_model)
fki_rain_table   <- make_aic_table(null_rain_model, fki_rain_min_model, fki_rain_mean_model, fki_rain_max_model, fki_rain_heavy_model)
hatch_rain_table <- make_aic_table(null_rain_model, hatch_rain_min_model, hatch_rain_mean_model, hatch_rain_max_model, hatch_rain_heavy_model)

# Add a grouping label and combine
fed_rain_table$Period   <- "Egg laying"
fki_rain_table$Period   <- "Incubation"
hatch_rain_table$Period <- "Hatching"

combined_rain_table <- bind_rows(fed_rain_table, fki_rain_table, hatch_rain_table)
combined_rain_table

gt(combined_rain_table)

# check these all return the same number to confirm same size dataset 
nobs(null_rain_model)
nobs(fed_rain_min_model)
nobs(fki_rain_max_model)
nobs(hatch_rain_mean_model)

# compare aic for each period
fed_rain_2_table   <- make_aic_table(fed_rain_min_model, fed_rain_mean_model, fed_rain_max_model, fed_rain_heavy_model)
fki_rain_2_table   <- make_aic_table(fki_rain_min_model, fki_rain_mean_model, fki_rain_max_model, fki_rain_heavy_model)
hatch_rain_2_table <- make_aic_table(hatch_rain_min_model, hatch_rain_mean_model, hatch_rain_max_model, hatch_rain_heavy_model)

best_rain_fed   <- min(fed_rain_2_table$AIC)
best_rain_fki   <- min(fki_rain_2_table$AIC)
best_rain_hatch <- min(hatch_rain_2_table$AIC)
null_rain_aic   <- AIC(null_rain_model)

period_rain_comparison <- data.frame(
  Period = c("Null", "Egg laying", "Incubation", "Hatching"),
  AIC = c(null_rain_aic, best_rain_fed, best_rain_fki, best_rain_hatch)
)
period_rain_comparison$ΔAIC <- period_rain_comparison$AIC - min(period_rain_comparison$AIC)
period_rain_comparison

gt(period_rain_comparison)

## Making rain model with 3 variables -----

# make model (with altered optimizer to find the optimal with more iterations)
combined_model <- glmer(
  suc ~ rain_b_fed + I(rain_b_fed^2) +
    rain_b_fki + I(rain_b_fki^2) +
    rain_d_hatch + I(rain_d_hatch^2) +
    (1 | site) + 
    (1 | year) + 
    (1 | site_year) + 
    (1 | female),
  data = jointdata_rain,
  family = poisson(link = "log"),
  )

summary(combined_model)

VarCorr(combined_model)

### fed model -----
# make predictions
pred_fed <- as.data.frame(
  ggpredict(
    combined_model,
    terms = "rain_b_fed [all]",
    condition = c(
      rain_b_fki = mean(jointdata_rain$rain_b_fki, na.rm = TRUE),
      rain_d_hatch = mean(jointdata_rain$rain_d_hatch, na.rm = TRUE)
    )
  )
)
# convert back to real temp
rain_mean_fed <- mean(jointdata$rainmean_fed, na.rm = TRUE)
pred_fed$x_real_rain_fed <- pred_fed$x + rain_mean_fed

# making combined fed model
combined_stages_rain_fed_plot <- ggplot(pred_fed, aes(x = x_real_rain_fed, y = predicted)) +
  geom_line(linewidth = 1, colour = "#2C7FB8") +
  geom_rug(data = jointdata_rain,
           aes(x = rainmean_fed),
           inherit.aes = FALSE,
           sides = "b",
           alpha = 0.3,
           colour = "#2C7FB8") +
  coord_cartesian(xlim=c(0,8), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(0,50,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2C7FB8") +
  labs(
    x = "Mean rainfall (mm)",
    y = "Fledging success (number of chicks)",
    title = "M"
  ) +
  theme_classic(base_size = 20)

combined_stages_rain_fed_plot

### fki model
# make predictions
pred_fki <- as.data.frame(
  ggpredict(
    combined_model,
    terms = "rain_b_fki [all]",
    condition = c(
      rain_b_fed = mean(jointdata_rain$rain_b_fed, na.rm = TRUE),
      rain_d_hatch = mean(jointdata_rain$rain_d_hatch, na.rm = TRUE)
    )
  )
)
# convert back to real temp
rain_mean_fki <- mean(jointdata$rainmean_fki, na.rm = TRUE)
pred_fki$x_real_rain_fki <- pred_fki$x + rain_mean_fki

# making combined fed model
combined_stages_rain_fki_plot <- ggplot(pred_fki, aes(x = x_real_rain_fki, y = predicted)) +
  geom_line(linewidth = 1, colour = "#FDD835") +
  geom_rug(data = jointdata_rain,
           aes(x = rainmean_fki),
           inherit.aes = FALSE,
           sides = "b",
           alpha = 0.3,
           colour = "#FDD835") +
  coord_cartesian(xlim=c(0,8), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(0,60,by=2))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#FDD835") +
  labs(
    x = "Mean rainfall (mm)",
    y = "Fledging success (number of chicks)",
    title = "N"
  ) +
  theme_classic(base_size = 20)

combined_stages_rain_fki_plot

### hatch model -----
pred_hatch <- as.data.frame(
  ggpredict(
    combined_model,
    terms = "rain_d_hatch [all]",
    condition = c(
      rain_b_fki = mean(jointdata_rain$rain_b_fki, na.rm = TRUE),
      rain_b_fed = mean(jointdata_rain$rain_b_fed, na.rm = TRUE)
    )
  )
)
# convert back to real temp
rain_heavy_hatch <- mean(jointdata$rainheavy_hatch, na.rm = TRUE)
pred_hatch$x_real_rain_hatch <- pred_hatch$x + rain_heavy_hatch

# making combined fed model
combined_stages_rain_hatch_plot <- ggplot(pred_hatch, aes(x = x_real_rain_hatch, y = predicted)) +
  geom_line(linewidth = 1, colour = "#41AB5D") +
  geom_rug(data = jointdata_rain,
           aes(x = rainheavy_hatch),
           inherit.aes = FALSE,
           sides = "b",
           alpha = 0.3,
           colour = "#41AB5D") +
  coord_cartesian(xlim=c(0,5), ylim=c(0,14)) +
  scale_y_continuous(breaks=seq(0,15,by=2)) +
  scale_x_continuous(breaks=seq(0,70,by=1))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#41AB5D") +
  labs(
    x = "Number of heavy rainfall days",
    y = "Fledging success (number of chicks)",
    title = "O"
  ) +
  theme_classic(base_size = 20)

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
j <- fed_rain_heavy_plot
k <- fki_rain_heavy_plot
l <- hatch_rain_heavy_plot
m <- combined_stages_rain_fed_plot
n <- combined_stages_rain_fki_plot
o <- combined_stages_rain_hatch_plot

big_rain_plot <- (
  a + b + c + d + e + f + g + h + i + j + k + l + m + n + o
) +
  plot_layout(nrow = 5, ncol = 3)

big_rain_plot


# Adding caterpillars in -----

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

# get data into correct form
caterpillardata <- caterpillardata %>%
  mutate(site = as.character(site), year = as.integer(as.character(year)))

## calc the average/range of branches at each site
branch_summary <- caterpillardata %>%
  group_by(site, year, tree) %>%
  summarise(n_branches = n(), .groups = "drop") %>%
  group_by(site, year) %>%
  summarise(
    n_trees = n_distinct(tree),
    total_branches = sum(n_branches),
    .groups = "drop"
  )

summary(branch_summary)

# group caterpillars
site_year_totals <- caterpillardata %>%
  group_by(site, year) %>%
  summarise(
    n_samples = n(),
    total_caterpillar = sum(caterpillars, na.rm=TRUE),
    total_biomass = sum(caterpillar_mass, na.rm=TRUE),
    .groups = "drop"
    ) %>%
    mutate(site = as.character(site),
           year = as.integer(as.character(year)))

# see how many caterpillars are per site per year
summary(site_year_totals$total_caterpillar)
summary(site_year_totals$total_biomass)

# filter to site-years with enough data (median total_caterpillars is 8, median total_biomass is 0.175)
caterpillardata_filtered <- caterpillardata %>%
  semi_join(
    site_year_totals %>% filter(total_caterpillar >= 8),  
    by = c("site", "year")
  )

# make weighted mean for caterpillar
caterpillar_mean_date <- caterpillardata_filtered %>%
  group_by(site, year) %>%
  summarise(
    caterpillar_date = weighted.mean(date, w = caterpillar_mass, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(site = as.character(site),
         year = as.integer(as.character(year)))

# get hatch date data
hatch_date <- jointdata_temp %>%
  group_by(site, year) %>%
  summarise(mean_hatch_date = mean(hatching_first_recorded, na.rm = TRUE),
            .groups = "drop"
  ) %>%
  mutate(site = as.character(site),
         year = as.integer(as.character(year)))

# get data in right form
jointdata_temp <- jointdata_temp %>%
  mutate(
    site = as.character(site),
    year = as.integer(as.character(year))
  )

# calculate asynchrony and standardise data
asynchrony_data <- hatch_date %>%
  left_join(caterpillar_mean_date, by = c("site", "year")) %>%
  mutate(
    chick_peak = mean_hatch_date + 10,
    asynchrony = caterpillar_date - chick_peak,
    site       = as.character(site),
    year       = as.integer(as.character(year))
  )

# merge datasets
all_async <- jointdata_temp %>%
  left_join(asynchrony_data %>% select(site, year, asynchrony),
            by = c("site", "year")) %>%
  filter(!is.na(asynchrony))

# remove any negative values
all_async$suc[all_async$suc < 0] <- NA

# centre and scale all_async
all_async <- all_async %>%
  mutate(asynchrony_c  = as.numeric(scale(asynchrony)),
         asynchrony_c2 = asynchrony_c^2)

# Making caterpillar model -----
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

# make predictions with standard errors on the link (log) scale
pred_link <- predict(async_model, newdata = pred_range, re.form = NA, 
                     type = "link", se.fit = TRUE)

pred_range$fit    <- exp(pred_link$fit)
pred_range$conf.low  <- exp(pred_link$fit - 1.96 * pred_link$se.fit)
pred_range$conf.high <- exp(pred_link$fit + 1.96 * pred_link$se.fit)

async_plot <- ggplot(all_async, aes(x=asynchrony_c, y=suc)) +
  geom_line(data = pred_range, aes(asynchrony_c, fit), linewidth = 1, colour="purple3") +
  geom_ribbon(data=pred_range, aes(x=asynchrony_c, ymin = conf.low, ymax = conf.high), inherit.aes = FALSE, alpha = 0.2, fill = "purple2") +
  geom_rug(data = all_async, aes(x = asynchrony_c), inherit.aes = FALSE, sides = "b", colour="purple3") +
  coord_cartesian(xlim=c(-3,3), ylim=c(3,8)) +
  scale_y_continuous(breaks=seq(3,8,by=1)) +
  scale_x_continuous(breaks=seq(-3,3,by=1))+
  labs(
    x = "Asynchrony (standardised)", 
    y = "Fledging success (number of chicks)",
    title = "A2") +
  theme_classic()

async_plot

# Making caterpillar and temp model -----
async_temp_model <- glmer(
  suc ~ temp_b_fed + I(temp_b_fed^2) +
    asynchrony_c + asynchrony_c2 + 
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = all_async,
  family = poisson(link = "log"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
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
    temp_b_fed = mean(all_async$temp_b_fed, na.rm = TRUE)
    )

pred_obj <- predict(
  async_temp_model, 
  newdata = pred_range, 
  re.form = NA, 
  type = "response",
  se.fit = TRUE
)

pred_range$fit      <- pred_obj$fit
pred_range$conf.low  <- pred_obj$fit - 1.96 * pred_obj$se.fit
pred_range$conf.high <- pred_obj$fit + 1.96 * pred_obj$se.fit

async_temp_plot <- ggplot(all_async, aes(x=asynchrony_c, y=suc)) +
  geom_line(data = pred_range, aes(asynchrony_c, fit), linewidth = 1, colour="purple3") +
  geom_ribbon(data=pred_range, aes(x=asynchrony_c, ymin = conf.low, ymax = conf.high), inherit.aes = FALSE, alpha = 0.2, fill = "purple2") +
  geom_rug(data = all_async, aes(x = asynchrony_c), inherit.aes = FALSE, sides = "b", colour="purple3") +
  coord_cartesian(xlim=c(-3,3), ylim=c(2,7)) +
  scale_y_continuous(breaks=seq(2,7,by=1)) +
  scale_x_continuous(breaks=seq(-3,3,by=1))+
  labs(
    x = "Asynchrony (standardised)", 
    y = "Fledging success",
    title = "B2") +
  theme_classic()

async_temp_plot

async_plot + async_temp_plot

## Temp against fledging success plot -----
# make plot
temp_async_mean <- mean(all_async$tempmeanmin_fed, na.rm = TRUE)
temp_async_sd   <- sd(all_async$tempmeanmin_fed, na.rm = TRUE)

# prediction range on scaled scale
pred_range <- data.frame(
  tempmeanmin_fed = seq(
    min(all_async$tempmeanmin_fed, na.rm = TRUE),
    max(all_async$tempmeanmin_fed, na.rm = TRUE),
    length.out = 200
  )
) %>%
  mutate(
    asynchrony_c = mean(all_async$asynchrony_c, na.rm = TRUE),
    asynchrony_c2 = asynchrony_c^2,
    temp_b_fed = (tempmeanmin_fed - temp_async_mean) / temp_async_sd
  )

# make predictions with CIs on the link scale
pred_link <- predict(async_temp_model, newdata = pred_range, re.form = NA,
                     type = "link", se.fit = TRUE)

pred_range$fit       <- exp(pred_link$fit)
pred_range$conf.low  <- exp(pred_link$fit - 1.96 * pred_link$se.fit)
pred_range$conf.high <- exp(pred_link$fit + 1.96 * pred_link$se.fit)

async_temp_2_plot <- ggplot(all_async, aes(x=tempmeanmin_fed, y=suc)) +
  geom_ribbon(data=pred_range, aes(x=tempmeanmin_fed, ymin = conf.low, ymax = conf.high), inherit.aes = FALSE, alpha = 0.2, fill = "purple2") +
  geom_line(data = pred_range, aes(tempmeanmin_fed, fit), linewidth = 1, colour="purple3") +
  geom_rug(data = all_async, aes(x = tempmeanmin_fed), inherit.aes = FALSE, sides = "b", colour="purple3") +
  coord_cartesian(xlim=c(0,12), ylim=c(2,7)) +
  scale_y_continuous(breaks=seq(2,7,by=1)) +
  scale_x_continuous(breaks=seq(0,12,by=2))+
  labs(
    x = "Mean daily minimum temperature (°C)", 
    y = "Fledging success (number of chicks)",
    title = "A") +
  theme_classic(base_size = 20)

async_temp_2_plot

## Temp against clutch size plot -----
# gaussian version
async_clutch_temp_model <- lmer(
  cs ~ temp_b_fed + I(temp_b_fed^2) +
    asynchrony_c + asynchrony_c2 +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = all_async
)

summary(async_clutch_temp_model)

# Mean used to centre temp_b_fed
temp_async_mean <- mean(jointdata$tempmeanmin_fed, na.rm = TRUE)

# Population-level predictions
pred_range <- as.data.frame(
  ggpredict(
    async_clutch_temp_model,
    terms = "temp_b_fed [all]",
    condition = c(
      asynchrony_c = mean(all_async$asynchrony_c, na.rm = TRUE)
    )
  )
)

# Convert x-axis back to °C
pred_range$tempmeanmin_fed <- pred_range$x + temp_async_mean

# Plot
async_clutch_temp_plot <- ggplot() +
  geom_ribbon(data = pred_range, aes(x = tempmeanmin_fed, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "purple2") +
  geom_line(data = pred_range, aes(x = tempmeanmin_fed, y = predicted), linewidth = 1, colour = "purple3") +
  geom_rug(data = all_async, aes(x = tempmeanmin_fed), sides = "b", colour = "purple3") +
  coord_cartesian(xlim = c(0, 12), ylim = c(5, 10)) +
  scale_y_continuous(breaks = seq(5, 12, by = 1)) +
  scale_x_continuous(breaks = seq(0, 12, by = 2)) +
  labs(
    x = "Mean daily minimum temperature (°C)",
    y = "Clutch size (number of eggs)",
    title = "B"
  ) +
  theme_classic(base_size = 20)

async_clutch_temp_plot

## Proportion of clutch that fledged plot -----
# make fail suc
all_async <- all_async[!is.na(all_async$cs) & all_async$cs > 0, ]
all_async$fail <- all_async$cs - all_async$suc

# make model
async_fledge_model <- glmer(
  cbind(suc, fail) ~ temp_b_fed + I(temp_b_fed^2) +
    asynchrony_c + asynchrony_c2 +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = all_async,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(async_fledge_model)

# check to see which optimser is best to apply 
#install.packages("optimx")
#library(optimx)
#all_fits <- allFit(async_fledge_model)
#ss <- summary(all_fits)  # compare log-likelihoods and which converge cleanly
#ss$llik # select the highest log-likelihood

temp_async_mean <- mean(all_async$tempmeanmin_fed, na.rm = TRUE)
temp_async_sd   <- sd(all_async$tempmeanmin_fed, na.rm = TRUE)

pred_range <- data.frame(
  tempmeanmin_fed = seq(
    min(all_async$tempmeanmin_fed, na.rm = TRUE),
    max(all_async$tempmeanmin_fed, na.rm = TRUE),
    length.out = 200
  )
) %>%
  mutate(
    temp_b_fed = (tempmeanmin_fed - temp_async_mean) / temp_async_sd,
    asynchrony_c  = mean(all_async$asynchrony_c, na.rm = TRUE),
    asynchrony_c2 = asynchrony_c^2
  )

# make predictions with CIs on the link scale
pred_link <- predict(async_fledge_model, newdata = pred_range, re.form = NA,
                     type = "link", se.fit = TRUE)

pred_range$fit       <- plogis(pred_link$fit) * 100
pred_range$conf.low  <- plogis(pred_link$fit - 1.96 * pred_link$se.fit) * 100
pred_range$conf.high <- plogis(pred_link$fit + 1.96 * pred_link$se.fit) * 100

async_fledge_plot_1 <- ggplot(all_async, aes(x = tempmeanmin_fed)) +
  geom_point(aes(y = (suc / (suc+fail)) * 100), alpha = 0.2, colour="purple3") +
  geom_ribbon(data=pred_range, aes(x=tempmeanmin_fed, ymin = conf.low, ymax = conf.high), inherit.aes = FALSE, alpha = 0.2, fill = "purple2") +
  geom_line(data = pred_range, aes(x = tempmeanmin_fed, y = fit), linewidth = 1, colour="purple3") +
  coord_cartesian(xlim=c(0,12), ylim=c(55,100)) +
  scale_y_continuous(breaks=seq(0,100,by=10)) +
  scale_x_continuous(breaks=seq(0,12,by=2))+
  labs(
    x = "Mean daily minimum temperature (°C)",
    y = "Percentage of clutch fledged (%)",
    title = "C"
  ) +
  theme_classic(base_size = 20)

async_fledge_plot_1

async_temp_2_plot + async_clutch_temp_plot + async_fledge_plot_1 

# Plotted against asynchrony -----
async_temp_model <- glmer(
  suc ~ temp_b_fed + I(temp_b_fed^2) +
    asynchrony_c + asynchrony_c2 + 
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = all_async,
  family = poisson(link = "log"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

## Async against fledging success -----
async_mean <- mean(all_async$asynchrony_c, na.rm = TRUE)
async_sd   <- sd(all_async$asynchrony_c, na.rm = TRUE)

# prediction range on scaled scale
pred_range <- data.frame(
  asynchrony_c = seq(
    min(all_async$asynchrony_c, na.rm = TRUE),
    max(all_async$asynchrony_c, na.rm = TRUE),
    length.out = 200
  )
) %>%
  mutate(
    asynchrony_c2 = asynchrony_c^2,
    temp_b_fed = mean(all_async$temp_b_fed, na.rm=TRUE)
  )

# make predictions with CIs on the link scale
pred_link <- predict(async_temp_model, newdata = pred_range, re.form = NA,
                     type = "link", se.fit = TRUE)

pred_range$fit       <- exp(pred_link$fit)
pred_range$conf.low  <- exp(pred_link$fit - 1.96 * pred_link$se.fit)
pred_range$conf.high <- exp(pred_link$fit + 1.96 * pred_link$se.fit)

async_temp_A_plot <- ggplot(all_async, aes(x=asynchrony_c, y=suc)) +
  geom_ribbon(data=pred_range, aes(x=asynchrony_c, ymin = conf.low, ymax = conf.high), inherit.aes = FALSE, alpha = 0.2, fill = "purple2") +
  geom_line(data = pred_range, aes(asynchrony_c, fit), linewidth = 1, colour="purple3") +
  geom_rug(data = all_async, aes(x = asynchrony_c), inherit.aes = FALSE, sides = "b", colour="purple3") +
  coord_cartesian(xlim=c(-3,3), ylim=c(2,7)) +
  scale_y_continuous(breaks=seq(2,7,by=1)) +
  scale_x_continuous(breaks=seq(-3,3,by=1))+
  labs(
    x = "Asynchrony (standardised units)", 
    y = "Fledging success (number of chicks)",
    title = "D") +
  theme_classic(base_size = 20)

async_temp_A_plot

## Async against clutch size -----
# gaussian version
async_clutch_temp_model <- lmer(
  cs ~ temp_b_fed + I(temp_b_fed^2) +
    asynchrony_c + asynchrony_c2 +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = all_async
)

# Population-level predictions
pred_range <- as.data.frame(
  ggpredict(
    async_clutch_temp_model,
    terms = "asynchrony_c [all]",
    condition = c(
      temp_b_fed = mean(all_async$temp_b_fed, na.rm=TRUE)
    )
  )
)

# Plot
async_clutch_temp_plot_A <- ggplot() +
  geom_ribbon(data = pred_range, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "purple2") +
  geom_line(data = pred_range, aes(x = x, y = predicted), linewidth = 1, colour = "purple3") +
  geom_rug(data = all_async, aes(x = asynchrony_c), sides = "b", colour = "purple3") +
  coord_cartesian(xlim = c(-3, 3), ylim = c(6, 10)) +
  scale_y_continuous(breaks = seq(5, 12, by = 1)) +
  scale_x_continuous(breaks = seq(-3, 3, by = 1)) +
  labs(
    x = "Asynchrony (standardised units)",
    y = "Clutch size (number of eggs)",
    title = "E"
  ) +
  theme_classic(base_size = 20)

async_clutch_temp_plot_A

## Async against proportion of clutch fledged -----
async_fledge_model <- glmer(
  cbind(suc, fail) ~ temp_b_fed + I(temp_b_fed^2) +
    asynchrony_c + asynchrony_c2 +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = all_async,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

async_mean <- mean(all_async$asynchrony_c, na.rm = TRUE)
async_sd   <- sd(all_async$asynchrony_c, na.rm = TRUE)

pred_range <- data.frame(
  asynchrony_c = seq(
    min(all_async$asynchrony_c, na.rm = TRUE),
    max(all_async$asynchrony_c, na.rm = TRUE),
    length.out = 200
  )
) %>%
  mutate(
    temp_b_fed = mean(all_async$temp_b_fed, na.rm=TRUE),
    asynchrony_c2 = asynchrony_c^2
  )

# make predictions with CIs on the link scale
pred_link <- predict(async_fledge_model, newdata = pred_range, re.form = NA,
                     type = "link", se.fit = TRUE)

pred_range$fit       <- plogis(pred_link$fit) * 100
pred_range$conf.low  <- plogis(pred_link$fit - 1.96 * pred_link$se.fit) * 100
pred_range$conf.high <- plogis(pred_link$fit + 1.96 * pred_link$se.fit) * 100

async_fledge_plot_A <- ggplot(all_async, aes(x = asynchrony_c)) +
  geom_point(aes(y = (suc / (suc+fail)) * 100), alpha = 0.2, colour="purple3") +
  geom_ribbon(data=pred_range, aes(x=asynchrony_c, ymin = conf.low, ymax = conf.high), inherit.aes = FALSE, alpha = 0.2, fill = "purple2") +
  geom_line(data = pred_range, aes(x = asynchrony_c, y = fit), linewidth = 1, colour="purple3") +
  coord_cartesian(xlim=c(-3,3), ylim=c(35,100)) +
  scale_y_continuous(breaks=seq(0,100,by=10)) +
  scale_x_continuous(breaks=seq(-3,3,by=1))+
  labs(
    x = "Asynchrony (standardised units)",
    y = "Percentage of clutch fledged (%)",
    title = "F"
  ) +
  theme_classic(base_size = 20)

async_fledge_plot_A


(async_temp_2_plot + async_clutch_temp_plot + async_fledge_plot_1) /
(async_temp_A_plot + async_clutch_temp_plot_A + async_fledge_plot_A)


