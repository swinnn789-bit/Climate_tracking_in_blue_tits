#For a particular time period - e.g., egg-laying (and defined time period after)

#1. Select a focal climate metric. The climate metric that was found to be most important for each period.
# mean of minima for fed, mean of minima for fki, min for hatch

#### Observation model
#2. Obtain that climate data that's pertinent to each nesting attempt (you already have this)

#3. Construct a mixed effects model with the climate variable as a response. 
#You need to come up withe a first pass of what your fixed and random terms are.

#That model is your observed model. If they are tracking the hypothesis is that temperature won't change much with gradients or across key random terms.

####
#Null model
#4. For the focal period, find the mean start date. Then obtain the climate data for every nest that corresponds to this shared start date and the fixed duration after.

#5. Run the same statistical model as you do for the observed data.

#6. Generate metrics of tracking. Plot metrics for observed model versus null model. 

##### Making the observational model #####
##### Get datasets and packages #####
library(dplyr)
library(lme4)
library(Matrix)
library(ggeffects)
library(ggplot2)
library(patchwork)

## nest data
nestdata <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Thesis/Data play/Bird_Phenology.csv")

unique(nestdata$site)

# remove clutch swap treatment
nestdata <- nestdata %>%
  filter(
    is.na(clutch.swap.treatment) |
      clutch.swap.treatment == "" |
      clutch.swap.treatment == "unmanipulated"
  )

## temp data
loggerdata <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Thesis/Data play/temperatures.csv")
# remove columns without temp measurements
loggerdata <- loggerdata[, !(names(loggerdata) %in% c("logger_id", "logger_res"))]

## ring dataset
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

##### Making loops for the best fitting temp metrics for each stage #####
## fed mean of minima data ##
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

## fki mean of minima data ##
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

## hatch min data ##
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

# filter to entries with hatching_first_recorded
jointdata <- nestdata %>%
  filter(!is.na(hatching_first_recorded),
         hatching_first_recorded !="")

# remove any negative values
jointdata$suc[jointdata$suc < 0] <- NA

## coord data
coords <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Thesis/Data play/nestlocations.csv")
summary(coords$site) 
coords %>% count(site)

# number of sites with 8 nestboxes
site_counts <- coords %>%
  count(site) %>%
  summarise(
    total_sites = n(),
    sites_with_8 = sum(n == 8),
    percent_with_8 = (sites_with_8 / total_sites) * 100
  )

site_counts

coords %>% filter(is.na(lat) | is.na(elevation)) # KBC has no elevation data 

# extracting elevation and latitude data # remove KBC
nestbox_coords <- coords %>%
  filter(site != "KBC") %>%
  select(site, box, lat, elevation) %>%
  distinct(site, box, .keep_all = TRUE)

summary(nestbox_coords)

# add to jointdata
jointdata <- jointdata %>%
  left_join(nestbox_coords, by = c("site", "box"))
names(jointdata)

## scale and centre
jointdata$temp_b_fed <- scale(jointdata$tempmeanmin_fed, center = TRUE, scale = TRUE)[,1]
jointdata$temp_b_fki <- scale(jointdata$tempmeanmin_fki, center = TRUE, scale = TRUE)[,1]
jointdata$temp_a_hatch <- scale(jointdata$tempmin_hatch, center = TRUE, scale = TRUE)[,1]

# make big dataset
vars_needed <- c(
  "suc",
  "temp_b_fed", "tempmeanmin_fed",  # fed predictors 
  "temp_b_fki", "tempmeanmin_fki",  # fki predictors 
  "temp_a_hatch", "tempmin_hatch",  # hatch predictors
  "site", "year", "site_year", "female",
  "lat", "elevation")

# Check all exist
vars_needed[!vars_needed %in% names(jointdata)]

# Create common complete-case dataset
jointdata_common <- jointdata[complete.cases(jointdata[, vars_needed]), ]
names(jointdata_common)

# make year numeric
jointdata_common$year_num <- as.numeric(as.character(jointdata_common$year))

##### Making the fed model #####
# using meanmin
fed_obs_model <- lmer(
  tempmeanmin_fed ~ year_num + elevation + lat +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_common,
  REML = TRUE
)

summary(fed_obs_model)

VarCorr(fed_obs_model)

##### Making the fed year plot #####
# make predictions 
pred_range_fed <- data.frame(
  year_num = seq(
    min(jointdata_common$year_num, na.rm = TRUE),
    max(jointdata_common$year_num, na.rm = TRUE),
    length.out = 100
  )
) %>%
  mutate(
    elevation = mean(jointdata_common$elevation, na.rm = TRUE),
    lat       = mean(jointdata_common$lat,       na.rm = TRUE)
  )

# predictions
pred_link_fed <- predict(fed_obs_model, newdata = pred_range_fed, re.form = NA,
                     type = "response", se.fit = TRUE)

pred_range_fed$fit       <- pred_link_fed$fit
pred_range_fed$conf.low  <- pred_link_fed$fit - 1.96 * pred_link_fed$se.fit
pred_range_fed$conf.high <- pred_link_fed$fit + 1.96 * pred_link_fed$se.fit

# year plot
fed_obs_plot <- ggplot(jointdata_common, aes(x = year_num, y = tempmeanmin_fed)) +
  geom_jitter(alpha = 0.2, width = 0.2, colour = "grey40") +
  geom_ribbon(data = pred_range_fed, aes(x = year_num, ymin = conf.low, ymax = conf.high),
              inherit.aes = FALSE, alpha = 0.2, fill = "#2C7FB8") +
  geom_line(data = pred_range_fed, aes(x = year_num, y = fit),
            inherit.aes = FALSE, linewidth = 1, colour = "#2C7FB8") +
  scale_y_continuous(breaks=seq(0,12,by=2)) +
  scale_x_continuous(breaks=seq(2014,2025,by=1))+
  labs(
    x = "Year",
    y = "Mean of daily minima temperature (°C)",
    title = "fed period — observed thermal niche"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fed_obs_plot

##### Making the fed latitude plot #####
# make predictions 
pred_range_fed_lat <- data.frame(
  lat = seq(
    min(jointdata_common$lat, na.rm = TRUE),
    max(jointdata_common$lat, na.rm = TRUE),
    length.out = 100
  )
) %>%
  mutate(
    elevation = mean(jointdata_common$elevation, na.rm = TRUE),
    year_num  = mean(jointdata_common$year_num, na.rm = TRUE)
  )

# predictions
pred_link_fed_lat <- predict(fed_obs_model, newdata = pred_range_fed_lat, re.form = NA,
                         type = "response", se.fit = TRUE)

pred_range_fed_lat$fit       <- pred_link_fed_lat$fit
pred_range_fed_lat$conf.low  <- pred_link_fed_lat$fit - 1.96 * pred_link_fed_lat$se.fit
pred_range_fed_lat$conf.high <- pred_link_fed_lat$fit + 1.96 * pred_link_fed_lat$se.fit

# lat plot
fed_obs_lat_plot <- ggplot(jointdata_common, aes(x = lat, y = tempmeanmin_fed)) +
  geom_jitter(alpha = 0.2, width = 0.001, colour = "grey40") +
  geom_ribbon(data = pred_range_fed_lat, aes(x = lat, ymin = conf.low, ymax = conf.high),
              inherit.aes = FALSE, alpha = 0.2, fill = "#2C7FB8") +
  geom_line(data = pred_range_fed_lat, aes(x = lat, y = fit),
            inherit.aes = FALSE, linewidth = 1, colour = "#2C7FB8") +
  coord_cartesian(xlim=c(56,58), ylim=c(0,12)) +
  scale_y_continuous(breaks=seq(0,12,by=2)) +
  scale_x_continuous(breaks=seq(50,60,by=1))+
  labs(
    x = "Latitude (°N)",
    y = "Mean of daily minima temperature (°C)",
    title = "fed period — observed thermal niche"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fed_obs_lat_plot

##### Making the fed elevation plot #####
# make predictions 
pred_range_fed_elev <- data.frame(
  elevation = seq(
    min(jointdata_common$elevation, na.rm = TRUE),
    max(jointdata_common$elevation, na.rm = TRUE),
    length.out = 100
  )
) %>%
  mutate(
    lat = mean(jointdata_common$lat, na.rm = TRUE),
    year_num  = mean(jointdata_common$year_num, na.rm = TRUE)
  )

# predictions
pred_link_fed_elev <- predict(fed_obs_model, newdata = pred_range_fed_elev, re.form = NA,
                             type = "response", se.fit = TRUE)

pred_range_fed_elev$fit       <- pred_link_fed_elev$fit
pred_range_fed_elev$conf.low  <- pred_link_fed_elev$fit - 1.96 * pred_link_fed_elev$se.fit
pred_range_fed_elev$conf.high <- pred_link_fed_elev$fit + 1.96 * pred_link_fed_elev$se.fit

# elev plot
fed_obs_elev_plot <- ggplot(jointdata_common, aes(x = elevation, y = tempmeanmin_fed)) +
  geom_jitter(alpha = 0.2, width = 0.5, colour = "grey40") +
  geom_ribbon(data = pred_range_fed_elev, aes(x = elevation, ymin = conf.low, ymax = conf.high),
              inherit.aes = FALSE, alpha = 0.2, fill = "#2C7FB8") +
  geom_line(data = pred_range_fed_elev, aes(x = elevation, y = fit),
            inherit.aes = FALSE, linewidth = 1, colour = "#2C7FB8") +
  scale_y_continuous(breaks=seq(0,12,by=2)) +
  scale_x_continuous(breaks=seq(0,600,by=50))+
  labs(
    x = "Elevation (m)",
    y = "Mean of daily minima temperature (°C)",
    title = "fed period — observed thermal niche"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fed_obs_elev_plot

##### Making the fki model #####
# using meanmin
fki_obs_model <- lmer(
  tempmeanmin_fki ~ year_num + elevation + lat +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_common,
  REML = TRUE
)

summary(fki_obs_model)

VarCorr(fki_obs_model)

##### Making the fki plot #####
# make predictions 
pred_range_fki <- data.frame(
  year_num = seq(
    min(jointdata_common$year_num, na.rm = TRUE),
    max(jointdata_common$year_num, na.rm = TRUE),
    length.out = 100
  )
) %>%
  mutate(
    elevation = mean(jointdata_common$elevation, na.rm = TRUE),
    lat       = mean(jointdata_common$lat,       na.rm = TRUE)
  )

# predictions
pred_link_fki <- predict(fki_obs_model, newdata = pred_range_fki, re.form = NA,
                     type = "response", se.fit = TRUE)

pred_range_fki$fit       <- pred_link_fki$fit
pred_range_fki$conf.low  <- pred_link_fki$fit - 1.96 * pred_link_fki$se.fit
pred_range_fki$conf.high <- pred_link_fki$fit + 1.96 * pred_link_fki$se.fit

# plot
fki_obs_plot <- ggplot(jointdata_common, aes(x = year_num, y = tempmeanmin_fki)) +
  geom_jitter(alpha = 0.2, width = 0.2, colour = "grey40") +
  geom_ribbon(data = pred_range_fki, aes(x = year_num, ymin = conf.low, ymax = conf.high),
              inherit.aes = FALSE, alpha = 0.2, fill = "#FDD835") +
  geom_line(data = pred_range_fki, aes(x = year_num, y = fit),
            inherit.aes = FALSE, linewidth = 1, colour = "#FDD835") +
  scale_y_continuous(breaks=seq(0,12,by=2)) +
  scale_x_continuous(breaks=seq(2014,2025,by=1))+
  labs(
    x = "Year",
    y = "Mean of daily minima temperature (°C)",
    title = "fki period — observed thermal niche"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fki_obs_plot

##### Making the fki latitude plot #####
# make predictions 
pred_range_fki_lat <- data.frame(
  lat = seq(
    min(jointdata_common$lat, na.rm = TRUE),
    max(jointdata_common$lat, na.rm = TRUE),
    length.out = 100
  )
) %>%
  mutate(
    elevation = mean(jointdata_common$elevation, na.rm = TRUE),
    year_num  = mean(jointdata_common$year_num, na.rm = TRUE)
  )

# predictions
pred_link_fki_lat <- predict(fki_obs_model, newdata = pred_range_fki_lat, re.form = NA,
                             type = "response", se.fit = TRUE)

pred_range_fki_lat$fit       <- pred_link_fki_lat$fit
pred_range_fki_lat$conf.low  <- pred_link_fki_lat$fit - 1.96 * pred_link_fki_lat$se.fit
pred_range_fki_lat$conf.high <- pred_link_fki_lat$fit + 1.96 * pred_link_fki_lat$se.fit

# lat plot
fki_obs_lat_plot <- ggplot(jointdata_common, aes(x = lat, y = tempmeanmin_fki)) +
  geom_jitter(alpha = 0.2, width = 0.001, colour = "grey40") +
  geom_ribbon(data = pred_range_fki_lat, aes(x = lat, ymin = conf.low, ymax = conf.high),
              inherit.aes = FALSE, alpha = 0.2, fill = "#FDD835") +
  geom_line(data = pred_range_fki_lat, aes(x = lat, y = fit),
            inherit.aes = FALSE, linewidth = 1, colour = "#FDD835") +
  coord_cartesian(xlim=c(56,58), ylim=c(0,12)) +
  scale_y_continuous(breaks=seq(0,12,by=2)) +
  scale_x_continuous(breaks=seq(50,60,by=1))+
  labs(
    x = "Latitude (°N)",
    y = "Mean of daily minima temperature (°C)",
    title = "fki period — observed thermal niche"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fki_obs_lat_plot

##### Making the fki elevation plot #####
# make predictions 
pred_range_fki_elev <- data.frame(
  elevation = seq(
    min(jointdata_common$elevation, na.rm = TRUE),
    max(jointdata_common$elevation, na.rm = TRUE),
    length.out = 100
  )
) %>%
  mutate(
    lat = mean(jointdata_common$lat, na.rm = TRUE),
    year_num  = mean(jointdata_common$year_num, na.rm = TRUE)
  )

# predictions
pred_link_fki_elev <- predict(fki_obs_model, newdata = pred_range_fki_elev, re.form = NA,
                              type = "response", se.fit = TRUE)

pred_range_fki_elev$fit       <- pred_link_fki_elev$fit
pred_range_fki_elev$conf.low  <- pred_link_fki_elev$fit - 1.96 * pred_link_fki_elev$se.fit
pred_range_fki_elev$conf.high <- pred_link_fki_elev$fit + 1.96 * pred_link_fki_elev$se.fit

# lat plot
fki_obs_elev_plot <- ggplot(jointdata_common, aes(x = elevation, y = tempmeanmin_fki)) +
  geom_jitter(alpha = 0.2, width = 0.5, colour = "grey40") +
  geom_ribbon(data = pred_range_fki_elev, aes(x = elevation, ymin = conf.low, ymax = conf.high),
              inherit.aes = FALSE, alpha = 0.2, fill = "#FDD835") +
  geom_line(data = pred_range_fki_elev, aes(x = elevation, y = fit),
            inherit.aes = FALSE, linewidth = 1, colour = "#FDD835") +
  scale_y_continuous(breaks=seq(0,12,by=2)) +
  scale_x_continuous(breaks=seq(0,600,by=50))+
  labs(
    x = "Elevation (m)",
    y = "Mean of daily minima temperature (°C)",
    title = "fki period — observed thermal niche"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fki_obs_elev_plot

##### Making the hatch model #####
# using min
hatch_obs_model <- lmer(
  tempmin_hatch ~ year_num + elevation + lat +
    (1 | site) +
    (1 | year) +
    (1 | site_year) +
    (1 | female),
  data = jointdata_common,
  REML = TRUE
)

summary(hatch_obs_model)

VarCorr(hatch_obs_model)

##### Making the hatch plot #####
# make predictions 
pred_range_hatch <- data.frame(
  year_num = seq(
    min(jointdata_common$year_num, na.rm = TRUE),
    max(jointdata_common$year_num, na.rm = TRUE),
    length.out = 100
  )
) %>%
  mutate(
    elevation = mean(jointdata_common$elevation, na.rm = TRUE),
    lat       = mean(jointdata_common$lat,       na.rm = TRUE)
  )

# predictions
pred_link_hatch <- predict(hatch_obs_model, newdata = pred_range_hatch, re.form = NA,
                     type = "response", se.fit = TRUE)

pred_range_hatch$fit       <- pred_link_hatch$fit
pred_range_hatch$conf.low  <- pred_link_hatch$fit - 1.96 * pred_link_hatch$se.fit
pred_range_hatch$conf.high <- pred_link_hatch$fit + 1.96 * pred_link_hatch$se.fit

# plot
hatch_obs_plot <- ggplot(jointdata_common, aes(x = year_num, y = tempmin_hatch)) +
  geom_jitter(alpha = 0.2, width = 0.2, colour = "grey40") +
  geom_ribbon(data = pred_range_hatch, aes(x = year_num, ymin = conf.low, ymax = conf.high),
              inherit.aes = FALSE, alpha = 0.2, fill = "#41AB5D") +
  geom_line(data = pred_range_hatch, aes(x = year_num, y = fit),
            inherit.aes = FALSE, linewidth = 1, colour = "#41AB5D") +
  scale_y_continuous(breaks=seq(0,12,by=2)) +
  scale_x_continuous(breaks=seq(2014,2025,by=1))+
  labs(
    x = "Year",
    y = "Minimum temperature (°C)",
    title = "hatch period — observed thermal niche"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

hatch_obs_plot

##### Making the hatch latitude plot #####
# make predictions 
pred_range_hatch_lat <- data.frame(
  lat = seq(
    min(jointdata_common$lat, na.rm = TRUE),
    max(jointdata_common$lat, na.rm = TRUE),
    length.out = 100
  )
) %>%
  mutate(
    elevation = mean(jointdata_common$elevation, na.rm = TRUE),
    year_num  = mean(jointdata_common$year_num, na.rm = TRUE)
  )

# predictions
pred_link_hatch_lat <- predict(hatch_obs_model, newdata = pred_range_hatch_lat, re.form = NA,
                             type = "response", se.fit = TRUE)

pred_range_hatch_lat$fit       <- pred_link_hatch_lat$fit
pred_range_hatch_lat$conf.low  <- pred_link_hatch_lat$fit - 1.96 * pred_link_hatch_lat$se.fit
pred_range_hatch_lat$conf.high <- pred_link_hatch_lat$fit + 1.96 * pred_link_hatch_lat$se.fit

# lat plot
hatch_obs_lat_plot <- ggplot(jointdata_common, aes(x = lat, y = tempmin_hatch)) +
  geom_jitter(alpha = 0.2, width = 0.001, colour = "grey40") +
  geom_ribbon(data = pred_range_hatch_lat, aes(x = lat, ymin = conf.low, ymax = conf.high),
              inherit.aes = FALSE, alpha = 0.2, fill = "#41AB5D") +
  geom_line(data = pred_range_hatch_lat, aes(x = lat, y = fit),
            inherit.aes = FALSE, linewidth = 1, colour = "#41AB5D") +
  coord_cartesian(xlim=c(56,58), ylim=c(0,12)) +
  scale_y_continuous(breaks=seq(0,12,by=2)) +
  scale_x_continuous(breaks=seq(50,60,by=1))+
  labs(
    x = "Latitude (°N)",
    y = "Minimum temperature (°C)",
    title = "hatch period — observed thermal niche"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

hatch_obs_lat_plot

##### Making the hatch elevation plot #####
# make predictions 
pred_range_hatch_elev <- data.frame(
  elevation = seq(
    min(jointdata_common$elevation, na.rm = TRUE),
    max(jointdata_common$elevation, na.rm = TRUE),
    length.out = 100
  )
) %>%
  mutate(
    lat = mean(jointdata_common$lat, na.rm = TRUE),
    year_num  = mean(jointdata_common$year_num, na.rm = TRUE)
  )

# predictions
pred_link_hatch_elev <- predict(hatch_obs_model, newdata = pred_range_hatch_elev, re.form = NA,
                              type = "response", se.fit = TRUE)

pred_range_hatch_elev$fit       <- pred_link_hatch_elev$fit
pred_range_hatch_elev$conf.low  <- pred_link_hatch_elev$fit - 1.96 * pred_link_hatch_elev$se.fit
pred_range_hatch_elev$conf.high <- pred_link_hatch_elev$fit + 1.96 * pred_link_hatch_elev$se.fit

# lat plot
hatch_obs_elev_plot <- ggplot(jointdata_common, aes(x = elevation, y = tempmin_hatch)) +
  geom_jitter(alpha = 0.2, width = 0.5, colour = "grey40") +
  geom_ribbon(data = pred_range_hatch_elev, aes(x = elevation, ymin = conf.low, ymax = conf.high),
              inherit.aes = FALSE, alpha = 0.2, fill = "#41AB5D") +
  geom_line(data = pred_range_hatch_elev, aes(x = elevation, y = fit),
            inherit.aes = FALSE, linewidth = 1, colour = "#41AB5D") +
  scale_y_continuous(breaks=seq(0,12,by=2)) +
  scale_x_continuous(breaks=seq(0,600,by=50))+
  labs(
    x = "Elevation (m)",
    y = "Minimum temperature (°C)",
    title = "hatch period — observed thermal niche"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

hatch_obs_elev_plot

fed_obs_plot + fki_obs_plot + hatch_obs_plot

fed_obs_lat_plot + fki_obs_lat_plot + hatch_obs_lat_plot

fed_obs_elev_plot + fki_obs_elev_plot + hatch_obs_elev_plot

##### Making the null model #####
##### Making the null fed model #####

# calculate the overall mean fed start date 
overall_mean_fed <- mean(nestdata$fed, na.rm = TRUE)

# make loop 
tempnull_fed <- rep(NA, nrow(nestdata))

for (x in 1:nrow(nestdata)) {
  site.i <- nestdata$site[x]
  year.i <- nestdata$year[x]
  # no box.i as this doesn't add anything
  
  # using the mean start date 
  null_start <- overall_mean_fed
  null_end   <- null_start + 10.16
  
  # skip if mean fed is missing for that year
  if (length(null_start) == 0 || is.na(null_start)) next
  
  cols.use <- which(temp.times >= null_start & temp.times <= null_end)
  if (length(cols.use) == 0) next
  
  rows.use <- which(loggerdata$site == site.i & loggerdata$year == year.i)
  if (length(rows.use) == 0) next
  
  temp.sub    <- as.matrix(loggerdata[rows.use, temp.cols[cols.use]])
  hourly.temp <- if (nrow(temp.sub) > 1) colMeans(temp.sub, na.rm = TRUE) else as.numeric(temp.sub)
  
  day.id    <- floor(temp.times[cols.use])
  daily.min <- tapply(hourly.temp, day.id, min)
  tempnull_fed[x] <- mean(daily.min)
}

# add null temp to nestdata and jointdata
nestdata$tempnull_fed <- tempnull_fed
jointdata_common$tempnull_fed <- nestdata$tempnull_fed[
  match(
    paste(jointdata_common$site, jointdata_common$year, jointdata_common$box, sep = "_"),
    paste(nestdata$site, nestdata$year, nestdata$box, sep = "_")
  )
]

# fed null model # NO FEMALE OR SITE_YEAR TERM
fed_null_model <- lmer(
  tempnull_fed ~ year_num + elevation + lat +
    (1 | site) +
    (1 | year),
  data = jointdata_common,
  REML = TRUE
)

summary(fed_null_model)
VarCorr(fed_null_model)

##### Making the fed null plot #####
# make predictions
pred_range_null_fed <- data.frame(
  year_num = seq(
    min(jointdata_common$year_num, na.rm = TRUE),
    max(jointdata_common$year_num, na.rm = TRUE),
    length.out = 100
  )
) %>%
  mutate(
    elevation = mean(jointdata_common$elevation, na.rm = TRUE),
    lat       = mean(jointdata_common$lat,       na.rm = TRUE)
  )

pred_null_fed <- predict(fed_null_model, newdata = pred_range_null_fed, re.form = NA,
                     type = "response", se.fit = TRUE)

pred_range_null_fed$fit       <- pred_null_fed$fit
pred_range_null_fed$conf.low  <- pred_null_fed$fit - 1.96 * pred_null_fed$se.fit
pred_range_null_fed$conf.high <- pred_null_fed$fit + 1.96 * pred_null_fed$se.fit

# compare obs and null models
pred_range_fed$model       <- "Observed"
pred_range_null_fed$model  <- "Null"
pred_combined_fed          <- bind_rows(pred_range_fed, pred_range_null_fed)

# make null and obs dataset
raw_points_fed <- bind_rows(
  jointdata_common %>% select(year_num, tempmeanmin_fed) %>%
    rename(temp = tempmeanmin_fed) %>% mutate(model = "Observed"),
  jointdata_common %>% select(year_num, tempnull_fed) %>%
    rename(temp = tempnull_fed) %>% mutate(model = "Null")
)

# summarise mean and sd of temp per year, per model
year_summary_fed <- raw_points_fed %>%
  group_by(year_num, model) %>%
  summarise(
    mean_temp = mean(temp, na.rm = TRUE),
    sd_temp   = sd(temp, na.rm = TRUE),
    n = sum(!is.na(temp)),
    se_temp = sd_temp / sqrt(n),
    .groups = "drop"
  )
# make plot
fed_obs_null_plot <- ggplot() +
  geom_ribbon(data = pred_combined_fed,
              aes(x = year_num, ymin = conf.low, ymax = conf.high, fill = model),
              inherit.aes = FALSE, alpha = 0.2) +
  geom_line(data = pred_combined_fed,
            aes(x = year_num, y = fit, colour = model),
            inherit.aes = FALSE, linewidth = 1) +
  geom_errorbar(data = year_summary_fed,
                aes(x = year_num,
                    ymin = mean_temp - se_temp, ymax = mean_temp + se_temp,
                    colour = model),
                position = position_dodge(width = 0.5),
                width = 0.3,      
                linewidth = 1) +
  geom_point(data = year_summary_fed,
             aes(x = year_num, y = mean_temp, colour = model),
             position = position_dodge(width = 0.5),
             size = 2.5) +
  scale_fill_manual(values   = c("Observed" = "royalblue", "Null" = "purple4")) +
  scale_colour_manual(values = c("Observed" = "royalblue", "Null" = "purple4")) +
  scale_y_continuous(breaks = seq(0, 12, by = 2)) +
  scale_x_continuous(breaks = seq(2014, 2025, by = 1)) +
  labs(x      = "Year",
       y      = "Mean daily minimum temperature (°C)",
       title  = "A",
       colour = "Model",
       fill   = "Model"
  ) +
  theme_classic(base_size = 25) +
  theme(legend.position = "bottom") +
theme(axis.text.x = element_text(angle = 45, hjust = 1))

fed_obs_null_plot

##### Making the fed null latitude plot #####
# make predictions over lat, holding year_num and elevation at their means
pred_range_null_fed_lat <- data.frame(
  lat = seq(
    min(jointdata_common$lat, na.rm = TRUE),
    max(jointdata_common$lat, na.rm = TRUE),
    length.out = 100
  )
) %>%
  mutate(
    year_num  = mean(jointdata_common$year_num,  na.rm = TRUE),
    elevation = mean(jointdata_common$elevation, na.rm = TRUE)
  )

pred_null_fed_lat <- predict(fed_null_model, newdata = pred_range_null_fed_lat, re.form = NA,
                             type = "response", se.fit = TRUE)

pred_range_null_fed_lat$fit       <- pred_null_fed_lat$fit
pred_range_null_fed_lat$conf.low  <- pred_null_fed_lat$fit - 1.96 * pred_null_fed_lat$se.fit
pred_range_null_fed_lat$conf.high <- pred_null_fed_lat$fit + 1.96 * pred_null_fed_lat$se.fit

# combine observed and null
pred_range_fed_lat$model       <- "Observed"
pred_range_null_fed_lat$model  <- "Null"
pred_combined_fed_lat          <- bind_rows(pred_range_fed_lat, pred_range_null_fed_lat)

# make null and obs dataset
raw_points_fed_lat <- bind_rows(
  jointdata_common %>% select(lat, tempmeanmin_fed) %>%
    rename(temp = tempmeanmin_fed) %>% mutate(model = "Observed"),
  jointdata_common %>% select(lat, tempnull_fed) %>%
    rename(temp = tempnull_fed) %>% mutate(model = "Null")
)

# make plot
fed_obs_null_lat_plot <- ggplot() +
  geom_jitter(data = raw_points_fed_lat,
              aes(x = lat, y = temp, colour = model),
              alpha = 0.1, width = 0.2, size = 3) +
  geom_ribbon(data = pred_combined_fed_lat,
              aes(x = lat, ymin = conf.low, ymax = conf.high, fill = model),
              inherit.aes = FALSE, alpha = 0.2, size = 3) +
  geom_line(data = pred_combined_fed_lat,
            aes(x = lat, y = fit, colour = model),
            inherit.aes = FALSE, linewidth = 2) +
  scale_fill_manual(values   = c("Observed" = "royalblue", "Null" = "purple4")) +
  scale_colour_manual(values = c("Observed" = "royalblue", "Null" = "purple4")) +
  scale_y_continuous(breaks = seq(0, 12, by = 2)) +
  scale_x_continuous(breaks = seq(50, 60, by = 1)) +
  labs(
    x      = "Latitude (°N)",
    y      = "Mean daily minimum temperature (°C)",
    title  = "A",
    colour = "Model",
    fill   = "Model"
  ) +
  theme_classic(25) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fed_obs_null_lat_plot

##### Making the fed null elevation plot #####
# make predictions over elevation, holding year_num and lat at their means
pred_range_null_fed_elev <- data.frame(
  elevation = seq(
    min(jointdata_common$elevation, na.rm = TRUE),
    max(jointdata_common$elevation, na.rm = TRUE),
    length.out = 100
  )
) %>%
  mutate(
    year_num = mean(jointdata_common$year_num, na.rm = TRUE),
    lat      = mean(jointdata_common$lat, na.rm = TRUE)
  )

pred_null_fed_elev <- predict(fed_null_model, newdata = pred_range_null_fed_elev, re.form = NA,
                              type = "response", se.fit = TRUE)

pred_range_null_fed_elev$fit       <- pred_null_fed_elev$fit
pred_range_null_fed_elev$conf.low  <- pred_null_fed_elev$fit - 1.96 * pred_null_fed_elev$se.fit
pred_range_null_fed_elev$conf.high <- pred_null_fed_elev$fit + 1.96 * pred_null_fed_elev$se.fit

# combine observed and null
pred_range_fed_elev$model       <- "Observed"
pred_range_null_fed_elev$model  <- "Null"
pred_combined_fed_elev          <- bind_rows(pred_range_fed_elev, pred_range_null_fed_elev)

# make null and obs dataset
raw_points_fed_elev <- bind_rows(
  jointdata_common %>% select(elevation, tempmeanmin_fed) %>%
    rename(temp = tempmeanmin_fed) %>% mutate(model = "Observed"),
  jointdata_common %>% select(elevation, tempnull_fed) %>%
    rename(temp = tempnull_fed) %>% mutate(model = "Null")
)

# make plot
fed_obs_null_elev_plot <- ggplot() +
  geom_jitter(data = raw_points_fed_elev,
              aes(x = elevation, y = temp, colour = model),
              alpha = 0.1, width = 0.2, size = 3) +
  geom_ribbon(data = pred_combined_fed_elev,
              aes(x = elevation, ymin = conf.low, ymax = conf.high, fill = model),
              inherit.aes = FALSE, alpha = 0.2, size = 3) +
  geom_line(data = pred_combined_fed_elev,
            aes(x = elevation, y = fit, colour = model),
            inherit.aes = FALSE, linewidth = 2) +
  scale_fill_manual(values   = c("Observed" = "royalblue", "Null" = "purple4")) +
  scale_colour_manual(values = c("Observed" = "royalblue", "Null" = "purple4")) +
  scale_y_continuous(breaks = seq(0, 12, by = 2)) +
  scale_x_continuous(breaks = seq(0, 500, by = 50)) +
  labs(
    x      = "Elevation (m)",
    y      = "Mean daily minimum temperature (°C)",
    title  = "A",
    colour = "Model",
    fill   = "Model"
  ) +
  theme_classic(25) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fed_obs_null_elev_plot

##### Making the null fki model #####

# calculate the overall mean fki start date 
overall_mean_fki <- mean(nestdata$fki, na.rm = TRUE)

# make loop to get temperature for each nest using the year-specific mean fki date
tempnull_fki <- rep(NA, nrow(nestdata))

for (x in 1:nrow(nestdata)) {
  site.i <- nestdata$site[x]
  year.i <- nestdata$year[x]
  # no box.i as this doesn't add anything
  
  # using the year-specific mean start date 
  null_start <- overall_mean_fki
  null_end   <- null_start + 13.32
  
  # skip if mean fed is missing for that year
  if (length(null_start) == 0 || is.na(null_start)) next
  
  cols.use <- which(temp.times >= null_start & temp.times <= null_end)
  if (length(cols.use) == 0) next
  
  rows.use <- which(loggerdata$site == site.i & loggerdata$year == year.i)
  if (length(rows.use) == 0) next
  
  temp.sub    <- as.matrix(loggerdata[rows.use, temp.cols[cols.use]])
  hourly.temp <- if (nrow(temp.sub) > 1) colMeans(temp.sub, na.rm = TRUE) else as.numeric(temp.sub)
  
  day.id    <- floor(temp.times[cols.use])
  daily.min <- tapply(hourly.temp, day.id, min)
  tempnull_fki[x] <- mean(daily.min)
}

# add null temp to nestdata and jointdata
nestdata$tempnull_fki <- tempnull_fki
jointdata_common$tempnull_fki <- nestdata$tempnull_fki[
  match(
    paste(jointdata_common$site, jointdata_common$year, jointdata_common$box, sep = "_"),
    paste(nestdata$site, nestdata$year, nestdata$box, sep = "_")
  )
]

# fki null model # REMOVED FEMALE AND SITE_YEAR
fki_null_model <- lmer(
  tempnull_fki ~ year_num + elevation + lat +
    (1 | site) +
    (1 | year),
  data = jointdata_common,
  REML = TRUE
)

summary(fki_null_model)
VarCorr(fki_null_model)

##### Making the fki null plot #####
# make predictions
pred_range_null_fki <- data.frame(
  year_num = seq(
    min(jointdata_common$year_num, na.rm = TRUE),
    max(jointdata_common$year_num, na.rm = TRUE),
    length.out = 100
  )
) %>%
  mutate(
    elevation = mean(jointdata_common$elevation, na.rm = TRUE),
    lat       = mean(jointdata_common$lat,       na.rm = TRUE)
  )

pred_null_fki <- predict(fki_null_model, newdata = pred_range_null_fki, re.form = NA,
                         type = "response", se.fit = TRUE)

pred_range_null_fki$fit       <- pred_null_fki$fit
pred_range_null_fki$conf.low  <- pred_null_fki$fit - 1.96 * pred_null_fki$se.fit
pred_range_null_fki$conf.high <- pred_null_fki$fit + 1.96 * pred_null_fki$se.fit

# compare obs and null models
pred_range_fki$model       <- "Observed"
pred_range_null_fki$model  <- "Null"
pred_combined_fki          <- bind_rows(pred_range_fki, pred_range_null_fki)

# make null and obs dataset
raw_points_fki <- bind_rows(
  jointdata_common %>% select(year_num, tempmeanmin_fki) %>%
    rename(temp = tempmeanmin_fki) %>% mutate(model = "Observed"),
  jointdata_common %>% select(year_num, tempnull_fki) %>%
    rename(temp = tempnull_fki) %>% mutate(model = "Null")
)

# summarise mean and sd of temp per year, per model
year_summary_fki <- raw_points_fki %>%
  group_by(year_num, model) %>%
  summarise(
    mean_temp = mean(temp, na.rm = TRUE),
    sd_temp   = sd(temp, na.rm = TRUE),
    n = sum(!is.na(temp)),
    se_temp = sd_temp / sqrt(n),
    .groups = "drop"
  )
# make plot
fki_obs_null_plot <- ggplot() +
  geom_ribbon(data = pred_combined_fki,
              aes(x = year_num, ymin = conf.low, ymax = conf.high, fill = model),
              inherit.aes = FALSE, alpha = 0.2) +
  geom_line(data = pred_combined_fki,
            aes(x = year_num, y = fit, colour = model),
            inherit.aes = FALSE, linewidth = 1) +
  geom_errorbar(data = year_summary_fki,
                aes(x = year_num,
                    ymin = mean_temp - se_temp, ymax = mean_temp + se_temp,
                    colour = model),
                position = position_dodge(width = 0.5),
                width = 0.3,      
                linewidth = 1) +
  geom_point(data = year_summary_fki,
             aes(x = year_num, y = mean_temp, colour = model),
             position = position_dodge(width = 0.5),
             size = 2.5) +
  scale_fill_manual(values   = c("Observed" = "darkgoldenrod1", "Null" = "purple4")) +
  scale_colour_manual(values = c("Observed" = "darkgoldenrod1", "Null" = "purple4")) +
  scale_y_continuous(breaks = seq(0, 12, by = 2)) +
  scale_x_continuous(breaks = seq(2014, 2025, by = 1)) +
  labs(x      = "Year",
       y      = "Mean daily minimum temperature (°C)",
       title  = "B",
       colour = "Model",
       fill   = "Model"
  ) +
  theme_classic(base_size = 25) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fki_obs_null_plot

##### Making the fki null latitude plot #####
# make predictions over lat, holding year_num and elevation at their means
pred_range_null_fki_lat <- data.frame(
  lat = seq(
    min(jointdata_common$lat, na.rm = TRUE),
    max(jointdata_common$lat, na.rm = TRUE),
    length.out = 100
  )
) %>%
  mutate(
    year_num  = mean(jointdata_common$year_num,  na.rm = TRUE),
    elevation = mean(jointdata_common$elevation, na.rm = TRUE)
  )

pred_null_fki_lat <- predict(fki_null_model, newdata = pred_range_null_fki_lat, re.form = NA,
                             type = "response", se.fit = TRUE)

pred_range_null_fki_lat$fit       <- pred_null_fki_lat$fit
pred_range_null_fki_lat$conf.low  <- pred_null_fki_lat$fit - 1.96 * pred_null_fki_lat$se.fit
pred_range_null_fki_lat$conf.high <- pred_null_fki_lat$fit + 1.96 * pred_null_fki_lat$se.fit

# combine observed and null
pred_range_fki_lat$model       <- "Observed"
pred_range_null_fki_lat$model  <- "Null"
pred_combined_fki_lat          <- bind_rows(pred_range_fki_lat, pred_range_null_fki_lat)

# make null and obs dataset
raw_points_fki_lat <- bind_rows(
  jointdata_common %>% select(lat, tempmeanmin_fki) %>%
    rename(temp = tempmeanmin_fki) %>% mutate(model = "Observed"),
  jointdata_common %>% select(lat, tempnull_fki) %>%
    rename(temp = tempnull_fki) %>% mutate(model = "Null")
)

# make plot
fki_obs_null_lat_plot <- ggplot() +
  geom_jitter(data = raw_points_fki_lat,
              aes(x = lat, y = temp, colour = model),
              alpha = 0.1, width = 0.2, size = 3) +
  geom_ribbon(data = pred_combined_fki_lat,
              aes(x = lat, ymin = conf.low, ymax = conf.high, fill = model),
              inherit.aes = FALSE, alpha = 0.2, size = 3) +
  geom_line(data = pred_combined_fki_lat,
            aes(x = lat, y = fit, colour = model),
            inherit.aes = FALSE, linewidth = 2) +
  scale_fill_manual(values   = c("Observed" = "darkgoldenrod1", "Null" = "purple4")) +
  scale_colour_manual(values = c("Observed" = "darkgoldenrod1", "Null" = "purple4")) +
  scale_y_continuous(breaks = seq(0, 12, by = 2)) +
  scale_x_continuous(breaks = seq(50, 60, by = 1)) +
  labs(
    x      = "Latitude (°N)",
    y      = "Mean daily minimum temperature (°C)",
    title  = "B",
    colour = "Model",
    fill   = "Model"
  ) +
  theme_classic(25) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fki_obs_null_lat_plot

##### Making the fki null elevation plot #####
# make predictions over elevation, holding year_num and lat at their means
pred_range_null_fki_elev <- data.frame(
  elevation = seq(
    min(jointdata_common$elevation, na.rm = TRUE),
    max(jointdata_common$elevation, na.rm = TRUE),
    length.out = 100
  )
) %>%
  mutate(
    year_num = mean(jointdata_common$year_num, na.rm = TRUE),
    lat      = mean(jointdata_common$lat, na.rm = TRUE)
  )

pred_null_fki_elev <- predict(fki_null_model, newdata = pred_range_null_fki_elev, re.form = NA,
                              type = "response", se.fit = TRUE)

pred_range_null_fki_elev$fit       <- pred_null_fki_elev$fit
pred_range_null_fki_elev$conf.low  <- pred_null_fki_elev$fit - 1.96 * pred_null_fki_elev$se.fit
pred_range_null_fki_elev$conf.high <- pred_null_fki_elev$fit + 1.96 * pred_null_fki_elev$se.fit

# combine observed and null
pred_range_fki_elev$model       <- "Observed"
pred_range_null_fki_elev$model  <- "Null"
pred_combined_fki_elev          <- bind_rows(pred_range_fki_elev, pred_range_null_fki_elev)

# make null and obs dataset
raw_points_fki_elev <- bind_rows(
  jointdata_common %>% select(elevation, tempmeanmin_fki) %>%
    rename(temp = tempmeanmin_fki) %>% mutate(model = "Observed"),
  jointdata_common %>% select(elevation, tempnull_fki) %>%
    rename(temp = tempnull_fki) %>% mutate(model = "Null")
)

# make plot
fki_obs_null_elev_plot <- ggplot() +
  geom_jitter(data = raw_points_fki_elev,
              aes(x = elevation, y = temp, colour = model),
              alpha = 0.1, width = 0.2, size = 3) +
  geom_ribbon(data = pred_combined_fki_elev,
              aes(x = elevation, ymin = conf.low, ymax = conf.high, fill = model),
              inherit.aes = FALSE, alpha = 0.2, size = 3) +
  geom_line(data = pred_combined_fki_elev,
            aes(x = elevation, y = fit, colour = model),
            inherit.aes = FALSE, linewidth = 2) +
  scale_fill_manual(values   = c("Observed" = "darkgoldenrod1", "Null" = "purple4")) +
  scale_colour_manual(values = c("Observed" = "darkgoldenrod1", "Null" = "purple4")) +
  scale_y_continuous(breaks = seq(0, 12, by = 2)) +
  scale_x_continuous(breaks = seq(0, 500, by = 50)) +
  labs(
    x      = "Elevation (m)",
    y      = "Mean daily minimum temperature (°C)",
    title  = "B",
    colour = "Model",
    fill   = "Model"
  ) +
  theme_classic(25) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fki_obs_null_elev_plot

##### Making the null hatch model #####

# calculate the overall mean hatch start date
overall_mean_hatch <- mean(nestdata$hatching_first_recorded, na.rm = TRUE)

# make loop to get temperature for each nest using the year-specific mean fki date
tempnull_hatch <- rep(NA, nrow(nestdata))

for (x in 1:nrow(nestdata)) {
  site.i <- nestdata$site[x]
  year.i <- nestdata$year[x]
  # no box.i as this doesn't add anything
  
  # using the year-specific mean start date 
  null_start <- overall_mean_hatch
  null_end   <- null_start + 18
  
  # skip if mean fed is missing for that year
  if (length(null_start) == 0 || is.na(null_start)) next
  
  cols.use <- which(temp.times >= null_start & temp.times <= null_end)
  if (length(cols.use) == 0) next
  
  rows.use <- which(loggerdata$site == site.i & loggerdata$year == year.i)
  if (length(rows.use) == 0) next
  
  temp.sub    <- as.matrix(loggerdata[rows.use, temp.cols[cols.use]])
  hourly.temp <- if (nrow(temp.sub) > 1) colMeans(temp.sub, na.rm = TRUE) else as.numeric(temp.sub)
  
  day.id    <- floor(temp.times[cols.use])
  tempnull_hatch[x] <- min(hourly.temp, na.rm = TRUE)
}

# add null temp to nestdata and jointdata
nestdata$tempnull_hatch <- tempnull_hatch
jointdata_common$tempnull_hatch <- nestdata$tempnull_hatch[
  match(
    paste(jointdata_common$site, jointdata_common$year, jointdata_common$box, sep = "_"),
    paste(nestdata$site, nestdata$year, nestdata$box, sep = "_")
  )
]

# hatch null model # FEMALE AND SITE_YEAR REMOVED 
hatch_null_model <- lmer(
  tempnull_hatch ~ year_num + elevation + lat +
    (1 | site) +
    (1 | year),
  data = jointdata_common,
  REML = TRUE
)

summary(hatch_null_model)
VarCorr(hatch_null_model)

##### Making the hatch null plot #####
# make predictions
pred_range_null_hatch <- data.frame(
  year_num = seq(
    min(jointdata_common$year_num, na.rm = TRUE),
    max(jointdata_common$year_num, na.rm = TRUE),
    length.out = 100
  )
) %>%
  mutate(
    elevation = mean(jointdata_common$elevation, na.rm = TRUE),
    lat       = mean(jointdata_common$lat,       na.rm = TRUE)
  )

pred_null_hatch <- predict(hatch_null_model, newdata = pred_range_null_hatch, re.form = NA,
                         type = "response", se.fit = TRUE)

pred_range_null_hatch$fit       <- pred_null_hatch$fit
pred_range_null_hatch$conf.low  <- pred_null_hatch$fit - 1.96 * pred_null_hatch$se.fit
pred_range_null_hatch$conf.high <- pred_null_hatch$fit + 1.96 * pred_null_hatch$se.fit

# compare obs and null models
pred_range_hatch$model       <- "Observed"
pred_range_null_hatch$model  <- "Null"
pred_combined_hatch          <- bind_rows(pred_range_hatch, pred_range_null_hatch)

# make null and obs dataset
raw_points_hatch <- bind_rows(
  jointdata_common %>% select(year_num, tempmin_hatch) %>%
    rename(temp = tempmin_hatch) %>% mutate(model = "Observed"),
  jointdata_common %>% select(year_num, tempnull_hatch) %>%
    rename(temp = tempnull_hatch) %>% mutate(model = "Null")
)

# summarise mean and sd of temp per year, per model
year_summary_hatch <- raw_points_hatch %>%
  group_by(year_num, model) %>%
  summarise(
    mean_temp = mean(temp, na.rm = TRUE),
    sd_temp   = sd(temp, na.rm = TRUE),
    n = sum(!is.na(temp)),
    se_temp = sd_temp / sqrt(n),
    .groups = "drop"
  )
# make plot
hatch_obs_null_plot <- ggplot() +
  geom_ribbon(data = pred_combined_hatch,
              aes(x = year_num, ymin = conf.low, ymax = conf.high, fill = model),
              inherit.aes = FALSE, alpha = 0.2) +
  geom_line(data = pred_combined_hatch,
            aes(x = year_num, y = fit, colour = model),
            inherit.aes = FALSE, linewidth = 1) +
  geom_errorbar(data = year_summary_hatch,
                aes(x = year_num,
                    ymin = mean_temp - se_temp, ymax = mean_temp + se_temp,
                    colour = model),
                position = position_dodge(width = 0.5),
                width = 0.3,      
                linewidth = 1) +
  geom_point(data = year_summary_hatch,
             aes(x = year_num, y = mean_temp, colour = model),
             position = position_dodge(width = 0.5),
             size = 2.5) +
  scale_fill_manual(values   = c("Observed" = "green2", "Null" = "purple4")) +
  scale_colour_manual(values = c("Observed" = "green3", "Null" = "purple4")) +
  scale_y_continuous(breaks = seq(0, 12, by = 2)) +
  scale_x_continuous(breaks = seq(2014, 2025, by = 1)) +
  labs(x      = "Year",
       y      = "Minimum temperature (°C)",
       title  = "C",
       colour = "Model",
       fill   = "Model"
  ) +
  theme_classic(base_size = 25) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

hatch_obs_null_plot

##### Making the hatch null latitude plot #####
# make predictions over lat, holding year_num and elevation at their means
pred_range_null_hatch_lat <- data.frame(
  lat = seq(
    min(jointdata_common$lat, na.rm = TRUE),
    max(jointdata_common$lat, na.rm = TRUE),
    length.out = 100
  )
) %>%
  mutate(
    year_num  = mean(jointdata_common$year_num,  na.rm = TRUE),
    elevation = mean(jointdata_common$elevation, na.rm = TRUE)
  )

pred_null_hatch_lat <- predict(hatch_null_model, newdata = pred_range_null_hatch_lat, re.form = NA,
                             type = "response", se.fit = TRUE)

pred_range_null_hatch_lat$fit       <- pred_null_hatch_lat$fit
pred_range_null_hatch_lat$conf.low  <- pred_null_hatch_lat$fit - 1.96 * pred_null_hatch_lat$se.fit
pred_range_null_hatch_lat$conf.high <- pred_null_hatch_lat$fit + 1.96 * pred_null_hatch_lat$se.fit

# combine observed and null
pred_range_hatch_lat$model       <- "Observed"
pred_range_null_hatch_lat$model  <- "Null"
pred_combined_hatch_lat          <- bind_rows(pred_range_hatch_lat, pred_range_null_hatch_lat)


# make null and obs dataset
raw_points_hatch_lat <- bind_rows(
  jointdata_common %>% select(lat, tempmin_hatch) %>%
    rename(temp = tempmin_hatch) %>% mutate(model = "Observed"),
  jointdata_common %>% select(lat, tempnull_hatch) %>%
    rename(temp = tempnull_hatch) %>% mutate(model = "Null")
)

# make plot
hatch_obs_null_lat_plot <- ggplot() +
  geom_jitter(data = raw_points_hatch_lat,
              aes(x = lat, y = temp, colour = model),
              alpha = 0.1, width = 0.2, size = 3) +
  geom_ribbon(data = pred_combined_hatch_lat,
              aes(x = lat, ymin = conf.low, ymax = conf.high, fill = model),
              inherit.aes = FALSE, alpha = 0.2, size = 3) +
  geom_line(data = pred_combined_hatch_lat,
            aes(x = lat, y = fit, colour = model),
            inherit.aes = FALSE, linewidth = 2) +
  scale_fill_manual(values   = c("Observed" = "green2", "Null" = "purple4")) +
  scale_colour_manual(values = c("Observed" = "green3", "Null" = "purple4")) +
  scale_y_continuous(breaks = seq(0, 12, by = 2)) +
  scale_x_continuous(breaks = seq(50, 60, by = 1)) +
  labs(
    x      = "Latitude (°N)",
    y      = "Minimum temperature (°C)",
    title  = "C",
    colour = "Model",
    fill   = "Model"
  ) +
  theme_classic(25) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

hatch_obs_null_lat_plot

##### Making the hatch null elevation plot #####
# make predictions over elevation, holding year_num and lat at their means
pred_range_null_hatch_elev <- data.frame(
  elevation = seq(
    min(jointdata_common$elevation, na.rm = TRUE),
    max(jointdata_common$elevation, na.rm = TRUE),
    length.out = 100
  )
) %>%
  mutate(
    year_num = mean(jointdata_common$year_num, na.rm = TRUE),
    lat      = mean(jointdata_common$lat, na.rm = TRUE)
  )

pred_null_hatch_elev <- predict(hatch_null_model, newdata = pred_range_null_hatch_elev, re.form = NA,
                              type = "response", se.fit = TRUE)

pred_range_null_hatch_elev$fit       <- pred_null_hatch_elev$fit
pred_range_null_hatch_elev$conf.low  <- pred_null_hatch_elev$fit - 1.96 * pred_null_hatch_elev$se.fit
pred_range_null_hatch_elev$conf.high <- pred_null_hatch_elev$fit + 1.96 * pred_null_hatch_elev$se.fit

# combine observed and null
pred_range_hatch_elev$model       <- "Observed"
pred_range_null_hatch_elev$model  <- "Null"
pred_combined_hatch_elev          <- bind_rows(pred_range_hatch_elev, pred_range_null_hatch_elev)

# make null and obs dataset
raw_points_hatch_elev <- bind_rows(
  jointdata_common %>% select(elevation, tempmin_hatch) %>%
    rename(temp = tempmin_hatch) %>% mutate(model = "Observed"),
  jointdata_common %>% select(elevation, tempnull_hatch) %>%
    rename(temp = tempnull_hatch) %>% mutate(model = "Null")
)

# make plot
hatch_obs_null_elev_plot <- ggplot() +
  geom_jitter(data = raw_points_hatch_elev,
              aes(x = elevation, y = temp, colour = model),
              alpha = 0.1, width = 0.2, size = 3) +
  geom_ribbon(data = pred_combined_hatch_elev,
              aes(x = elevation, ymin = conf.low, ymax = conf.high, fill = model),
              inherit.aes = FALSE, alpha = 0.2, size = 3) +
  geom_line(data = pred_combined_hatch_elev,
            aes(x = elevation, y = fit, colour = model),
            inherit.aes = FALSE, linewidth = 2) +
  scale_fill_manual(values   = c("Observed" = "green2", "Null" = "purple4")) +
  scale_colour_manual(values = c("Observed" = "green3", "Null" = "purple4")) +
  scale_y_continuous(breaks = seq(0, 12, by = 2)) +
  scale_x_continuous(breaks = seq(0, 500, by = 50)) +
  labs(
    x      = "Elevation (m)",
    y      = "Minimum temperature (°C)",
    title  = "C",
    colour = "Model",
    fill   = "Model"
  ) +
  theme_classic(25) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

hatch_obs_null_elev_plot


fed_obs_null_plot + fki_obs_null_plot + hatch_obs_null_plot

fed_obs_null_lat_plot + fki_obs_null_lat_plot + hatch_obs_null_lat_plot

fed_obs_null_elev_plot + fki_obs_null_elev_plot + hatch_obs_null_elev_plot
