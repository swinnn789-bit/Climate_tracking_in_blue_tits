niche <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Thesis/tit thermal niche.csv")
summary(niche)

install.packages("ggplot2")
library(ggplot2)

ggplot(niche, aes(x = Date)) +
  geom_line(aes(y = Temperature_1980, colour = "1980"), linewidth = 1.2) +
  geom_line(aes(y = Temperature_2020, colour = "2020"), linewidth = 1.2) +
  
  coord_cartesian(xlim = c(80, 140), ylim = c(10, 17)) +
  
  scale_colour_manual(
    values = c("1980" = "blue", "2020" = "red")
  ) +
  
  labs(
    title = "Thermal niche tracking",
    x = "Date",
    y = "Temperature (°C)",
    colour = "Year"
  ) +
  
  theme_minimal()


data <- data.frame(
  LayDate = seq(90, 110,length.out = 20),
  Temperature = seq(18, 10,length.out = 20) 
)

# Plot
ggplot(data, aes(x = Temperature, y = LayDate)) +
  geom_line(colour = "green", linewidth = 1.2) +
 
  labs(
    title = "Temperature vs Lay Date",
    x = "Temperature (°C)",
    y = "Lay Date"
  ) +
  
  theme_minimal()

################

library(ggplot2)

year <- 2000:2010

df <- data.frame(
  Year = year,
  Ambient = 0.2 * (year - 2000) + 14,
  Slight = 0.1 * (year - 2000) + 14.5,
  Bosh = 0.2 * (year - 2000) + 13.9,
  Constant = rep(15, length(year))
)

t1<- ggplot(df, aes(x = Year)) +
  geom_line(aes(y = Ambient, colour = "Ambient Temperature"), linewidth = 2) +
  geom_hline(aes(yintercept = 15, colour = "Observed thermal niche"), linewidth = 2) +
  labs(y = "Temperature", colour = "Legend",
       title = "Full tracking") +
  scale_x_continuous(breaks = seq(2000, 2010, 2)) +
  theme_classic(base_size=16)+
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank())
  
t1

t2 <- ggplot(df, aes(x = Year)) +
  geom_line(aes(y = Ambient, colour = "Ambient Temperature"), linewidth = 2) +
  geom_line(aes(y = Slight, colour = "Observed thermal niche"), linewidth = 2) +
  labs(y = "Temperature", colour = "Legend",
       title = "Partial tracking") +
  scale_x_continuous(breaks = seq(2000, 2010, 2)) +
  theme_classic(base_size=16)+
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank())

t2

t3 <- ggplot(df, aes(x = Year)) +
  geom_line(aes(y = Ambient, colour = "Ambient Temperature"), linewidth = 2) +
  geom_line(aes(y = Bosh, colour = "Observed thermal niche"), linewidth = 2) +
  labs(y = "Temperature", colour = "Legend",
       title = "No tracking") +
  scale_x_continuous(breaks = seq(2000, 2010, 2)) +
  theme_classic(base_size=16)+
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank())

t3

library("patchwork")
t1/t2/t3

year <- 2000:2010

df2 <- data.frame(
  Year = year,
  Ambient = -0.2 * (year - 2000) + 16,
  Slight = -0.1 * (year - 2000) + 15.5,
  Bosh = -0.2 * (year - 2000) + 15.9,
  Constant = rep(15, length(year))
)

ggplot(df2, aes(x = Year)) +
  geom_line(aes(y = Ambient, colour = "Optimal phenology"), linewidth = 1) +
  geom_hline(aes(yintercept = 15, colour = "Observed thermal niche"), linewidth = 1) +
  labs(y = "Phenology", colour = "Legend",
       title = "No tracking") +
  scale_x_continuous(breaks = seq(2000, 2010, 2)) +
  theme_minimal()+
  theme(legend.position = "bottom")+
  scale_colour_manual(values = c(
    "Optimal phenology" = "purple3",
    "Observed thermal niche" = "cyan3"
  ))

  ggplot(df2, aes(x = Year)) +
    geom_line(aes(y = Ambient, colour = "Optimal phenology"), linewidth = 1) +
    geom_line(aes(y = Slight, colour = "Observed thermal niche"), linewidth = 1) +
    scale_colour_manual(values = c(
      "Optimal phenology" = "purple3",
      "Observed thermal niche" = "cyan3"
    )) +
    labs(y = "Phenology", colour = "Legend",
         title = "Partial tracking") +
    scale_x_continuous(breaks = seq(2000, 2010, 2)) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggplot(df2, aes(x = Year)) +
    geom_line(aes(y = Ambient, colour = "Optimal phenology"), linewidth = 1) +
    geom_line(aes(y = Bosh, colour = "Observed thermal niche"), linewidth = 1) +
    scale_colour_manual(values = c(
      "Optimal phenology" = "purple3",
      "Observed thermal niche" = "cyan3"
    )) +
    labs(y = "Phenology", colour = "Legend",
         title = "Full tracking") +
    scale_x_continuous(breaks = seq(2000, 2010, 2)) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
###
  df <- data.frame(
    Year = year,
    Ambient = -0.2 * (year - 2000) + 16,
    Slight = -0.1 * (year - 2000) + 15.5,
    Bosh = -0.2 * (year - 2000) + 15.9,
    Constant = rep(15, length(year))
  )
  
l1 <- ggplot(df, aes(x = Year)) +
    geom_line(aes(y = Ambient, colour = "Ambient Temperature"), linewidth = 2) +
    geom_hline(aes(yintercept = 15, colour = "Observed thermal niche"), linewidth = 2) +
    labs(y = "Temperature", x = "Latitude/Elevation", colour = "Legend",
         title = "") +
    scale_x_continuous(breaks = seq(2000, 2010, 2)) +
  theme_classic(base_size=16)+
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank())
  
l1  

l2 <- ggplot(df, aes(x = Year)) +
    geom_line(aes(y = Ambient, colour = "Ambient Temperature"), linewidth = 2) +
    geom_line(aes(y = Slight, colour = "Observed thermal niche"), linewidth = 2) +
    labs(y = "Temperature", x = "Latitude/Elevation", colour = "Legend",
         title = "") +
    scale_x_continuous(breaks = seq(2000, 2010, 2)) +
  theme_classic(base_size=16)+
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank())
  
l2  

l3<- ggplot(df, aes(x = Year)) +
    geom_line(aes(y = Ambient, colour = "Ambient Temperature"), linewidth = 2) +
    geom_line(aes(y = Bosh, colour = "Observed thermal niche"), linewidth = 2) +
    labs(y = "Temperature", x = "Latitude/Elevation", colour = "Legend",
         title = "") +
    scale_x_continuous(breaks = seq(2000, 2010, 2)) +
  theme_classic(base_size=16)+
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank())
  
l3  

t1 + l1 + t2 + l2 + t3 + l3 + 
  plot_layout(nrow = 3, ncol = 2, guides = "collect") &
  theme(legend.position = "bottom",
        legend.justification = "center") &
  guides(colour = guide_legend(title.position = "top", title.hjust = 0))


year <- 10:20
  
  df2 <- data.frame(
    Year = year,
    Ambient = 0.2 * (year - 10) + 14,
    Slight = 0.1 * (year - 10) + 14.5,
    Bosh = 0.2 * (year - 10) + 13.9,
    Constant = rep(15, length(year))
  )
  
  ggplot(df, aes(x = Year)) +
    geom_line(aes(y = Ambient, colour = "Optimal phenology"), linewidth = 1) +
    geom_hline(yintercept = 15, colour = "cyan3", linewidth = 1) +  # horizontal line
    labs(y = "Phenology", x = "Latitude/Elevation", colour = "Legend",
         title = "No tracking") +
    scale_x_continuous(breaks = seq(2000, 2010, 2)) +
    scale_colour_manual(values = c(
      "Optimal phenology" = "purple3"
    )) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_blank(),    # remove x-axis numbers
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),    # remove x-axis numbers
      axis.ticks.y = element_blank()
    )
  
  ggplot(df2, aes(x = Year)) +
    
    geom_line(aes(y = Ambient, colour = "Optimal phenology"), linewidth = 1) +
    geom_hline(aes(yintercept = 15, colour = "Observed thermal niche"), linewidth = 1) +
    labs(y = "Phenology", x = "Latitude/Elevation", colour = "Legend",
         title = "No tracking") +
    scale_x_continuous(breaks = seq(2000, 2010, 2)) +
    theme_minimal()+
    theme(
      legend.position = "bottom",
      axis.text.x = element_blank(),    # remove x-axis numbers
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),    # remove x-axis numbers
      axis.ticks.y = element_blank())
    theme(legend.position = "bottom")+
    scale_colour_manual(values = c(
      "Optimal phenology" = "purple3",
      "Observed thermal niche" = "cyan3"
    )) 
    
    ggplot(df2, aes(x = Year)) +
      
      geom_line(aes(y = Ambient, colour = "Optimal phenology"), linewidth = 1) +
      geom_hline(aes(yintercept = 15, colour = "Observed thermal niche"), linewidth = 1) +
      labs(y = "Phenology", x = "Latitude/Elevation", colour = "Legend",
           title = "No tracking") +
      scale_x_continuous(breaks = seq(2000, 2010, 2)) +
      theme_minimal()+
      theme(legend.position = "bottom")+
      scale_colour_manual(values = c(
        "Optimal phenology" = "purple3",
        "Observed thermal niche" = "cyan3"
      ))
    
  
  ggplot(df2, aes(x = Year)) +
    geom_line(aes(y = Ambient, colour = "Optimal phenology"), linewidth = 1) +
    geom_line(aes(y = Slight, colour = "Observed thermal niche"), linewidth = 1) +
    scale_colour_manual(values = c(
      "Optimal phenology" = "purple3",
      "Observed thermal niche" = "cyan3"
    )) +
    labs(y = "Phenology", x = "Latitude/Elevation", colour = "Legend",
         title = "Partial tracking") +
    scale_x_continuous(breaks = seq(2000, 2010, 2)) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggplot(df2, aes(x = Year)) +
    geom_line(aes(y = Ambient, colour = "Optimal phenology"), linewidth = 1) +
    geom_line(aes(y = Bosh, colour = "Observed thermal niche"), linewidth = 1) +
    scale_colour_manual(values = c(
      "Optimal phenology" = "purple3",
      "Observed thermal niche" = "cyan3"
    )) +
    labs(y = "Phenology", x = "Latitude/Elevation", colour = "Legend",
         title = "Full tracking") +
    scale_x_continuous(breaks = seq(2000, 2010, 2)) +
    theme_minimal() +
    theme(legend.position = "bottom")
  