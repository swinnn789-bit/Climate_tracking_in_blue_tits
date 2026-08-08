library(ggplot2)
library(patchwork)

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
       title = "(A) Full tracking") +
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
       title = "(B) Partial tracking") +
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
       title = "(C) No tracking") +
  scale_x_continuous(breaks = seq(2000, 2010, 2)) +
  theme_classic(base_size=16)+
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank())

t3

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
       title = "(D)") +
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
       title = "(E)") +
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
       title = "(F)") +
  scale_x_continuous(breaks = seq(2000, 2010, 2)) +
  theme_classic(base_size=16)+
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank())

l3  

t1 + t2 + t3 + l1 + l2 + l3 + 
  plot_layout(nrow = 2, ncol = 3, guides = "collect") &
  theme(legend.position = "bottom",
        legend.justification = "center") &
  guides(colour = guide_legend(title.position = "top", title.hjust = 0))

