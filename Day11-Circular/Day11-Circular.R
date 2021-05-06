library(ggplot2)
library(geosphere) # to calculate daylength
library(dplyr)
library(extrafont)
library(viridis)
library(showtext)

#fonts
#fonts
font_add(family = "bold", "AlegreyaSans-Bold.ttf")
font_add(family = "regular", "AlegreyaSans-Regular.ttf")
showtext_auto()

# daylength function takes latitude and date, will use the 2 cities that I want to visit next
data = data.frame(city=c(rep('Reykjavik, Iceland', 365), rep('Seoul, South Korea', 365)),
                  doy = c(rep(seq(as.Date("2020-01-01"), as.Date("2020-12-31"), length.out = 365), 2)),
                  lat = c(rep(64.08, 365), rep(37.57, 365)))

data$month = sapply(data$doy, function(x) as.integer(format(x, '%m')))
data$month_name = factor(month.abb[data$month],levels=month.abb)
data$hours_daylight = mapply(daylength, data$lat, data$doy)

data %>% group_by(month_name, city) %>% 
  summarise(avg_monthly_daylength = mean(hours_daylight)) %>%
  ggplot() +
  geom_bar(aes(x=month_name, y=avg_monthly_daylength, fill=avg_monthly_daylength), stat='identity') +
  scale_fill_viridis_c(option='viridis') +
  facet_wrap(vars(city)) + coord_polar() +
  labs(title='Where to next?',
        subtitle='Average Monthly Daylight Hours in the Cities at the Top of My Travel List',
       caption = 'moriah_taylor58 | #30daychartchallenge | inspired by: dosullivan019',
       fill = "Daylight Hours") +
  theme(#legend
        legend.position='top',
        legend.title = element_text(family='regular',size=30, vjust=0.85, color='white'),
        legend.background = element_rect(fill='black'),
        legend.text = element_text(family='regular', color='white', size=30),
        #background
        panel.background = element_rect(fill='black', color='white'),
        plot.background = element_rect(fill='black'),
        strip.background = element_rect(fill='black', color='white'),
        #leave blank
        axis.title = element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        #remove gridlines
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #text
        plot.title=element_text(family='bold', size=50, hjust=0.5, color='white'),
        plot.caption = element_text(family='regular', size=30, color='white'),
        plot.subtitle = element_text(family='bold', size=38, color='white', hjust=0.5),
        axis.text.x = element_text(family='bold',size=20, color='white'),
        strip.text.x = element_text(size = 30, family='bold', color='white')) # modify facet text
        


ggsave("Day11-Circular.png")
