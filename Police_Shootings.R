library(readr)
library(magrittr)
library(leaflet)
install.packages("mapproj")
data <- read_csv("C:/Users/olive/Downloads/MPVDatasetDownload (1).csv")
states <- rgdal::readOGR("C:/Users/olive/Downloads/gz_2010_us_040_00_5m.json")
codes <- read_csv("C:/Users/olive/Downloads/MPVDatasetDownload (2).csv")
stats <- read_csv("C:/Users/olive/Downloads/MPVDatasetDownload (3).csv")
stats <- dplyr::filter(stats, is.na(State) == F)
pop <- data.frame(stats$`State Abbreviation`, stats$`Total Population`)

by_state <- table(data$State)%>%
  as.data.frame()
by_state <- merge(by_state, pop, by.x = "Var1", by.y = "stats..State.Abbreviation.")
by_state$Freq <- (by_state$Freq/by_state$stats..Total.Population.)*100000
by_state <- merge(by_state, codes, by.x = "Var1", by.y = "Code")
states@data <- merge(by_state, states@data, by.x = "State", by.y = "NAME")


states <- states[!states$STATE %in% c("02", "15"),]
spdf_fortified <- broom::tidy(states)
by_state <- by_state[order(by_state$State), ]
by_state$id <- as.character(0:50)
by_state <- by_state[!by_state$Var1 %in% c("AK", "HI"),]
spdf_fortified = spdf_fortified %>%
  dplyr::left_join(. , by_state, by=c("id"="id"))
spdf_fortified$Freq[is.na(spdf_fortified$Freq)] = 0.001

library(ggplot2)
p <- ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = Freq, x = long, y = lat, group = group) , size=0, alpha=0.95) +
  theme_void() +
  scico::scale_fill_scico(palette = "lajolla", breaks=c(1, 3, 5, 7), name="Police Shootings per 100k", guide = guide_colourbar(direction = "horizontal", label.position = "bottom", title.position = 'top', nrow=1)) +
  labs(
    title = "Police Shootings by State",
    subtitle = "Number of Police Shootings in the Lower 48 per 100k Residents between Jan 2013 & Jun 2021",
    caption = "Data: Mapping Police Violence & U.S. Census Bureau | u/Serious-Jellyfish-59  "
  ) +
  theme(
    text = element_text(color = "#22211d", family="Times New Roman"),
    plot.background = element_rect(fill = "#d1cec5", color = NA),
    panel.background = element_rect(fill = "#d1cec5", color = NA),
    legend.background = element_rect(fill = "#d1cec5", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.03, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 12, hjust=0.10, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.15, 0.12)
  ) +
  coord_map()
p
