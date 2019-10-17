library(viridis) # nice color palette
library(ggplot2) # plotting
library(dplyr) # use for fixing up data
library(purrr) # for mapping over a function
library(magick) # this is call to animate/read pngs
library(here)
library(xml2)

load(here("animated_gif","planetyear_data.RData"))

y_range <- x_range <- c(-600,650)
years <- c(seq(from=2100, to=3020, by=5),
           3025:3081,
           seq(from=3085, to=3130, by=5),
           3132:3145)

theme_mine <- function() {
  theme_minimal()+
    theme(plot.background = element_rect(fill="black"),
          axis.line = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.background = element_rect(color="white", fill="black", linetype="solid"),
          legend.text = element_text(color="white"),
          legend.title = element_text(color="white"),
          legend.position = "none",
          strip.text = element_text(color="white", size = 12)
          )
}

#test out with tech figs
ggplot(subset(planet_data, year==3100 & !is.na(tech)), aes(x=x, y=y, color=tech))+
  geom_point(alpha=0.8)+
  lims(x=x_range, y=y_range)+
  facet_wrap(~year, ncol = 1)+
  scale_color_viridis_d()+
  labs(x=NULL, y=NULL, color="Technology")+
  theme_mine()


#make the function that saves to file
plot_tech <- function(yr) {
  ggplot(subset(planet_data, year==yr & !is.na(tech)), aes(x=x, y=y, color=tech))+
    geom_point(alpha=0.8)+
    lims(x=c(-600,650), y=c(-600,650))+
    facet_wrap(~year, ncol = 1)+
    scale_color_viridis_d()+
    labs(x=NULL, y=NULL, color="Technology")+
    theme_mine()
  print(paste0("saving plot ", yr))
  ggsave(filename = paste0(here("animated_gif","tech_figs","tech"), yr,".png"),
         width = 4, height=4, dpi = 150)
}
 
#create images from 2100 to 3145
years %>% map_df(plot_tech)

#animate with image magic
list.files(path = here("animated_gif","tech_figs"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=2) %>% # animates, can opt for number of loops
  image_write(here("animated_gif","tech_animated.gif")) # write to current dir

#ok now try factions

#load in factions.xml to pull out custom colors
convertRGB <- function(col_rgb) {
  temp <- as.numeric(strsplit(col_rgb, ",")[[1]])
  return(rgb(temp[1]/255, temp[2]/255, temp[3]/255, alpha=1))
}

factions <- xml_find_all(read_xml(here("animated_gif","factions.xml")), "faction")
shortname <- NULL
faction_colors <- NULL
for(faction in factions) {
  shortname <- c(shortname, xml_text(xml_find_first(faction, "shortname")))
  faction_colors <- c(faction_colors, convertRGB(xml_text(xml_find_first(faction, "colorRGB"))))
}
names(faction_colors) <- shortname

ggplot(subset(planet_data, year==3100 & !is.na(faction)), aes(x=x, y=y, color=faction))+
  geom_point(alpha=0.8)+
  lims(x=x_range, y=y_range)+
  facet_wrap(~year, ncol = 1)+
  scale_color_manual(values = faction_colors)+
  labs(x=NULL, y=NULL, color="Faction")+
  theme_mine()

#make the function that saves to file
plot_faction <- function(yr) {
  ggplot(subset(planet_data, year==yr & !is.na(faction)), aes(x=x, y=y, color=faction))+
    geom_point(alpha=0.8)+
    lims(x=x_range, y=y_range)+
    facet_wrap(~year, ncol = 1)+
    scale_color_manual(values = faction_colors)+
    labs(x=NULL, y=NULL, color="Faction")+
    theme_mine()
  print(paste0("saving plot ", yr))
  ggsave(filename = paste0(here("animated_gif","faction_figs","faction"), yr,".png"),
         width = 4, height=4, dpi = 150)
}

years %>% map_df(plot_faction)

#animate with image magic
list.files(path = here("animated_gif","faction_figs"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=2) %>% # animates, can opt for number of loops
  image_write(here("animated_gif","faction_animated.gif")) # write to current dir


#population

#test out with tech figs
ggplot(subset(planet_data, year==3100 & !is.na(population)), aes(x=x, y=y, color=population))+
  geom_point(alpha=0.8)+
  lims(x=x_range, y=y_range)+
  facet_wrap(~year, ncol = 1)+
  scale_color_viridis_d()+
  labs(x=NULL, y=NULL, color="Technology")+
  theme_mine()


#make the function that saves to file
plot_tech <- function(yr) {
  ggplot(subset(planet_data, year==yr & !is.na(tech)), aes(x=x, y=y, color=tech))+
    geom_point(alpha=0.8)+
    lims(x=c(-600,650), y=c(-600,650))+
    facet_wrap(~year, ncol = 1)+
    scale_color_viridis_d()+
    labs(x=NULL, y=NULL, color="Technology")+
    theme_mine()
  print(paste0("saving plot ", yr))
  ggsave(filename = paste0(here("animated_gif","tech_figs","tech"), yr,".png"),
         width = 4, height=4, dpi = 150)
}

#create images from 2100 to 3145
years %>% map_df(plot_tech)

#animate with image magic
list.files(path = here("animated_gif","tech_figs"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=2) %>% # animates, can opt for number of loops
  image_write(here("animated_gif","tech_animated.gif")) # write to current dir
