source("system_creation_functions.R")

#Run generate_system() 5000 times and pull out all habitable planets


#create the first one to add on to
system <- generate_system()
#easiest way to identify habitable is life
habitable_planets <- subset(system$planets, !is.na(life) & life!="None")
n_habitable <- nrow(habitable_planets)

for(i in 1:4999) {
  system <- generate_system()
  #easiest way to identify habitable is life
  temp <- subset(system$planets, !is.na(life) & life!="None")
  n_habitable <- c(n_habitable, nrow(temp))
  habitable_planets <- rbind(habitable_planets, temp)
  if(nrow(temp)==0) {
    print(system$star$type)
    print(system$iterations)
    warning("Error: produced a system with no inhabitable planets")
    break
  }
}


barplot(100*prop.table(table(n_habitable)), las=1)


hist(habitable_planets$gravity)
hist(habitable_planets$water)
barplot(table(habitable_planets$life))
hist(habitable_planets$day_length)
hist(habitable_planets$year_length)
barplot(table(habitable_planets$pressure))
barplot(table(habitable_planets$atmosphere))
barplot(table(habitable_planets$type))
hist(habitable_planets$temperature)
hist(habitable_planets$continents)

library(ggplot2)

ggplot(habitable_planets, aes(x=gravity))+
  geom_histogram(binwidth=0.1, color="grey")+
  theme_bw()

ggplot(habitable_planets, aes(x=water))+
  geom_histogram(binwidth=5, color="grey")+
  theme_bw()

ggplot(habitable_planets, aes(x=temperature))+
  geom_histogram(binwidth=1, color="grey")+
  theme_bw()

ggplot(habitable_planets, aes(x=continents))+
  geom_histogram(binwidth=1, color="grey")+
  theme_bw()
  
ggplot(habitable_planets, aes(x=water, y=continents))+
  geom_jitter(alpha=0.2)+
  geom_smooth()
  theme_bw()

ggplot(habitable_planets, aes(x=life))+
  geom_bar()+
  theme_bw()

ggplot(habitable_planets, aes(x=atmosphere))+
  geom_bar()+
  theme_bw()

