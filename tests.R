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
barplot(table(habitable_planets$temperature))
hist(habitable_planets$continents)

# Some problems here.

# why are we still getting a small number of cases with no habitable planets

# water coverage seems lowish to me even after diameter adjustments

# why are we getting so few microbes. It seems like the habitability mods
# should have pushed more lower here