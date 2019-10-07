#this script will create a dataset that can be used to look at the planet data
#at any point in time with a variety of social characteristics. The dataset will
#consist of planets by years in the long mode so each row will be a planet-year.
#I will then track

# - current faction (no entry if not yet founded or abandoned)
# - population
# - sic codes (on variable for each)
# - HPG status

library(xml2)
library(magrittr)
library(rlist)
library(here)
library(tibble)
source(here("functions","data_functions.R"))

years <- as.list(2100:3145)
systems <- read_xml(here("output","systems.xml"))
events <- read_xml(here("output","system_events.xml"))

planet_data <- tibble(id=character(),
                      year=numeric(),
                      x=numeric(),
                      y=numeric(),
                      faction=character(),
                      population=numeric(),
                      tech=character(),
                      industry=character(),
                      raw=character(),
                      output=character(),
                      agriculture=character(),
                      hpg=character())

#return a data frame of type of events for a given planet from the systems data
get_planet_event_data <- function(planet_events, event_type) {
  all_children <- xml_children(planet_events)
  chosen_children <- all_children[xml_name(all_children)==event_type]
  chosen_events <- xml_parent(chosen_children)
  dates <- xml_text(xml_find_first(chosen_events, "date"))
  v_events <- xml_text(xml_find_first(chosen_events, event_type))
  #remove any missing values that may occur due to bad date formatting
  events_df <- na.omit(data.frame(year=as.numeric(substr(dates,1,4)),
                                  event=v_events))
  events_df$event <- as.character(events_df$event)
  events_df <- events_df[order(events_df$year),]
  return(events_df)
}

#get the closest event to the given year that is not later than the given date
get_closest_event <- function(chosen_year, event_table) {
  year_diff <- event_table$year-chosen_year
  if(sum(year_diff<=0)==0) {
    ##all the dates are later than the target date
    return(NA)
  }
  return(event_table$event[which(year_diff==max(year_diff[year_diff<=0]))[1]])
}



for(i in 1:xml_length(systems)) {
  system <- xml_children(systems)[[i]]
  id <- xml_text(xml_find_first(system, "id"))
  cat(id)
  x <- as.numeric(xml_text(xml_find_first(system, "xcood")))
  y <- as.numeric(xml_text(xml_find_first(system, "ycood")))
  primary_slot <- as.numeric(xml_text(xml_find_first(system, "primarySlot")))
  pevents <- xml_find_all(get_planet_in_system(events, id, primary_slot), "event")
  factions <- get_planet_event_data(pevents, "faction")
  populations <- get_planet_event_data(pevents, "population")
  populations$event <- as.numeric(populations$event)
  sics <- get_planet_event_data(pevents, "socioIndustrial")
  hpgs <- get_planet_event_data(pevents, "hpg")

  faction <- sapply(years, get_closest_event, factions)
  population <- sapply(years, get_closest_event, populations)
  sic <- sapply(years, get_closest_event, sics)
  tech <- substr(sic,1,1)
  industry <- substr(sic,3,3)
  raw <- substr(sic,5,5)
  output <- substr(sic,7,7)
  agriculture <- substr(sic,9,9)
  hpg <- sapply(years, get_closest_event, hpgs)

  temp <- tibble(id=id,
                 year=as.numeric(2100:3145),
                 x=x,
                 y=y,
                 faction=faction,
                 population=population,
                 tech=tech,
                 industry=industry,
                 raw=raw,
                 output=output,
                 agriculture=agriculture,
                 hpg=hpg)
  temp <- subset(temp, !is.na(faction) & faction!="ABN")
  planet_data <- rbind(planet_data, temp)
  cat("\n")
}

#for cases with multiple factions just take the first
planet_data$faction <- sapply(strsplit(planet_data$faction, ","), function(x) x[1])

#turn stuff into factors
planet_data$faction <- as.factor(planet_data$faction)

planet_data$tech <- factor(planet_data$tech,
                           levels=c("F","D","C","B","A"),
                           ordered = TRUE)
planet_data$industry <- factor(planet_data$industry,
                               levels=c("F","D","C","B","A"),
                               ordered = TRUE)
planet_data$raw <- factor(planet_data$raw,
                          levels=c("F","D","C","B","A"),
                          ordered = TRUE)
planet_data$output <- factor(planet_data$output,
                             levels=c("F","D","C","B","A"),
                             ordered = TRUE)
planet_data$agriculture <- factor(planet_data$agriculture,
                                  levels=c("F","D","C","B","A"),
                                  ordered = TRUE)
planet_data$hpg <- factor(planet_data$hpg,
                               levels=c("D","C","B","A"),
                               ordered = TRUE)


save(planet_data, file=here("analysis","planetyear_data.RData"))
