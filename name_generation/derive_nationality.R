#derive likely nationality of planet by geoparsing the data using Open Cage

library(xml2)
library(magrittr)
library(opencage)
source("../data_functions.R")

planets <- read_xml("../output/planets_initial.xml")
events <- read_xml("../output/planetevents_initial.xml")
name_changes <- read_xml("../input/0999_namechanges.xml")
stars <-read.csv("../input/real_star_data.csv")
star_names <- subset(as.character(stars$ProperName), stars$ProperName!="")

#figure out what names to search on
name_list <- NULL
for(i in 1:xml_length(planets)) {

  planet <- xml_children(planets)[[i]]
  id <- xml_text(xml_find_first(planet, "id"))
  name <- xml_text(xml_find_first(planet, "name"))

  cat(id)  
  faction_table <- get_event_data(events, id, "faction")
  founding_date <- NA
  if(!is.null(faction_table)) {
    founding_date <- faction_table$date[1]
  }
  alt_names <- get_event_data(name_changes, id, "name")
  if(!is.na(founding_date) & !is.null(alt_names)) {
    alt_name <- get_closest_event(alt_names, founding_date)
    if(!is.na(alt_name)) {
      name <- alt_name
    }
  }
  cat("\t")
  cat(name)
  name_list <- rbind(name_list, c(id, name))
  cat("\n")
}

name_data <- as.data.frame(name_list, stringsAsFactors = FALSE)
colnames(name_data) <- c("id","founding_name")

clean_name <- function(name) {
  #remove everything in parenthesis
  name <- gsub("\\(.*\\)", "", name)
  #remove numbers
  name <- gsub("\\d", "", name)
  #remove any pound signs
  name <- gsub("\\#", "", name)
  #remove roman numeral
  name <- gsub("\\s+(I|II|III|IV|V|VI|VII|VIII|IX|X|XI|XII|XIII|XIV|XV)$", "", name)
  #remove New at the front of a word
  name <- gsub("^New", "", name)
  return(trimws(name))
}

name_data$search_name <- clean_name(name_data$founding_name)
write.csv(name_data, file="search_name.csv", fileEncoding = "UTF-8")

#check for star names
name_data$country_match <- NA

name_data$country_match[name_data$search_name %in% star_names] <- "STAR"

#since we can only make 2500 calls a day, lets
#first try a sample of 500 planets to see how well it works
sampled_idx <- sample(1:nrow(name_data), 500, replace=FALSE)
full_matches <- NULL
#TODO: make sure we don't sample too many calls for the day (2500 limit)
for(i in 1:nrow(name_data)) {
  id <- name_data$id[i]
  search_name <- name_data$search_name[i]
  cat(search_name)
  cat("....")
  #delay for one second to make sure we don't make too many calls too fast
  Sys.sleep(1)
  results <- opencage_forward(search_name)
  if(!is.null(results$results)) {
    cat(paste(" found", nrow(results$results), "possible matches"))
    #TODO: include language info
    full_matches <- rbind(full_matches, cbind(id, search_name, 
                                              results$results[,c("components.country",
                                                                 "components._type",
                                                                 "confidence")]))
  } else {
    cat(" NO MATCHES")
    full_matches <- rbind(full_matches, c(id, search_name, NA, NA, NA))
  }
  full_matches$id <- as.character(full_matches$id)
  full_matches$search_name <- as.character(full_matches$search_name)
  cat("\n")
  #check to see if we hit out limit and if so, sleep until we get to the reset
  if(results$rate_info$remaining<=0) {
    cat("Hit open cage limit. Sleeping until reset...")
    Sys.sleep(as.numeric(difftime(results$rate_info$reset,Sys.time(),units="secs")))
    cat("Waking up!\n")
  }
}

#cycle through all the unique ids and decide on a best match
priority_type=c("county","state","city", "village", "peak", "neighborhood",
                "park", "nature_preserve", "road", "stream", "university") 
name_data$country_match <- NA
for(this_id in name_data$id) {
  matches <- subset(full_matches, id==this_id)
  best_match <- NULL
  if(nrow(matches)>0) {
    for(type in priority_type) {
      temp <- subset(matches, components._type==type)
      if(nrow(temp)>0) {
        name_data$country_match[name_data$id==this_id] <- sample(unique(as.character(temp$components.country)),1)
        break
      }
    }
  }
}

  