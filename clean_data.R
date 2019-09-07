# Read in the raw XML dta for planets from MHQ and do some cleaning before producing systems

library(xml2)
library(magrittr)
library(rlist)
library(here)
library(stringr)
source(here("functions","system_creation_functions.R"))
source(here("functions","data_functions.R"))

planets <- read_xml(here("input","planets.xml"))
# We are no longer going to read in the existing planet events file
# because it just a bunch of non-canon junk
#events <- read_xml(here("input","0002_planetevents.xml"))
connectors <- read_xml(here("input","1000_connectors.xml"))

#now run through planets and separate out the events as well
#as connectors
new_planets <- xml_new_document() %>% xml_add_child("planets")
new_events <- xml_new_document() %>% xml_add_child("planets")

for(i in 1:xml_length(planets)) {
  planet <- xml_children(planets)[[i]]
  id <- xml_text(xml_find_first(planet, "id"))
  cat(id)
  
  ## Pull out all events and add them to the events XML
  events <- xml_find_all(planet, "event")
  
  #need to add a new planet to events
  pevent_node <- xml_add_child(new_events, "planet")
  xml_add_child(pevent_node, "id", id)
  for(event in events) {
    #ignore HPGs in events - I dont trust them
    if(!is.na(xml_find_first(event, "hpg"))) {
      next
    }
    #ignore name changes in events - they should already
    #be in the name change file
    if(!is.na(xml_find_first(event, "name"))) {
      next
    }
    xml_add_child(pevent_node, event)
  }
   
  # Check for connector
  name <- xml_text(xml_find_first(planet, "name"))
  if(grepl("^(DPR|ER|KC|TFS|NP|NC|HL|JF|Interstellar Expeditions|Cambridge Perimeter Defense|RWR Outpost|Transfer Station|Wolf Orbital|Transfer Facility)", name)) {
     xml_add_child(connectors, planet)
     next
  }

  # 
  # If we are still here cycle through nodes and spit out results to the 
  # new file
  planet_node <- xml_add_child(new_planets, "planet")
  for(node in xml_children(planet)) {
    #run a check on names to get rid of parenthetical stuff for planet number 
    #or name changes
    new_name <- NULL
    if(xml_name(node)=="name") {
      current_name <- xml_text(node)
      #if it ends in "+)" then take the part not in parenthesis
      if(grepl("\\+\\)$",current_name)) {
        new_name <- trimws(gsub("\\(.*\\)", "", current_name))
      }
      #if it ends in "-)" take the part in parenthesis minus any numbers and the -
      if(grepl("\\-\\)$",current_name)) {
        new_name <- trimws(gsub("(\\(|\\)|\\-|\\d+)", "", str_extract(current_name, "\\(.*\\)")))
      }
      #roman numerals
      if(grepl("\\w\\s+(I|II|III|IV|V|VI|VII|VIII|IX|X|XI|XII|XIII|XIV|XV)\\)", current_name)) {
        new_name <- trimws(gsub("(\\(|\\))", "", str_extract(current_name, "\\(.*\\)")))
      }
    }
    
    if(!is.null(new_name)) {
      #write the new name
      new_name_node <- xml_add_child(planet_node, "name", new_name)
    } else if(xml_name(node)!="event" & xml_name(node)!="pop") {
      #write the existing node
      xml_add_child(planet_node, node)
    }
    
  }
  cat("\n")
}

cat(as.character(new_planets), file = here("output","planets_initial.xml"))
cat(as.character(new_events), file = here("output","planetevents_initial.xml"))

#ok clean up connectors now
new_connectors <- xml_new_document() %>% xml_add_child("planets")
for(i in 1:xml_length(connectors)) {
  planet <- xml_children(connectors)[[i]]
  planet_node <- xml_add_child(new_connectors, "planet")
  for(node in xml_children(planet)) {
    if(xml_name(node)!="event" & xml_name(node)!="pop" & xml_name(node)!="desc") {
      xml_add_child(planet_node, node)
    }
  }
}

cat(as.character(new_connectors), file = here("output","connectors_initial.xml"))
