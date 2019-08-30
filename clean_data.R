# Read in the raw XML dta for planets from MHQ and do some cleaning before producing systems

library(xml2)
library(magrittr)
library(rlist)
library(here)
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
  new_events <- xml_find_all(planet, "event")
  existing_planet <- get_planet_id(events, id)
  if(length(existing_planet)>0) {
    xml_add_child(existing_planet, new_events)
  } else {
    #need to add a new planet 
    planet_node <- xml_add_child(new_events, "planet")
    xml_add_child(planet_node, "id", id)
    for(new_event in new_events) {
      xml_add_child(planet_node, new_event)
    }
  }
  
  # Check for connector
  name <- xml_text(xml_find_first(planet, "name"))
  if(grepl("^(DPR|ER|KC|TFS|NP|NC|HL)", name)) {
    xml_add_child(connectors, planet)
    next
  }
  
  # If we are still here cycle through nodes and spit out results to the 
  # new file
  planet_node <- xml_add_child(new_planets, "planet")
  for(node in xml_children(planet)) {
    if(xml_name(node)!="event" & xml_name(node)!="pop") {
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
