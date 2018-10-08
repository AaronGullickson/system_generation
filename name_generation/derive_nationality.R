#derive likely nationality of planet by geoparsing the data using Open Cage

library(xml2)
library(magrittr)
library(opencage)

planets <- read_xml("data/planets.xml")

full_matches <- NULL

#since we can only make 2500 calls a day, lets
#first try a sample of 500 planets to see how well it works

sampled_idx <- sample(1:xml_length(planets), 500, replace=FALSE)
match_number <- NULL

for(i in sampled_idx) {
  
  planet <- xml_children(planets)[[i]]
  id <- xml_text(xml_find_first(planet, "id"))
  name <- xml_text(xml_find_first(planet, "name"))
  
  #clean up names
  temp <- strsplit(name, "\\(")[[1]][1]
  words <- strsplit(name, "\\s+")[[1]]
  search_name <- NULL
  for(word in words) {
    if(grepl("\\(", word) |
       grepl("\\d", word) |
       grepl("^(I|II|III|IV|V|VI|VII|VIII|IX|X|XI|XII|XIII|XIV|XV)$", word) |
       grepl("^New$", word)) {
      next
    }
    if(is.null(search_name)) {
      search_name <- word
    } else {
      search_name <- paste(search_name, word, sep=" ")
      
    }
  }
  results <- opencage_forward(search_name)

  cat(search_name)
  cat("....")
  
  if(!is.null(results$results)) {
    cat(paste(" found", nrow(results$results), "possible matches"))
    match_number <- c(match_number, nrow(results$results))
    #TODO: include language info
    full_matches <- rbind(full_matches, cbind(id, search_name, 
                                              results$results[,c("components.country",
                                                                 "components._type",
                                                                 "confidence")]))
  } else {
    cat(" NO MATCHES")
    match_number <- c(match_number, 0)
    full_matches <- rbind(full_matches, c(id, search_name, NA, NA, NA))
  }
  
  full_matches$id <- as.character(full_matches$id)
  full_matches$search_name <- as.character(full_matches$search_name)
  
  cat("\n")
}
  