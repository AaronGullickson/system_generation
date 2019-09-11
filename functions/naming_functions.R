#functions for random name generation

library(wrswoR)
library(here)
library(stringi)
load(here("name_generation","output","myth_sample.RData"))
load(here("name_generation","output","places_sample.RData"))
load(here("name_generation","output","surnames.RData"))
load(here("name_generation","output","name_corr.RData"))
load(here("name_generation","output","flavor_data.RData"))
nationalities <- read.csv(here("name_generation/output/name_nationality.csv"))

generate_system_names <- function(system, id=NA) {
  
  nationality <- name_corr[name_corr$id==id,]
  
  #determine if this system has a numbering system and if so, number the planets
  use_roman_planet_numbering <- grepl("\\s+(I|II|III|IV|V|VI|VII|VIII|IX|X|XI|XII|XIII|XIV|XV)$", 
                                      nationality$founding_name)
  #this is problematic because of things like Star Cluster 643. It turns out only Baker 3 uses
  #this in our data, so just change that one. 
  #use_arabic_planet_numbering <- grepl("\\s+\\d+$", nationality$founding_name)
  use_arabic_planet_numbering <- grepl("^Baker 3$", nationality$founding_name)
  
  #get base name for numbering
  base_name <- ""
  if(use_roman_planet_numbering) {
    base_name <- trimws(gsub("\\s+(I|II|III|IV|V|VI|VII|VIII|IX|X|XI|XII|XIII|XIV|XV)$", "", nationality$founding_name))
  }
  if(use_arabic_planet_numbering) {
    base_name <- trimws(gsub("\\s+\\d+$", "", nationality$founding_name))
  }
  
  #if no country iso for this planet, then resample to get nationality
  if(is.na(nationality$country_iso)) {
    nationality <- sample_nationality()
  }
  
  ### Planets have some special things about naming
  isAsteroid <- system$planets$type=="Asteroid Belt"
  #TODO: even for the base naming we should potentially name asteroid belts differently
  if(use_roman_planet_numbering) {
    system$planets$name[!isAsteroid] <- paste(base_name, convert_arabic2roman(1:sum(!isAsteroid)))
  }
  if(use_arabic_planet_numbering) {
    system$planets$name[!isAsteroid] <- paste(base_name, 1:sum(!isAsteroid))
  } else {
    system$planets$name[!isAsteroid] <- sample_names(sum(!isAsteroid), "planet", nationality)
    system$planets$name[isAsteroid] <- sample_names(sum(isAsteroid), "planet", nationality)
  }
  #TODO: asteroid should be an object type and this should be flavor
  system$planets$name[isAsteroid] <- paste(system$planets$name[isAsteroid], "Belt")
  
  system$planets$continent_names <- NA
  system$planets$capitol_name <- NA
  system$planets$moon_names <- NA
  
  for(i in 1:nrow(system$planets)) {
    #the correct name for focal planet will be corrected when we check canon data
    nmoons <- (system$planets$moons_giant+system$planets$moons_large+
                 system$planets$moons_medium)[i]
    if(nmoons > 0) {
      system$planets$moon_names[i] <- paste(sample_names(nmoons, "moon", nationality), 
                                            collapse=", ")
    }
    if(!is.na(system$planets$continents[i])) {
      system$planets$continent_names[i] <- paste(sample_names(system$planets$continents[i],
                                                              "continent", nationality), 
                                                 collapse=", ")
    }
    if(!is.na(system$planets$population[i])) {
      if(sample(1:100,1)<=10) {
        #some chance that you just inherit the planet name plus "City"
        system$planets$capitol_name[i] <- paste(system$planets$name[i], "City")
      } else if(sample(1:100,1)<=5 && !is.na(system$planets$continent_names[i]) && 
                nchar(system$planets$continent_names[i])>0) {
        system$planets$capitol_name[i] <- paste(strsplit(system$planets$continent_names[i], ",")[[1]][1], 
                                                "City")
      } else {
        system$planets$capitol_name[i] <- sample_names(1, "city", nationality)
      }
    }
  }
  
  # TODO: easter eggs
  # New Chehalis somewhere
  # Dortmund's capital city is Neu Schwarzgelben City
  # Koln's capital city is Beerockxstadt
  # Somewhere in periphery named Korriban
  # Something named Easter Egg
  # colony/city name Tanstaafl - Short of There ain't no such thing as a free lunch
  # capital named Corsucant
  
  return(system)
}

sample_names <- function(n, object_type, nationality, continuity=0.8) {
  
  #probability of name type by object type
  #name types are place, surname, mythological, sequence
  #types of object are planet, moon, landmass, city
  probs <- cbind(c(40,40,15, 5),
                 c(10,25,35,20),
                 c(53,40, 5, 2),
                 c(55,40, 5, 0))
  
  colnames(probs) <- c("planet","moon","continent","city")
  rownames(probs) <- c("place","surname","mythological","sequence")

  #chance (out of 20) by object type that we just name one thing and then number
  #everything else
  #should be zero for planets, because we only do it if named planet does it
  chance_numbered <- c(0, 10, 3, 0)
  names(chance_numbered) <- c("planet","moon","continent","city")
  
  sources <- NULL
  
  #choose the first source
  sources <- sample(rownames(probs), 1, prob = probs[,object_type])
  
  if(sources=="sequence") {
    #if sequence was chosen, then use sequence for everything
    continuity <- 1
  } else if(n > 1 & sample(1:20,1)<=chance_numbered[object_type]) {
    #some chance we just repeat the first name drawn with some counting
    #qualifier
    if(sources=="place") {
      name <- add_flavor(sample_place_name(1, 
                                           as.character(nationality$country_iso)),
                         object_type, "place", nationality$lgroup)
    }
    if(sources=="surname") {
      name <- add_flavor(sample_surname(1, 
                                        as.character(nationality$lgroup)),
                         object_type, "surname", nationality$lgroup)
    }
    if(sources=="mythological") {
      name <- add_flavor(sample_myth_name(1),
                         object_type, "mythological", nationality$lgroup)
    }
    return(add_counter(name, n))
  } else {
    #lower the chance of geting a sequence later
    probs[4,] <- probs[4,]/4
  }
  
  #if we are still here then loop through and pick other sources with some
  #likelihood of maintaing continuity
  if(n>1) {
    for(i in 2:n) {
      if(sample(c(FALSE,TRUE), 1, prob=c(1-continuity, continuity))) {
        #maintain continuity by sampling from available sources
        sources <- c(sources, sample(sources, 1))
      } else {
        #pick a source unconditionally
        sources <- c(sources, sample(rownames(probs), 1, prob = probs[,object_type]))
      }
    }
  }
    
  names <- rep(NA, length(sources))
  
  tab <- table(sources)
  for(i in 1:length(tab)) {
    source <- names(tab)[i]
    if(source=="place") {
      names[which(sources==source)] <- add_flavor(sample_place_name(tab[i], 
                                                                    as.character(nationality$country_iso)),
                                                  object_type, "place", nationality$lgroup)
    } else if(source=="surname") {
      names[which(sources==source)] <- add_flavor(sample_surname(tab[i], 
                                                                 as.character(nationality$lgroup)),
                                                  object_type, "surname", nationality$lgroup)
    } else if(source=="mythological") {
      names[which(sources==source)] <- add_flavor(sample_myth_name(tab[i]),
                                                  object_type, "mythological", nationality$lgroup)
    } else {
      names[which(sources==source)] <- generate_sequence_names(tab[i])
    }
    
  }
  
  return(names)
}

sample_nationality <- function() {
  #sample a country from ones with non-NA country ISO
  temp <- na.omit(name_corr, !is.na(country_iso))
  return(temp[sample(1:nrow(temp),1),])
}


sample_place_name <- function(n=1, c=NULL) {
  if(is.null(c) || is.na(c)) {
    #when country is missing, we will assume the same distribution
    #as for when countries are present
    tab <- table(name_corr$country_iso)
    c <- sample(names(tab), 1, prob = tab)
  }
  sample_country <- subset(places_sample, country==c)
  
  if(nrow(sample_country)==0) {
    warning(paste("No country named",c))
    return(NA)
  }
  if(n > nrow(sample_country)) {
    n <- nrow(sample_country)
  }
  sample_idx <- sample_int_expj(nrow(sample_country),
                                n,
                                prob=sample_country$weight)
  return(as.character(sample_country$name[sample_idx]))
}

sample_myth_name <- function(n=1, c=NULL) {
  #we will use relative odds to sample pantheons. 
  odds <- c(5,1,3,3,1,15,10,10,1,3,5,25,3,10,10,3,10,5,5,20,5,20,10,5,5,1,1)
  names(odds) <-unique(myth_sample$pantheon)
  if(!is.null(c)) {
    if(!is.na(c$myth1) && c$myth1 %in% names(odds)) {
      odds[c$myth1] <- odds[c$myth1] * 8
    }
    if(!is.na(c$myth2) && c$myth2 %in% names(odds)) {
      odds[c$myth2] <- odds[c$myth2] * 4
    }
  }
  p <- sample(unique(myth_sample$pantheon), 1, prob = odds)
  sample_pantheon <- subset(myth_sample, pantheon==p)
  if(n > nrow(sample_pantheon)) {
    n <- nrow(sample_pantheon)
  }
  sample_idx <- sample_int_expj(nrow(sample_pantheon),
                                n,
                                prob=sample_pantheon$popularity)
  return(as.character(sample_pantheon$name[sample_idx]))
}

sample_surname <- function(n=1, l=NULL) {
  if(is.null(l) || is.na(l)) {
    #when lgroup is missing, we will assume the same distribution
    #as for when its present
    tab <- table(name_corr$lgroup)
    l <- sample(names(tab), 1, prob = tab)
  }
  surname_sample <- subset(surnames, lgroup==l)
  if(nrow(surname_sample)==0) {
    warning(paste("No language group named",l))
    return(NA)
  }
  if(n > nrow(surname_sample)) {
    n <- nrow(surname_sample)
  }
  sample_idx <- sample_int_expj(nrow(surname_sample),
                                n,
                                prob=surname_sample$weight)
  return(surname_sample$surname[sample_idx])
}


sub_flavor <- function(name, flavor) {
  return(sub("\\[NAME\\]",name,sample(flavor$name, 1, prob = flavor$frequency)))
}

#A function that adds flavor to the random names by source and type
add_flavor <- function(names, type, source, language) {
  
  for(i in 1:length(names)) {
    if(type=="planet") {
      if(source=="surname" && sample(1:3,1)==1) {
        #1 in 3 chance of addition flavor
        names[i] <- sub_flavor(names[i], 
                               subset(surname_flavor, is.na(lgroup) | lgroup==language))
      }
      if(source=="place" && sample(1:3,1)==1) {
        #1 in 3 chance of flavor
        names[i] <- sub_flavor(names[i], 
                               subset(new_flavor, is.na(lgroup) | lgroup==language))
      }
    }
    if(type=="continent") {
      if(source=="surname" && sample(1:4,1)==1) {
        #1 in 4 chance of addition flavor
        names[i] <- sub_flavor(names[i], 
                               subset(surname_flavor, is.na(lgroup) | lgroup==language))
      }
      if(source=="place" && sample(1:3,1)==1) {
        #1 in 3 chance of New flavor
        names[i] <- sub_flavor(names[i], 
                               subset(new_flavor, is.na(lgroup) | lgroup==language))
      }
    }
    if(type=="city") {
      if(source=="surname" && sample(1:3,1)>1) {
        names[i] <- sub_flavor(names[i], 
                               subset(city_flavor, is.na(lgroup) | lgroup==language))
        #some additional chance of New flavor as well
        if(sample(1:5,1)==1) {
          names[i] <- sub_flavor(names[i], subset(new_flavor, is.na(lgroup) | lgroup==language))
        }
      }
      if(source=="place"  && sample(1:3,1)>1) {
        #1 in 3 chance of New flavor
        names[i] <- sub_flavor(names[i], 
                               subset(new_flavor, is.na(lgroup) | lgroup==language))
      }
    }
    if(type=="moon") {
      ##nothing here yet
    }
  }
  
  # change landmass names to big/small
  if(type=="continent" & length(names)>=2 & sample(1:20, 1)<=3) {
    idx <- sample(1:length(names), 2, replace = FALSE)
    dupe_name <- names[idx[1]]
    roll <- sample(1:3, 1)
    if(roll==1) {
      names[idx] <- paste(c("Big","Little"), dupe_name, sep=" ")
    } else if(roll==2) {
      names[idx] <- paste(c("Larger","Smaller"), dupe_name, sep=" ")
    } else {
      names[idx] <- paste(c("Greater","Lesser"), dupe_name, sep=" ")
    }
  }
  
  return(names)
  
}

generate_sequence_names <- function(n) {
  numChars <- sample(1:3, 1)
  prefix <- stri_rand_strings(1, numChars, "[A-Z0-9]")
  connector <- sample(c("","","",""," ","-","-","-","-",":","."), 1)
  return(paste(prefix, connector, 1:n, sep=""))
}

generate_connector_names <- function(system) {
  #TODO: use the same random sequence scheme for asteroid belts but attach something like "a" or ".1"
  isAsteroid <- system$planets$type=="Asteroid Belt"
  system$planets$name[!isAsteroid] <- generate_sequence_names(sum(!isAsteroid))
  system$planets$name[isAsteroid] <- paste(generate_sequence_names(sum(isAsteroid)), "Belt")
  system$planets$continent_names <- NA
  system$planets$capitol_name <- NA
  system$planets$moon_names <- NA
  for(i in 1:nrow(system$planets)) {
    nmoons <- (system$planets$moons_giant+system$planets$moons_large+
                 system$planets$moons_medium)[i]
    if(nmoons > 0) {
      system$planets$moon_names[i] <- paste(generate_sequence_names(nmoons), collapse=",")
    }
    if(!is.na(system$planets$continents[i])) {
      system$planets$continent_names[i] <- paste(generate_sequence_names(system$planets$continents[i]),
                                                 collapse=",")
    }
  }
  return(system)
}  

add_counter <- function(name, n, sep=" ") {
  greek <- c("Alpha","Beta","Gamma","Delta","Epsilon","Zeta",
             "Eta","Theta","Iota","Kappa","Lambda","Mu","Nu",
             "Xi","Omicron","Pi","Rho","Sigma","Tau","Upsilon",
             "Phi","Chi","Psi","Omega")
  roll <- sample(1:20, 1)
  if(roll<8 & n<=30) {
    #use roman
    return(paste(name, convert_arabic2roman(1:n), sep=sep))
  }
  if(roll<12 & n<=length(LETTERS)) {
    #use capital letters
    return(paste(name, LETTERS[1:n], sep=sep))
  }
  if(roll<13 & n<=length(letters)) {
    #use capital letters
    return(paste(name, letters[1:n], sep=sep))
  }
  if(roll<15 & n<=length(greek)) {
    #use greek
    
    return(paste(name, greek[1:n], sep=sep))
  }
  #if we are still here just use numbers
  return(paste(name, 1:n, sep=sep))
}

convert_roman2arabic <- function(roman) {
  if(roman=="I") {
    return(1)
  }
  if(roman=="II") {
    return(2)
  }
  if(roman=="III") {
    return(3)
  }
  if(roman=="IV") {
    return(4)
  }
  if(roman=="V") {
    return(5)
  }
  if(roman=="VI") {
    return(6)
  }
  if(roman=="VII") {
    return(7)
  }
  if(roman=="VIII") {
    return(8)
  }
  if(roman=="IX") {
    return(9)
  }
  if(roman=="X") {
    return(10)
  }
  if(roman=="XI") {
    return(11)
  }
  if(roman=="XII") {
    return(12)
  }
  if(roman=="XIII") {
    return(13)
  }
  if(roman=="XIV") {
    return(14)
  }
  if(roman=="XV") {
    return(15)
  }
  return(16)
}

convert_arabic2roman <- function(arabic) {
  arabic[arabic>20] <- 20
  roman <- c("I","II","III","IV","V","VI","VII","VIII","IX","X",
             "XI","XII","XIII","XIV","XV","XVI","XVII","XVIII","XIX","XX",
             "XXI","XXII","XXIII","XXIV","XXV","XXVI","XXVII","XXVIII","XXIX","XXX")
  return(roman[arabic])
}