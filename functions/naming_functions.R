#functions for random name generation

library(wrswoR)
library(here)
library(stringi)
library(magrittr)
load(here("name_generation","output","myth_sample.RData"))
load(here("name_generation","output","places_sample.RData"))
load(here("name_generation","output","surnames.RData"))
load(here("name_generation","output","name_corr.RData"))
load(here("name_generation","output","flavor_data.RData"))
nationalities <- read.csv(here("name_generation/output/name_nationality.csv"))

generate_system_names <- function(system, id=NA) {
  
  use_roman_planet_numbering <- FALSE
  use_arabic_planet_numbering <- FALSE
  base_name <- FALSE
  if(is.na(id)) {
    nationality <- sample_nationality()
  } else {
    nationality <- name_corr[name_corr$id==id,]
    #if no country iso for this planet, then resample to get nationality
    if(nrow(nationality)==0) {
      warning(paste("No nationality data found for id", id, sep=" "))
      nationality <- sample_nationality()
    } else {
      #determine if this system has a numbering system and if so, number the planets
      use_roman_planet_numbering <- grepl("\\s+(I|II|III|IV|V|VI|VII|VIII|IX|X|XI|XII|XIII|XIV|XV)$", 
                                          nationality$founding_name)
      #this is problematic because of things like Star Cluster 643. It turns out only Baker 3 uses
      #this in our data, so just change that one. 
      #use_arabic_planet_numbering <- grepl("\\s+\\d+$", nationality$founding_name)
      use_arabic_planet_numbering <- grepl("^Baker 3$", nationality$founding_name)
      
      #get base name for numbering
      if(use_roman_planet_numbering) {
        base_name <- trimws(gsub("\\s+(I|II|III|IV|V|VI|VII|VIII|IX|X|XI|XII|XIII|XIV|XV)$", "", 
                                 nationality$founding_name))
      }
      if(use_arabic_planet_numbering) {
        base_name <- trimws(gsub("\\s+\\d+$", "", nationality$founding_name))
      }
    }
  }
  
  ### Planets have some special things about naming
  isAsteroid <- system$planets$type=="Asteroid Belt"
  if(use_roman_planet_numbering) {
    system$planets$name[!isAsteroid] <- paste(base_name, convert_arabic2roman(1:sum(!isAsteroid)))
  }
  else if(use_arabic_planet_numbering) {
    system$planets$name[!isAsteroid] <- paste(base_name, 1:sum(!isAsteroid))
  } else {
    system$planets$name[!isAsteroid] <- sample_names(sum(!isAsteroid), "planet", nationality)
  }
  system$planets$name[isAsteroid] <- sample_names(sum(isAsteroid), "asteroid", nationality)
  
  system$planets$continent_names <- NA
  system$planets$capitol_name <- NA
  system$planets$moon_names <- NA
  
  for(i in 1:nrow(system$planets)) {

    #moons
    nmoons <- (system$planets$moons_giant+system$planets$moons_large+
                 system$planets$moons_medium)[i]
    if(nmoons > 0) {
      system$planets$moon_names[i] <- paste(sample_names(nmoons, "moon", nationality), 
                                            collapse=", ")
    }
    
    #capitol city name
    capitol <- NA
    if(system$planets$inhabitable[i]) {
      if(sample(1:100,1)<=10) {
        #some chance that you just inherit the planet name plus "City"
        capitol <- paste(system$planets$name[i], "City")
      } else {
        capitol <- sample_names(1, "city", nationality)
      }
    }
    
    #landmass names
    if(!is.na(system$planets$continents[i])) {
      if(system$planets$continents[i]==0) {
        if(system$planets$inhabitable[i]) {
          #just report capitol city
          system$planets$continent_names[i] <- paste("(", capitol, ")", sep="")
        }
      } else {
        continent_names <- sample_names(system$planets$continents[i], "continent", nationality)
        if(!is.na(system$planets$water[i]) && system$planets$water[i]>=90) {
          #then these should be archipelago
          continent_names <- paste(continent_names, "Archipelago")
        }
        
        #now add in capitol city
        if(sample(1:100,1)<=5) {
          #some chance that capitol is just "Landmass City"
          capitol <- paste(continent_names[1], "City")
        }
        
        #now concatenate first landmass and capitol city
        if(!is.na(capitol)) {
          continent_names[1] <- paste(continent_names[1], " (", capitol, ")", sep="")
        }
        
        #now combine
        system$planets$continent_names[i] <- paste(continent_names, collapse=",")
      }
    }
  }
  
  system <- add_easter_eggs(system, id)
  
  return(system)
}

sample_names <- function(n, object_type, nationality, continuity=0.8) {
  
  #probability of name type by object type
  #name types are place, surname, mythological, sequence
  #types of object are planet, moon, landmass, city
  probs <- cbind(c(30,40,25, 5),
                 c(10,25,35,20),
                 c(53,40, 5, 2),
                 c(55,40, 5, 0),
                 c(10,35,35,20))
  
  colnames(probs) <- c("planet","moon","continent","city","asteroid")
  rownames(probs) <- c("place","surname","mythological","sequence")

  #chance (out of 20) by object type that we just name one thing and then number
  #everything else
  #should be zero for planets, because we only do it if named planet does it
  chance_numbered <- c(0, 10, 3, 0, 10)
  names(chance_numbered) <- c("planet","moon","continent","city","asteroid")
  
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
      name <- add_flavor(sample_place_name(1, nationality),
                         object_type, "place", nationality$lgroup)
    }
    if(sources=="surname") {
      name <- add_flavor(sample_surname(1, nationality),
                         object_type, "surname", nationality$lgroup)
    }
    if(sources=="mythological") {
      name <- add_flavor(sample_myth_name(1, nationality),
                         object_type, "mythological", nationality$lgroup)
    }
    return(add_counter(name, n))
  } else {
    #lower the chance of geting a sequence later
    probs[4,] <- probs[4,]/4
  }
  
  #if we are still here then loop through and pick other sources with some
  #likelihood of maintaning continuity
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
      names[which(sources==source)] <- add_flavor(sample_place_name(tab[i], nationality),
                                                  object_type, "place", nationality$lgroup)
    } else if(source=="surname") {
      names[which(sources==source)] <- add_flavor(sample_surname(tab[i], nationality),
                                                  object_type, "surname", nationality$lgroup)
    } else if(source=="mythological") {
      names[which(sources==source)] <- add_flavor(sample_myth_name(tab[i], nationality),
                                                  object_type, "mythological", nationality$lgroup)
    } else {
      names[which(sources==source)] <- add_flavor(generate_sequence_names(tab[i]),
                                                  object_type, "sequence", nationality$lgroup)
    }
    
  }
  
  return(names)
}

sample_nationality <- function() {
  #sample a country from ones with non-NA country ISO
  temp <- na.omit(name_corr, !is.na(country_iso))
  return(temp[sample(1:nrow(temp),1),])
}


sample_place_name <- function(n=1, c=NULL, diversity=0.25) {
  tab <- table(name_corr$country_iso)
  country_list <- sample(names(tab), n, prob = tab, replace=TRUE)
  
  if(!is.null(c) && !is.na(c$country_iso) && (c$country_iso %in% places_sample$country)) {
    country_list[sample(c(FALSE, TRUE), n, 
                     replace = TRUE, 
                     prob=c(diversity, 1-diversity))] <- as.character(c$country_iso)
  }
  
  countries <- unique(country_list)
  chosen_names <- rep(NA, length(country_list))
  
  for(ucountry in countries) {
    sample_places <- subset(places_sample, country==ucountry)
    if(nrow(sample_places)==0) {
      #just use US
      sample_places <- subset(places_sample, country=="US")
      warning(paste("No country code", ucountry, "for place name sampling"))
    }
    n <- sum(country_list==ucountry)
    if(nrow(sample_places)<n) {
      #sample simply with replacement
      chosen_names[country_list==ucountry] <- sample_places$name[sample(1:nrow(sample_places), n,
                                                                        replace=TRUE)]
    } else {
      #sample w/o replacement and with weights
      chosen_names[country_list==ucountry] <- sample_places$name[sample_int_expj(nrow(sample_places), n,
                                                                                 prob=sample_places$weight)]
    }
  }
  return(chosen_names)
}

sample_myth_name <- function(n=1, c=NULL, diversity=0.1) {
  #we will use relative odds to sample pantheons. 
  odds <- c(5,1,3,3,1,15,10,10,1,3,5,25,3,10,10,3,10,5,5,20,5,20,10,5,5,1,1)
  names(odds) <-unique(myth_sample$pantheon)
  
  #increase odds based on country of settlers
  if(!is.null(c)) {
    if(!is.na(c$myth1) && c$myth1 %in% names(odds)) {
      odds[c$myth1] <- odds[c$myth1] * 8
    }
    if(!is.na(c$myth2) && c$myth2 %in% names(odds)) {
      odds[c$myth2] <- odds[c$myth2] * 4
    }
  }
  pantheon_list <- sample(unique(myth_sample$pantheon), n, prob = odds, replace=TRUE)
  
  #depending on diversity, maybe just repreat the first one in list
  pantheon_list[c(TRUE, sample(c(FALSE, TRUE), n-1, 
                               replace = TRUE, 
                               prob=c(diversity, 1-diversity)))] <- pantheon_list[1]
  
  pantheons <- unique(pantheon_list)
  chosen_names <- rep(NA, length(pantheon_list))
  
  for(upantheon in pantheons) {
    sample_pantheon <- subset(myth_sample, pantheon==upantheon)
    n <- sum(pantheon_list==upantheon)
    if(nrow(sample_pantheon)<n) {
      #sample simply with replacement
      chosen_names[pantheon_list==upantheon] <- sample_pantheon$name[sample(1:nrow(sample_pantheon), n,
                                                                      replace=TRUE)]
    } else {
      #sample w/o replacement and with weights
      chosen_names[pantheon_list==upantheon] <- sample_pantheon$name[sample_int_expj(nrow(sample_pantheon), n,
                                                                                     prob=sample_pantheon$popularity)]
    }
  }
  return(chosen_names)
}

sample_surname <- function(n=1, c=NULL, diversity=0.25) {
  tab <- table(name_corr$lgroup)
  lang_list <- sample(names(tab), n, prob = tab, replace=TRUE)
  
  if(!is.null(c) && !is.na(c$lgroup) && (c$lgroup %in% surnames$lgroup)) {
    lang_list[sample(c(FALSE, TRUE), n, 
                     replace = TRUE, 
                     prob=c(diversity, 1-diversity))] <- as.character(c$lgroup)
  }
  
  ulangs <- unique(lang_list)
  chosen_names <- rep(NA, length(lang_list))
  
  for(ulang in ulangs) {
    surname_sample <- subset(surnames, lgroup==ulang)
    n <- sum(lang_list==ulang)
    if(nrow(surname_sample)<n) {
      #sample simply with replacement
      chosen_names[lang_list==ulang] <- surname_sample$surname[sample(1:nrow(surname_sample), n,
                                                                        replace=TRUE)]
    } else {
      #sample w/o replacement and with weights
      chosen_names[lang_list==ulang] <- surname_sample$surname[sample_int_expj(nrow(surname_sample), n,
                                                                                 prob=surname_sample$weight)]
    }
  }
  return(chosen_names)
}


sub_flavor <- function(name, flavor) {
  return(sub("\\[NAME\\]",name,sample(flavor$name, 1, prob = flavor$frequency)))
}

#A function that adds flavor to the random names by source and type
add_flavor <- function(names, type, source, language) {
  
  for(i in 1:length(names)) {
    if(type=="planet") {
      if(source=="surname" && sample(1:5,1)<=2) {
        #40% chance of addition flavor
        names[i] <- sub_flavor(names[i], 
                               subset(surname_flavor, is.na(lgroup) | lgroup==language))
      }
      if(source=="place" && sample(1:10,1)==1) {
        #1 in 10 chance of New flavor - this seems pretty rare on canon planets
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
      #1 in 2 chance of city flavor
      if(source=="surname" && sample(1:2,1)>1) {
        names[i] <- sub_flavor(names[i], 
                               subset(city_flavor, is.na(lgroup) | lgroup==language))
        #some additional chance of New flavor as well
        if(sample(1:5,1)==1) {
          names[i] <- sub_flavor(names[i], subset(new_flavor, is.na(lgroup) | lgroup==language))
        }
      }
      if(source=="place"  && sample(1:2,1)>1) {
        #1 in 2 chance of New flavor
        names[i] <- sub_flavor(names[i], 
                               subset(new_flavor, is.na(lgroup) | lgroup==language))
      }
    }
    if(type=="moon") {
      #TODO: how about the name of the planet plus qualifier (e.g. Caph's Stone)
    }
    if(type=="asteroid") {
      #if surname, we should add possessive
      if(source=="surname") {
        names[i] <- paste(names[i], "'s", sep="")
      }
      #add belt
      names[i] <- paste(names[i], "Belt", sep=" ")
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
  connector <- sample(c(rep("",10), rep(" ",2), rep("-",8), rep(".",1)), 1)
  return(paste(prefix, 1:n, sep=connector))
}

generate_connector_names <- function(system) {
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
  if(roll<14 & n<=length(greek)) {
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

add_easter_eggs <- function(system, id) {

  idx <- which(system$planets$inhabitable)
  if(length(idx)==0) {
    return(system)
  }
  if(length(idx>1)) {
    idx <- idx[1]
  }
  
  # colony/city name Tanstaafl - Short of There ain't no such thing as a free lunch

  #substitute capitol cities
  capitol <- NA
  if(id=="Davisville") {
    capitol <- "New Chehalis"
  }
  if(id=="Dortmund") {
    capitol <- "Neu Schwarzgelben City"
  }
  if(id=="Köln (FWL)") {
    capitol <- "Beerockxstadt"
  }
  if(id=="Avior") {
    capitol <- "Corsucant"
  }
  
  if(!is.na(capitol)) {
    system$planets$continent_names[idx] <- sub("\\(.+?\\)", 
                                               paste("(",capitol,")",sep=""), 
                                               system$planets$continent_names[idx])
  }
  
  #substitute planet names
  possibles <- which(system$planets$type!="Asteroid Belt")
  possibles <- possibles[possibles!=idx]
  if(length(possibles)>1) {
    other_planet_idx <- sample(possibles, 1)
    if(id=="Gant") {
      system$planets$name[other_planet_idx] <- "Korriban"
    }
    if(id=="Gibbs") {
      system$planets$name[other_planet_idx] <- "Easter Egg"
    }
  }
  
  #Hastur must have Lovecraftian planet names
  if(id=="Hastur" & length(possibles)>1) {
    lovecraft_names <- c("Cthulhu","Nyarlathotep","Tsothuggua","Yog-Sothoth","Azathoth")
    n_possible <- length(possibles)
    if(n_possible>length(lovecraft_names)) {
      n_possible <- length(lovecraft_names)
      possibles <- possibles[1:n_possible]
    }
    system$planets$name[possibles] <- sample(lovecraft_names, n_possible, replace = FALSE)
    
  }
  
  
  return(system)
    
}

uses_roman_numbering <- function(x) {
  
}