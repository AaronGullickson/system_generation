#readr package to the rescue
library(readr)
library(here)
library(magrittr)
library(googlesheets)

#this is needed to authenticate, but will spit an error
gs_ls()
sheets <- gs_title("bad_place_names")

place_names <- read_delim(here("name_generation","output","place_names.tsv.gz"), 
                          "\t", escape_double = FALSE, col_names = FALSE, 
                          trim_ws = TRUE,
                          progress = TRUE)

colnames(place_names) <- c("name","feature_class","feature_code","country","population")
place_names$feature_class <- factor(place_names$feature_class)
place_names$feature_code <- factor(place_names$feature_code)
place_names$country <- factor(place_names$country)


#restrict by feature code
places <-subset(place_names, feature_code=="ADM1" | feature_code=="ADM2" | feature_code=="ADM3" |
                  feature_code=="ADM4" | feature_code=="ADM5" | feature_code=="PPLA" | feature_code=="PPLA2" |
                  feature_code=="PPLA3" | feature_code=="PPLA4" | feature_code=="PPLC")


#set weights equal to the log of population size. If population is missing, then assign the lowest known
#value within the country. If no populations for the whole country, then multiply weights for admin divisions and 
#administative capitals in some fashion. 

pop_mins <- subset(places, population>0) %$% tapply(population,country,function(x) {
  y <- min(x)
  if(y<20) {
    y <- 20
  }
  return(y)})


places$population[places$population==0] <- pop_mins[places$country[places$population==0]]
places$weight <- places$population

TF <- places$feature_code=="PPLC" | places$feature_code=="PPLA"
places$weight[TF] <- 20*places$weight[TF]
TF <- places$feature_code=="PPLA2"
places$weight[TF] <- 15*places$weight[TF]
TF <- places$feature_code=="PPLA3"
places$weight[TF] <- 5*places$weight[TF]

TF <- places$feature_code=="ADM1"
places$weight[TF] <- 8*places$weight[TF]
TF <- places$feature_code=="ADM2"
places$weight[TF] <- 6*places$weight[TF]
TF <- places$feature_code=="ADM3"
places$weight[TF] <- 4*places$weight[TF]
TF <- places$feature_code=="ADM4"
places$weight[TF] <- 2*places$weight[TF]

#log the weight
places$weight <- log(places$weight)

#TODO: check missing values on weight

## Do a bunch of clean up at least on English language stuff to remove excess

#



#anything with a / get rid of what comes after. 
places$name <- trimws(gsub("/.*","", places$name))

#remove any cases with numbers, pipe, or comma in them
places <- places[grepl("\\d+|\\||\\,|\\’|\\‘",places$name)==FALSE,]

places <- places[grepl("Northern|Southern|Western|Eastern|Council|National Capitol|Community|Central",places$name, 
                       ignore.case = TRUE)==FALSE,]

#remove anything in parenthesis
places$name <- trimws(gsub("\\(.*?\\)","", places$name))

#get rid of entries with wrong words
wrong_words <- gs_read(sheets, "remove entry")$phrase
places <- places[grepl(paste(wrong_words, collapse="|"), places$name, ignore.case = TRUE)==FALSE,]

#get rid of all the bad words
bad_words <- gs_read(sheets, "remove words")$phrase
places$name <- stri_trim_both(stri_replace_all_regex(places$name, paste(bad_words, collapse="|"), "", case_insensitive=TRUE))

#get rid of any of, and, du, de, di, etc. if they are at the beginning of term
places$name <- trimws(gsub("^\\s?(and|of|du|di|de|del)", "", places$name))


#this one is causing problems in combination
places$name <- trimws(gsub("\\sSum$",  "", places$name))

#trim whitespace one more time for sures
places$name <- trimws(places$name)

#remove any single letters or blanks
places <- places[nchar(places$name)>1,]

#combine same names together with max of weight
places <- places[order(places$weight, decreasing = TRUE),]
places <- places[duplicated(places[,c("name","country")])==FALSE,]


places_sample <- as.data.frame(subset(places, select = c("name","country","weight")))
places_sample <- places_sample[order(places_sample$country, places_sample$name),]
save(places_sample, file=here("name_generation","output","places_sample.RData"))
