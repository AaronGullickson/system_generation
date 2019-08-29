# read in surname data and convert numeric values to categories

#### Load Data ####

# load surnames file
surnames <- read.csv(here("name_generation","input","surnames.csv"), 
                     header=FALSE, as.is=1, col.names=c("surname","weight","code"))

# load ancestry correspondence file
ancestry <- read.csv(here("name_generation","input","surnames_masterancestry.csv"), 
                     header=FALSE, as.is=2, col.names=c("code","lgroup"))

#### Merge to get language group name ####
surnames <- merge(surnames, ancestry, all.x=TRUE, all.y=FALSE)
surnames <- subset(surnames,
                   select = c("surname","weight", "lgroup"))


#### Add in some additional surnames that are not in MM ####

add_surnames <- function(filename, lgroup) {
  surnames_add <- read.csv(here("name_generation","input","additional_surnames",filename), 
                           header=FALSE, 
                           as.is=1, 
                           col.names = c("surname","weight","code"))[,1:2]
  surnames_add$lgroup <- lgroup
  return(surnames_add)
}

surnames <- rbind(surnames,
                  add_surnames("names_persian.txt","Persian"),
                  add_surnames("names_burmese.txt","Burmese"),
                  add_surnames("names_israeli.txt","Hebrew"),
                  add_surnames("names_mongolian.txt","Mongolian"),
                  add_surnames("names_thai.txt","Thai"),
                  add_surnames("names_malay.txt","Malay"))


#### Save data ####
save(surnames, file=here("name_generation", "output", "surnames.RData"))
