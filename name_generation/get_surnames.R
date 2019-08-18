# read in surname data and convert numeric values to categories

surnames <- read.csv(here("name_generation","input","surnames.csv"), header=FALSE)

colnames(surnames) <- c("surname","weight","code")

ancestry <- read.csv(here("name_generation","input","surnames_masterancestry.csv"), header=FALSE)
colnames(ancestry) <- c("code","lgroup")

surnames <- merge(surnames, ancestry, all.x=TRUE, all.y=FALSE)
surnames <- subset(surnames,
                   select = c("surname","weight", "lgroup"))

surnames$surname <- as.character(surnames$surname)
save(surnames, file=here("name_generation", "output", "surnames.RData"))
