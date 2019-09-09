# Battletech Planetary System Generation

This repository contains code written primarily in R that generates planetary systems for all canon star systems in the [Battletech universe](https://www.sarna.net/wiki/BattleTech_Universe), generally following the rules outlined in the [Campaign Operations](https://store.catalystgamelabs.com/products/battletech-campaign-operations-book) handbook. The goal is to develop a full set of planetary and social data that can be implemented in [MekHQ](https://megamek.org/). However, many of the functions created can be used by others to easily generate a single inhabited or uninhabited star system.

Although we generally follow the rules in Campaign Operations for system generation, there are a variety of tweaks that we have made to these rules. In addition we also needed some method of projecting social data such as population and socioindustrial codes in time. Details on how to do that are given further below.

## Generating a star system

Most of the heavy lifting for generating systems is done by the functions defined in the `system_creation_functions.R`. To generate a star system, you will want to first type the following in to an R console (from the top-level directory):

```r
source("functions/system_creation_functions.R")
```

You will then, have several functions available. To generate an inhabited star system, type:

```r
generate_system()
```

By default, this function will generate an inhabited system. If you would prefer to generate any star system (which will most likely be uninhabited), type:

```r
generate_system(habitable=FALSE)
```

This function will also randomly generate a star type. If you want to specify the star type, you can do so with the `star` argument:

```r
generate_system(star="B2V", habitable=FALSE)
```

You can also specify a system position that you want the habitable planet to occupy:

```r
generate_system(habitable=FALSE, habit_pos)
```

In this case, the function will sample from a list of stars that can produce a planet in this life zone. 

You can specify both `star` and `habit_pos` but keep in mind that this may force an inhabited planet outside of the life zone for that star type. Following FASAstronomy, the function will dutifully create you inhabited system in the correct spot. 

### Adding Colonization Features 

You can add colonization features to your system as well. These include:

* population size
* socio-industrial codes
* HPG level
* recharge station

To add colonization features, save your output from `generate_system` to an object and then run `add_colonization` on that object.

```r
system <- generate_system()
disance_terra <- 200
founding_year <- 2300
faction_type <- "IS"
system <-add_colonzation(system, distance_terra, founding_year, faction_type)
system
```

For this to run, you will need to supply three other pieces of information:

* `distance_terra` -  distance from Earth in light years
* `founding_year` - year the colony was founded
* `faction_type` - one of either "Clan", "IS", "Periphery", or "Minor"

The social information will be added to the system object. 

### Adding random names

Coming Soon

## Changes From CamOps System

Coming Soon

## Generating the Universe

Coming Soon