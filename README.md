![faction changes](https://github.com/AaronGullickson/system_generation/blob/master/animated_gif/faction_animated.gif)

<img src="/animated_gif/faction_animated.gif?raw=true" width="200px">

# Battletech Planetary System Generation

This repository contains code written primarily in R that generates planetary systems for all canon star systems in the [Battletech universe](https://www.sarna.net/wiki/BattleTech_Universe), generally following the rules outlined in the [Campaign Operations](https://store.catalystgamelabs.com/products/battletech-campaign-operations-book) handbook. The goal is to develop a full set of planetary and social data that can be implemented in [MekHQ](https://megamek.org/). However, many of the functions created can be used by others to easily generate a single inhabited or uninhabited star system.

Although we generally follow the rules in Campaign Operations for system generation, there are a variety of tweaks that we have made to these rules. In addition we also needed some method of projecting social data such as population and socioindustrial codes in time. Details on how to do that are given further below.

## Generating a star system

Most of the heavy lifting for generating systems is done by the functions defined in the `system_creation_functions.R`. To generate a star system, you will first need to make sure you have all the necessary packages installed and install them if needed. You can do this by typing the following into an R console (from the top-level directory):

```{r}
source("check_packages.R")
```

You can then read in all of the nessary functions:

```r
source("functions/system_creation_functions.R")
source("functions/data_functions.R")
source("functions/naming_functions.R")
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

You can specify both `star` and `habit_pos` but keep in mind that this may force an inhabited planet outside of the life zone for that star type. Following FASAstronomy, the function will dutifully put your inhabited system in the requested spot. 

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
system <- add_colonization(system, distance_terra, founding_year, faction_type)
system
```

For this to run, you will need to supply three other pieces of information:

* `distance_terra` -  distance from Earth in light years
* `founding_year` - year the colony was founded
* `faction_type` - one of either "Clan", "IS", "Periphery", or "Minor"

The social information will be added to the system object. 

### Adding random names

You can also add random names for your planets, moons, landmasses, and capitol cities to your generated systems with the `generate_system_names` function:

```r
system <- generate_system()
system <- generate_system_names(system)
system
```

You can combine this with colonization feature to get the fullest features:

```r
system <- generate_system()
disance_terra <- 200
founding_year <- 2300
faction_type <- "IS"
system <- generate_system_names(add_colonization(system, distance_terra, founding_year, faction_type))
system
```

Random name generation pulls from a list of surnames for many different languages, a list of place names by country, and a list of mythological features. It samples from these lists based on estimation of what linguistic group and nationality most likely founded a planet based on reverse geo-coding planet names. It adds a variety of flavor components and some cultural variation. When specified without a specific planetary id, as above, it samples from the languages and nationalities of known planets in the battletech universe. 

## Changes From Campaign Ops System

In general, our system generation follows the rule in *Campaign Operations* as closely as possible. Below, we document places where we deviate from the rules. 

### Physical Planetary Features

Because the *Campaign Operations* system relies on discrete outcomes from dice rolls, the results for temperature and water coverage go up by even units of 10, which can lead to some "chunkiness" in the distribution of these values. We allow these values to vary by single digits by subtracting five and then drawing a number randomly between 0 and 9 (i.e. a d10 roll). This slightly changes the mean values because the mean of that random draw is 4.5 and not 5, but we preferred the aesthetics of whole numbers.

We also make several changes to the rules in the cases where we are forcing the system to produce a habitable planet. 

* We roll on the life friendly column for star type.
* We have tweaked the data on the outer life edge zone very slightly for M6V and M9V class planets because otherwise these planets would produce no planets in the life zone. 
* We randomly pick one slot within the life zone and continue to run the `generate_planet` function until we produce a habitable planet.
* We do not add the system level habitability modifier to any rolls for the planet.
* We add three to the atmospheric conditions roll to eliminate toxic atmospheres and reduce the frequency of tainted atmospheres (which are otherwise more than 40% of the cases).
* We add two to the water coverage roll on the assumption that colonizers would prefer planets with generous water. 
* We use different functions to determine diameter and density of terrestrials. We alter these functions to produce gravities closer to Earth and less variable. These functions are:
          * diameter = 9000+500*2d6
          * density = 3+1d6^0.75

### Colonization Features

#### Population

The table on pg. 123 of *Campaign Operations* gives base population numbers base on bandwidths away from Terra. This makes sense but the use of wide bandwidths produces some pretty chunky variation in population sizes. We used the base numbers and the midpoint of each bandwidth to estimate statistical models that predicted the right base number as a function of light years from Terra. This gives a nice smooth function.

The numbers for clan populations are extemely small and produce a total clan population well below the 1.2 billion cited in canon. We found that increasing the base numbers to 1 and 5 million for normal and high rolls, respectively, produced a total clan population very close to 1.2 billion, while still producing clan populations that were much smaller than typical Inner Sphere planets.

We also make one other change to population generation. According to the table on pg. 123 of *Campaign Operations*, more recent foundings only roll 2d6 rather than 4d6 for the random component of population that is multiplied by the base population to get final population. We found that this produced very small numbers for some distant periphery realms like the Hanseatic League because they were being double penalized by the base number roll and the multiplier when founded after the Star League. Therefore we use 4d6 as the base multiplier for all non-clan population multipliers.

#### USILR (Socio-industrial) Codes

The most important change to the USILR code system is that we apply a gamma distribution to our final result in order to create more variability and smoother distributions of USILR codes. To to this, we convert all letter codes to numerical codes (e.g. A,B,C,D,F becomes 5,4,3,2,1). After arriving at a final score *alpha* by the methods described in *Campaign Operations*, we then draw from a gamma distribution such that:

score = gamma(alpha, alpha/c)

Where *c* is a constant that allows us to scale the variation up or down. We use *c=0.2* for tech ratings and *c=0.1* for all other ratings. Because tech ratings play a role in most other codes, we wanted to generate more initial variation there and less variation in later codes. Because the gamma distribution produces a continuous value, we round to the nearest integer and then convert to the appropriate letter (after reducing or increasing numbers that fall outside the range). Mathematially, the mean of this distribution will be equal to *alpha* but will allow for some variation above and below. For the mathematically inclined, this approach is identical to fitting a poisson distribution with a mean of *alpha* but with underdispersion (smaller variance than the theoretical poisson).

The use of this approach helped to smooth out the distribution of ratings considerably. However the final results still frequently produced distributions that were too extreme in one direction or the other. The IS for example had almost exclusively A and B tech rated planets with a smattering of a few C-rated planets, while the minor periphery had nearly F on everything for all planets.

One of the nice features of using the gamma distribution described above is that the modifiers described on pg. 126 of *Campaign Operations* no longer have to be whole numbers. If we want to scale down the effect of one thing by 50% we can simply reduce its bonus/penalty from 1 to 0.5. Therefore, we tweaked a variety of numbers to produce smaller differentiation between cases and in particular to not penalize low population size as much. We also increased modifier sizes in a few cases. All of the modifiers we used are described in the tables below. 

We also made some additional changes for clan populations. Clan planets are supposedly fairly resource poor and clan populations are small, but supposedly with the strict clan system and high technology skill, the clans have been able to prosper on these planets. We removed all population based modifiers for the clans and also added some flat clan penalties to raw materials and agriculture. 

Please also note that we reverse the direction of these modifiers from *Campaign Operations*, so that "+" means higher rating.

##### Technology Modifiers

Base Value=4 (C). We do not use the Advanced or Regressed levels for Tech Rating by random generation.

| Condition                                      | Modifier  |
|:-----------------------------------------------|----------:|
| Star League or earlier                         |+0.5       |
| Population over 1 billion (non-Clan only)      |+1         |
| Clan settlement                                |+1.5       |
| Minor periphery                                |-0.25      |
| Population under 100 million (non-Clan only)   |-1         | 
| Population under 1 million (non-Clan only)     |-0.5       |

##### Industry Modifiers

Base Balue=2.5 (C-D)

| Condition                                      | Modifier  |
|:-----------------------------------------------|----------:|
| Tech Rating >= B                               |+0.75      |
| Clan settlement                                |+1.5       |
| Population over 4 billion (non-Clan only)      |+1         |
| Population over 1 billion (non-Clan only)      |+1         |
| Population under 100 million (non-Clan only)   |-0.5       | 
| Population under 1 million (non-Clan only)     |-0.25      |
| Tech Rating <= F                               |-0.75      |

##### Output Modifiers

Base Balue=3 (C)

| Condition                                      | Modifier  |
|:-----------------------------------------------|----------:|
| Clan settlement                                |+0.75      |
| Population over 4 billion (non-Clan only)      |+1         |
| Tech Rating >= A                               |+0.5       |
| Industry Rating >= B                           |+0.5       |
| Tech Rating <= D                               |-0.5       |
| Industry Rating <= D                           |-0.5       |

##### Raw Material Modifiers

Base Balue=4 (B)

| Condition                                      | Modifier  |
|:-----------------------------------------------|----------:|
| Clan settlement                                |-1.5       |
| Tech Rating >= C                               |+1         |
| Density over 5.5                               |+1         |
| Population over 3 billion (non-Clan only)      |-1         |
| Output Rating >= B                             |-1         |
| Settled over 250 years ago                     |-1         |
| Density under 4                                |-1         |

##### Agriculture Modifiers

Base Balue=3 (C)

| Condition                                      | Modifier  |
|:-----------------------------------------------|----------:|
| Clan settlement                                |-1         |
| Tech Rating >= B                               |+1         |
| Industry Rating >= C                           |+1         |
| Tech Rating <= F                               |-1         |
| Population over 1 billion (non-Clan only)      |-1         |
| Population over 5 billion (non-Clan only)      |-1         |
| Water Percentage under 50%                     |-1         |
| Tainted Atmosphere                             |-1         |
| Toxic Atmosphere                               |-2         | 

#### HPG Generation

We found that the -1 per 100 LY from Terra penalty was very harsh, so we reduced this penalty to -1 per 250 LY. We also thought these penalties were too severe for high tech worlds far from Terra. Logically, these planets should be ideal candidates for the First Circuit. So we removed the distance penalty altogether for A-rated tech planets.

#### Recharge Station Generation

We follow the table in *Campaign Operations* closely here but found it produced about 10% of inner sphere systems with a recharge stations whereas *Campaign Operations* suggests a a number closer to 25%. We added a positive +1 modifier for planets with an industry rating of B or higher and found that this got us much more reasonable results.

## Generating the Universe

Coming Soon

### Projection of Colonization Features
