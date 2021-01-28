# Script for interpolation on the hexagon level
# Created by Kaija Gahm on 19 October 2020
# For the Shiny app, we need to generate interpolation grids for each sentence, preferably at three different levels of resolution. 
# In this script, I will:
#   1. Write a function to generate hex grids of a specific size
#   2. Generate three different hex grids at three different resolutions
#   3. Aggregate all data needed for the interpolations
#   4. For each sentence, generate three different interpolations (at the three different resolutions)
#   5. Save three different lists: one for each resolution, with an element for each sentence. 

# 1. Function to generate hexes -------------------------------------------
# Function to make two hex grids (nodes and polygons), given a continental US object and an nHex parameter. Outputs a list of length 2 with items "nodes" and "polygons"
# contUS is the input object containing all cities in the continental US. Usually, this will be contUS.Rda, which I load below when I actually use this function.
# contUSOutline is a shapefile 
# nHex is the number of hexagons going across the grid (I think?). Having trouble figuring out exactly what this parameter is doing, but basically, the more hexagons, the finer the grid. 30 is a good default, but for most visualizations you would want to go a little finer scale, maybe nHex = 50 or so.
makeHexes <- function(outlineContUS, nHex = 30){
  # Make hex point grid
  message("generating polygons...")
  hexNodes <- outlineContUS %>%
    sf::st_make_grid(n = nHex, # defaults to 30 but can be specified by user.
                     what = "centers", # points at the center of each hex
                     square = F # make the grid hexagonal, not square
                     ) %>%
    sf::st_as_sf() # convert to sf object.
    
  
  # Make hex polygon grid
  hexPolygons <- outlineContUS %>%
    sf::st_make_grid(n = nHex, 
                     # `what` defaults to polygons, whereas before we specified points.
                     square = F) %>%
    sf::st_as_sf() # convert to sf object
  
  # Crop each of these to outlineContUS
  inds <- st_contains(x = outlineContUS,
                      y = hexNodes) %>%
    unlist()
  
  hexNodes <- hexNodes[inds,]
  hexPolygons <- hexPolygons[inds,]
  
  l <- list(hexNodes, hexPolygons) %>% 
    setNames(c("nodes", "polygons"))
  return(l)
}

# 1.5. Load data and libraries. -------------------------------------------
# Libraries and database connection
library(RSQLite)
library(here)
library(tidyverse)
library(sf) # for spatial manipulations
library(viridis)
library(gstat) # for IDW interpolation

# Named group split function from Romain Francois
named_group_split <- function(.tbl, ...) {
  grouped <- group_by(.tbl, ...)
  names <- rlang::eval_bare(rlang::expr(paste(!!!group_keys(grouped), sep = " / ")))
  
  grouped %>% 
    group_split() %>% 
    rlang::set_names(names)
}

# Connect to the database -------------------------------------------------
con <- dbConnect(RSQLite::SQLite(), "../R/database/currentDB/ygdpDB.db")

# Load continental US outline ---------------------------------------------
a <- st_read(here("data", "interpolations", "USNation20m")) %>%
  st_cast(., to = "POLYGON") %>%
  st_transform(., crs = 2163) %>% # transform to planar
  mutate(area = st_area(.)) # calculate area
outlineContUS <- a %>%
  filter(area == max(area)) # grab the continental US (i.e. the shape with the greatest area)

# 2. Generate hex grids at three different resolutions --------------------
hexes30 <- makeHexes(outlineContUS, nHex = 30)
hexes50 <- makeHexes(outlineContUS, nHex = 50)
hexes75 <- makeHexes(outlineContUS, nHex = 75)


# 2.5 Check geometry types ------------------------------------------------
if(!(st_geometry_type(hexes30[[1]]) %>% unique() == "POINT")|!(st_geometry_type(hexes50[[1]]) %>% unique() == "POINT")|!(st_geometry_type(hexes75[[1]]) %>% unique() == "POINT")){
  stop("Error: the first element of at least one of the hex grids does not contain points. Double-check the arguments to st_make_grid in the makeHexes function.")
}


# 3. Get data needed for interpolations -----------------------------------
r <- tbl(con, "ratings") %>%
  filter(!is.na(rating)) %>%
  select(responseID, rating, sentenceID) %>%
  left_join(tbl(con, "demo_geo") %>% # join to raised cities
              select(responseID, raisedCityID),
            by = "responseID") %>%
  left_join(tbl(con, "cities") %>% select(cityID, countryID, lat, long), by = c("raisedCityID" = "cityID")) %>% # join to continental US to get state/country info
  filter(countryID == "USA") %>% # remove any that fall outside the continental US
  mutate(rating = as.numeric(as.character(rating))) %>% # make ratings numeric
  collect() %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(2163)

## split into a list where each element is the data for one sentence
s <- dbReadTable(con, "sentences") %>% select(sentenceID, sentenceText)
sentencesList <- r %>%
  left_join(s, by =  "sentenceID") %>%
  mutate(sentenceText = str_replace_all(sentenceText, "’", "'")) %>% # replace any smart quotes/apostrophes with straight ones.
  group_by(sentenceText) %>%
  named_group_split(., sentenceText)
names(sentencesList) # good!

# 4. For each sentence, generate three different interpolations -----------
# NOTE: before I did this work in R, Jim did interpolations in GIS, which ended up in the mapbook (https://ling.auf.net/lingbuzz/005277). I've used the same parameters here: power = 0.5, nPoints = 12. 
# function to generate interpolations
hexIDW <- function(sentenceData, 
                   hexGrids, 
                   nPoints = 12, # default value is from the mapbook
                   power = 0.5 # default value is from the mapbook
                   ){
  # Check that hexGrids is a list
  if(is.list(hexGrids) & length(hexGrids) == 2){
    hexNodes <- hexGrids[["nodes"]]
    hexPolygons <- hexGrids[["polygons"]]
  }else{
    stop("Argument `hexGrids` must be a list of length 2.")
  }
  
  # Make the gstat object
  rating_gs <- gstat(id = "predRating", formula = rating ~ 1, 
                     nmax = nPoints, # number of near points to use in idw
                     data = as(sentenceData, "Spatial"), 
                     set = list(idp = power)) # power level
  
  # Interpolate and predict
  cat("interpolating...")
  rating_idw <- predict(rating_gs, hexNodes) %>%
    st_transform(2163) %>%
    mutate(predRating.pred = case_when(predRating.pred > 5 ~ 5,
                                       predRating.pred < 1 ~ 1,
                                       TRUE ~ predRating.pred))
  
  # Join to polygons
  predPoly <- st_join(hexPolygons, rating_idw) %>%
    filter(!is.na(predRating.pred)) %>%
    rename(pred = predRating.pred) %>%
    select(-predRating.var)
  
  # Return polygons
  return(predPoly)
}

# Create the interpolation lists for each sentence
interpListLarge <- lapply(sentencesList, 
                          function(x) hexIDW(sentenceData = x, 
                                             hexGrids = hexes30)) %>%
  setNames(names(sentencesList))

interpListMedium <- lapply(sentencesList, 
                           function(x) hexIDW(sentenceData = x, 
                                              hexGrids = hexes50)) %>%
  setNames(names(sentencesList))

interpListSmall <- lapply(sentencesList, 
                          function(x) hexIDW(sentenceData = x, 
                                             hexGrids = hexes75)) %>%
  setNames(names(sentencesList))


# Check -------------------------------------------------------------------
if(!nrow(interpListLarge[[1]] != nrow(hexes30[[1]]))|!nrow(interpListMedium[[1]] != nrow(hexes50[[1]]))|!nrow(interpListSmall[[1]] != nrow(hexes75[[1]]))){
  stop("Number of hexes does not match the number in the input grid for at least one of the interpLists.")
}

# 5. Save the interpolation lists -----------------------------------------
save(interpListLarge, file = here("data", "interpolations", "interpListLarge.Rda"))
save(interpListMedium, file = here("data", "interpolations", "interpListMedium.Rda"))
save(interpListSmall, file = here("data", "interpolations", "interpListSmall.Rda"))

load(here("data", "interpolations", "interpListLarge.Rda"))
load(here("data", "interpolations", "interpListMedium.Rda"))
load(here("data", "interpolations", "interpListSmall.Rda"))
 
# test it out by plotting
# interpListSmall[[1]] %>% 
#   ggplot() + 
#   geom_sf(aes(fill = pred, col = pred))+
#   scale_fill_viridis()+
#   scale_color_viridis()+
#   ggtitle(names(interpListSmall)[1])
# 
# interpListMedium[[1]] %>% 
#   ggplot() + 
#   geom_sf(aes(fill = pred, col = pred))+
#   scale_fill_viridis()+
#   scale_color_viridis()+
#   ggtitle(names(interpListMedium)[1])
# 
# interpListLarge[[1]] %>% 
#   ggplot() + 
#   geom_sf(aes(fill = pred, col = pred))+
#   scale_fill_viridis()+
#   scale_color_viridis()+
#   ggtitle(names(interpListLarge)[1])


# make interp data frames -------------------------------------------------
large1 <- interpListLarge[[1]]
largeRest <- interpListLarge[-1]
largeRest <- lapply(largeRest, function(x) x %>% st_drop_geometry())
largeDF <- do.call(cbind, largeRest) %>% setNames(names(largeRest))
large <- bind_cols(large1, largeDF)
names(large)[1] <- names(interpListLarge)[1]
str(large, 1)
interpDFLarge <- large

medium1 <- interpListMedium[[1]]
mediumRest <- interpListMedium[-1]
mediumRest <- lapply(mediumRest, function(x) x %>% st_drop_geometry())
mediumDF <- do.call(cbind, mediumRest) %>% setNames(names(mediumRest))
medium <- bind_cols(medium1, mediumDF)
names(medium)[1] <- names(interpListMedium)[1]
str(medium, 1)
interpDFMedium <- medium

small1 <- interpListSmall[[1]]
smallRest <- interpListSmall[-1]
smallRest <- lapply(smallRest, function(x) x %>% st_drop_geometry())
smallDF <- do.call(cbind, smallRest) %>% setNames(names(smallRest))
small <- bind_cols(small1, smallDF)
names(small)[1] <- names(interpListSmall)[1]
str(small, 1)
interpDFSmall <- small

save(interpDFLarge, file = here("data", "interpolations", "interpDFLarge.Rda"))
save(interpDFMedium, file = here("data", "interpolations", "interpDFMedium.Rda"))
save(interpDFSmall, file = here("data", "interpolations", "interpDFSmall.Rda"))

# Survey sentences table --------------------------------------------------
s <- dbReadTable(con, "sentences") %>%
  mutate(sentenceText = str_replace_all(sentenceText, "’", "'"))

ss <- dbReadTable(con, "survey_sentences")
ss %>% pull(surveyID) %>% table()

surveySentencesTable <- dbReadTable(con, "sentences") %>%
  mutate(sentenceText = str_replace_all(sentenceText, "’", "'")) %>%
  left_join(ss %>% # survey_sentences, created above.
              select(sentenceID, surveyID)) %>%
  select(sentenceID, sentenceText, surveyID, constructionID) %>%
  distinct() %>%
  left_join(dbReadTable(con, "constructions") %>% 
              select(constructionID, constructionName),
            by = "constructionID")
save(surveySentencesTable, file = here("data", "interpolations", "surveySentencesTable.Rda"))
