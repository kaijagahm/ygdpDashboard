# Script for dashboard data pre-processing
# Created by Kaija Gahm on 21 October 2020
# The data in this script will be sourced from the database, in the "database" folder, and written out to the "data/outputs" folder. Then when I have final data versions, I will copy them over to the app data folder.

# Load packages
library(here)
library(RSQLite)
library(tidyverse)

# Named group split function from Romain Francois
named_group_split <- function(.tbl, ...) {
  grouped <- group_by(.tbl, ...)
  names <- rlang::eval_bare(rlang::expr(paste(!!!group_keys(grouped), sep = " / ")))
  
  grouped %>% 
    group_split() %>% 
    rlang::set_names(names)
}

# Connect to the database
con <- dbConnect(RSQLite::SQLite(), here("data", "ygdpDB.db"))

## test out survey 7
rs7 <- tbl(con, "responses") %>%
  filter(surveyID == "S7") %>%
  collect() %>%
  pull(responseID)
dgs7 <- tbl(con, "demo_geo") %>%
  filter(responseID %in% rs7) %>% 
  collect()

# Grab the data for all ratings: we need surveyID, sentenceID, sentenceText, constructionID, age, age bin, raceCats, gender, education, rating. Then make that into a list of data frames, one for each sentence, and then nest that under a list of surveys.

r <- tbl(con, "ratings") %>%
  select(responseID, surveyID, sentenceID, rating) %>%
  left_join(tbl(con, "sentences") %>% # join sentence information: sentenceText, construction
              select(sentenceID, sentenceText, constructionID), 
            by = "sentenceID") %>%
  left_join(tbl(con, "demo_geo") %>% # join demo information
              select(responseID, raisedCityID, gender, age, raceCats, education),
            by = "responseID") %>%
  left_join(tbl(con, "cities") %>%
              select(cityID, stateID, countryID, lat, long),
            by = c("raisedCityID" = "cityID")) %>%
  filter(countryID == "USA") %>% # only people raised in the USA
  select(-countryID) %>%
  collect() # retrieve the data

# Join urban/rural designations
## get urban/rural data
ur <- tbl(con, "census_urban_areas") %>%
  select(cityID, UATYP10) %>%
  distinct() %>%
  collect()

## make sure there aren't any duplicates
ur %>%
  group_by(cityID) %>%
  filter(n() > 1) # 0 rows, good.
length(unique(ur$cityID)) == nrow(ur) # TRUE, good.

## Bind to `r`
nrow(r)
r <- r %>%
  left_join(ur,
            by = c("raisedCityID" = "cityID"))
nrow(r) # row count is the same.

## Edit the categories
r <- r %>%
  rename("urbanRural" = UATYP10) %>%
  mutate(urbanRural = forcats::fct_recode(urbanRural,
                                          "urban" = "U",
                                          "urban cluster" = "C"),
         urbanRural = case_when(is.na(urbanRural) ~ "rural",
                                TRUE ~ as.character(urbanRural)))

# test s7
r %>%
  filter(responseID %in% rs7) %>%
  View() # looks good

# Remove CU/CG sentences and 0 ratings  --------------------------------------
r <- r %>%
  filter(!(constructionID %in% c("CG", "CU"))) %>%
  filter(rating != 0) # these got in there because of a problem in the sql script.


# THIS IS REALLY IMPORTANT! Because of the part in the dat() reactiveValue where we check whether each responseID only appears as many times as there are sentences, we will get a really confusing and pernicious error if we allow any responseID's to correspond to more than one rating per sentence.
# For the 1054/1032 issue, only keep 1032 ---------------------------------
r <- r %>%
  group_by(responseID, surveyID, sentenceText) %>%
  slice(1) %>% # just take the first of these sentences
  ungroup()

## Confirm that this went through
r %>% filter(surveyID == "S5", sentenceID %in% c("1054", "1032")) %>% 
  pull(sentenceID) %>% table() # yes, we only see 1054 here.

# Add construction names -------------------------------------------------
c <- dbReadTable(con, "constructions") %>%
  select(constructionID, constructionName)

nrow(r) # 276162
r <- r %>%
  left_join(c, by = "constructionID")
nrow(r) # 276162, good (or should be same as previous.)

# Remove people younger than 18 and create ageBins
r <- r %>%
  mutate(age = as.numeric(age)) %>%
  filter(age > 18 | is.na(age)) %>%
  mutate(ageBin = cut(age, breaks = c(17, 30, 40, 50, 60, 70),
                      labels = c("18-30", "31-40", "41-50", "51-60", ">61")))


# Data checks -------------------------------------------------------------
table(r$surveyID, exclude = NULL) # now includes survey 13 data.
table(r$sentenceID, exclude = NULL) # have to set the "NA"s to "1295" (see idw_hex_interpolation_2 and GH issue #46)
r <- r %>%
  mutate(sentenceID = case_when(sentenceID == "NA" ~ "1295",
                                TRUE ~ sentenceID))
table(r$rating, exclude = NULL) # ratings look good.

# Remove any zeroes
r <- r %>%
  filter(rating != 0 & !is.na(rating))

# Check for NA sentence text
r %>% filter(is.na(sentenceText)|sentenceText == "NA") # no NA sentences

# Remove all leading and trailing spaces from the sentenceText column, and replace smart quotes with straight ones.
r <- r %>%
  mutate(sentenceText = str_replace(sentenceText, "^\\s", ""),
         sentenceText = str_replace(sentenceText, "\\s$", ""),
         sentenceText = str_replace_all(sentenceText, "’", "'"))

unique(r$sentenceText, exclude = NULL) # that looks a lot better. 

table(r$constructionID, exclude = NA)
# which sentences are problematic?
r %>%
  filter(constructionID == "check") %>%
  select(constructionID, sentenceID) %>%
  distinct() # no more problems!

# Aggregate gender categories ---------------------------------------------
table(r$gender, exclude = NULL) # going to aggregate these to female, male, and other
r <- r %>%
  mutate(gender = fct_recode(gender,
                             other = "not sure",
                             male = "transgender (ftm)",
                             female = "transgender (mtf)"))

r %>%
  filter(responseID %in% rs7) %>%
  View() # looks good

table(r$raceCats, exclude = NULL)
r <- r %>% # replace "NA" with <NA>
  mutate(raceCats = case_when(raceCats == "NA" ~ NA_character_,
                              TRUE ~ raceCats))


# Exclude control sentences from the app ----------------------------------
r <- r %>%
  filter(!(constructionID %in% c("CG", "CU")))

# Fix education levels ----------------------------------------------------
table(r$education, exclude = NULL) # these need to be standardized
r <- r %>%
  mutate(education = forcats::fct_recode(education,
                                         "associate" = "associate",
                                         "associate" = "associates", 
                                         "bachelor's" = "bachelor's",
                                         "graduate" = "graduate",
                                         "high school diploma" = "high school diploma",
                                         "some college" = "some college",
                                         "some high school" = "some high school"))


# Jitter lat/long ---------------------------------------------------------
# We're going to pull out lat/long by person, find the ones that are the same, and jitter them slightly.

## get unique people and their lat/long coords
personLatLong <- r %>%
  select(responseID, surveyID, lat, long) %>%
  distinct()

## group by lat/long and surveyID. 
personLatLong <- personLatLong %>%
  group_by(lat, long, surveyID) %>%
  arrange(lat, long, surveyID) %>%
  mutate(n = n()) %>%
  ungroup() 

## If the group contains more than one element, then jitter
needsFix <- personLatLong %>%
  filter(n > 1)
doesntNeedFix <- personLatLong %>%
  filter(n == 1) %>%
  mutate_at(vars(lat, long), .funs = function(x) as.numeric(x, length = 6))
n <- nrow(needsFix)
addLat <- runif(n, min = -0.0005, max = 0.0005)
addLong <- runif(n, min = -0.0005, max = 0.0005)
needsFix <- needsFix %>%
  mutate(lat = as.numeric(lat, length = 6) + addLat,
         long = as.numeric(long, length = 6) + addLong)

## Bind back together
personLatLong <- bind_rows(needsFix, doesntNeedFix) %>%
  select(-n)

## Test that it worked
personLatLong %>%
  group_by(lat, long, surveyID) %>%
  filter(n() > 1) # good, zero rows.

## Bind back to r
r <- r %>%
  select(-c("lat", "long")) %>%
  left_join(personLatLong, by = c("responseID", "surveyID"))

# Change "NA" to <NA> -----------------------------------------------------
r <- r %>%
  mutate_if(is.character, .funs = function(x) x <- na_if(x, "NA")) %>%
  mutate_if(is.factor, .funs = function(x) x <- na_if(x, "NA"))

# Replace any smart quotes ------------------------------------------------
head(r)
r <- r %>%
  mutate(sentenceText = str_replace_all(sentenceText, "’", "'"))

# # Rename the surveys by date ----------------------------------------------
# surveyDates <- tbl(con, "surveys") %>% select(surveyID, dateReleased) %>% 
#   collect() %>% 
#   mutate(dateReleased = lubridate::date(dateReleased)) %>%
#   distinct()
# 
# r <- r %>%
#   left_join(surveyDates, by = "surveyID") %>%
#   mutate(surveyID = paste0(dateReleased, " (", surveyID, ")") %>% str_replace("S", "Survey ")) %>%
#   select(-dateReleased)

# Split into list by surveys ----------------------------------------------
surveysList <- r %>%
  group_by(surveyID) %>%
  named_group_split(surveyID)

# Split into sentences within each survey ---------------------------------
sentencesNestedList <- lapply(surveysList, function(x) x %>% 
                                group_by(sentenceText) %>% 
                                named_group_split(sentenceText))

snl <- sentencesNestedList %>% lapply(as.list) 

lapply(snl, length) %>% unlist() # looks reasonable (remember, we removed the control sentences)

# Add the "label" column for leaflet map popups --------------------------
test <- snl[[1]][[1]] %>% as.data.frame()
test <- test %>%
  mutate(label = paste0("<b>Gender: </b> ", gender, " <br> <b>Age group: </b> ", ageBin, " <br> <b>Edu. level: </b> ", education, " <br> <b>Race: </b> ", raceCats)) # seems to be working.

## function to apply to each list of the nested list snl
addLabel <- function(list){
  newList <- lapply(list, function(x){
    x %>%
      mutate(label = paste0("<b>Gender: </b> ", gender, " <br> <b>Age group: </b> ", ageBin, " <br> <b>Edu. level: </b> ", education, " <br> <b>Race: </b> ", raceCats, " <br> <b>Urban/rural: </b> ", urbanRural))
  })
  return(newList)
}

## apply the function
snl <- snl %>%
  lapply(., function(x) x %>% addLabel())

head(snl[[1]][[1]]) %>% as.data.frame() # looks good!

# Save the data -----------------------------------------------------------
save(snl, file = here("data", "points", "snl.Rda"))
names(snl)
