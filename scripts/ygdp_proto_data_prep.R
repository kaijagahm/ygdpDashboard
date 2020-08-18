# Data prep etc. for ygdp proto Shiny app
library(here)
library(forcats)
library(dplyr)
load("data/ygdp_data.Rda")

# Let's look just at survey 11 for now
s11 <- ygdp_data %>%
  filter(Survey == "11")

# Select a few columns to play around with
s11 <- s11 %>%
  select(Survey, Age, AgeBin, Gender, Education, Income, Race, RaisedCity, RaisedState, Latitude, Longitude, RaisedYears, ResponseID, RegANAE, CarverReg, CarverSubReg, CarverSubSubReg, CarverRegCollapsed, POP2010, POP10_SQMI, UATYP10, Sentence, Judgment, Construction, SentenceText)

# Remove spaces from the beginning and end of the sentences
s11 <- s11 %>%
  mutate(SentenceText = str_replace(SentenceText, "^\\s|\\s$", ""))

# Fix factor names
s11 <- s11 %>%
  mutate(AgeBin = fct_recode(AgeBin, 
                             "61+" = "61-70",
                             "61+" = "71-80",
                             "61+" = "81-90",
                             "61+" = "91-100"),
         Gender = fct_recode(Gender,
                             "Male" = "Transgender (FTM)",
                             "Female" = "Transgender (MTF)",
                             "Other" = "Not sure"),
         Education = fct_recode(Education,
                                "High school" = "Some high school, no diploma",
                                "High school" = "High school diploma",
                                "Some college" = "Some college, no degree"),
         Income = fct_recode(Income,
                             "[0-12.5)" = "$0-$12,499",
                             "[12.5-25)" = "$12,500-$24,999",
                             "[25-37.5)" = "$25,000-$37,499",
                             "[37.5-50)" = "$37,500-$49,999",
                             "[50-62.5)" = "$50,000-$62,499",
                             "[62.5-75)" = "$62,500-$74,999",
                             "[75-87.5)" = "$75,000-$87,499",
                             "[87.5-100)" = "$87,500-$99,999",
                             "100+" = "$100,000+"),
         Race = fct_recode(Race,
                           "Native American" = "American Indian/Native American",
                           "Multiracial/Other" = "American Indian/Native American,White/Caucasian,Other ",
                           "Black" = "Black/African American",
                           "Hispanic/Latino" = "Hispanic/Latino/Latina",
                           "Multiracial/Other" = "Hispanic/Latino/Latina,White/Caucasian",
                           "White" = "White/Caucasian"),
         RegANAE = fct_recode(RegANAE,
                              "E. New England" = "Eastern New England",
                              "St. Louis Corr." = "St. Louis Corridor",
                              "W. New England" = "Western New England",
                              "W. Pennsylvania" = "Western Pennsylvania"),
         RegANAE = na_if(RegANAE, ""),
         CarverSubReg = fct_recode(CarverSubReg,
                                   "E. New England" = "Eastern New England",
                                   "W. New England" = "Western New England"),
         CarverSubSubReg = fct_recode(CarverSubSubReg,
                                      "E. New England" = "Eastern New England",
                                      "W. New England" = "Western New England"),
         CarverRegCollapsed = fct_recode(CarverRegCollapsed,
                                         "E. New England" = "Eastern New England",
                                         "W. New England" = "Western New England"))

# Change types and add label column for maps
s11 <- s11 %>%
  mutate(SentenceText = as.character(SentenceText),
         Construction = as.character(Construction)) %>%
  mutate(label = paste("<b>Gender: </b>", Gender, "<br>", "<b>Age group: </b>", AgeBin, "<br>", "<b>Edu. level: </b>", Education, "<br>", "<b>Race: </b>", Race, "<br>", "<b>Location: </b>", paste0(RaisedCity, ", ", RaisedState)))

# If points overlap, jitter them
## Get only unique pairs of lat/long so we don't apply a different jitter to each person*rating
temp <- s11 %>%
  select(ResponseID, Latitude, Longitude) %>% 
  distinct()

dup <- duplicated(temp[,2:3]) # get only the second one of a duplicate pair--we want to change as little data as possible.

temp <- temp %>%
  mutate(duplicate = dup) # add a column to indicate whether each row is a duplicate

temp <- temp %>%
  mutate(Latitude = case_when(duplicate == T ~ jitter(Latitude, amount = 0.001),
                              TRUE ~ Latitude),
         Longitude = case_when(duplicate == T ~ jitter(Longitude, amount = 0.001),
                               TRUE ~ Longitude))

# Join the new Lat/Long to s11
s11 <- s11 %>%
  select(-c("Latitude", "Longitude")) %>%
  left_join(temp, by = "ResponseID")

# Write out the data
save(s11, file = here("data", "s11.Rda"))

