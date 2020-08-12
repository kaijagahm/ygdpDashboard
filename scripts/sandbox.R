load("data/s11.Rda")
library(dplyr)
library(tidyr)

s1 <- unique(s11$SentenceText[25])
s2 <- unique(s11$SentenceText[31])

r1 <- 1:5
r2 <- 4:5

head(s11)

test <- s11 %>%
  filter(SentenceText %in% c(s1, s2))

meetscriteria <- test %>%
  group_by(ResponseID) %>%
  filter(Judgment[SentenceText == s1] %in% r1 & Judgment[SentenceText == s2] %in% r2) %>%
  select(ResponseID, Latitude, Longitude) %>%
  distinct() %>%
  mutate(criteria = T)

failscriteria <- s11 %>%
  filter(!(ResponseID %in% meetscriteria)) %>%
  select(ResponseID, Latitude, Longitude) %>%
  distinct() %>%
  mutate(criteria = F)

all <- bind_rows(meetscriteria, failscriteria) %>%
  arrange(ResponseID)

# Try wide instead
testwide <- s11 %>%
  filter(SentenceText %in% c(s1, s2)) %>%
  select(ResponseID, SentenceText, Latitude, Longitude, Judgment) %>%
  mutate(SentenceText = case_when(SentenceText == s1 ~ "s1",
                                  SentenceText == s2 ~ "s2")) %>%
  pivot_wider(., id_cols = c(ResponseID, Latitude, Longitude),
              names_from = SentenceText,
              values_from = Judgment) %>%
  mutate(criteria = case_when(s1 %in% r1 & s2 %in% r2 ~ T,
                              TRUE ~ F))
