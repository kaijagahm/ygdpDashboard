load("data/s11.Rda")

ps <- unique(s11$ResponseID)
s1 <- unique(s11$SentenceText[1])
s2 <- unique(s11$SentenceText[2])

s1d <- s11 %>%
  filter(SentenceText == s1, 
         ResponseID %in% s11 %>% filter(SentenceText == s2, Judgment > 4) %>% pull(responseID))

s2d <- s11 %>%
  filter(SentenceText == s2)

s2pass <- s2d %>%
  filter(Judgment > 4) %>%
  pull(ResponseID)

s1d <- s1d %>%
  filter(ResponseID %in% s2pass)
