library(tidyverse)
library(broom)
library(Lahman)
data(Teams)


head(Teams)
## Generating a better offensive metric for baseball

#Utilizing regression with BB, singles, doubles, triples and homeruns for scoring potential

fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB/G,
         singles = (H - X2B - X3B - HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR = HR/G,
         R = R/G) %>%
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs

## Predicting the number of runs for each team in 2002
Teams02 <- Teams %>% 
  filter(yearID %in% 2002) %>%
  mutate(BB = BB/G,
         singles = (H - X2B - X3B - HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR = HR/G,
         R = R/G) %>%
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) +
  geom_point() +
  geom_text(nudge_x = 0.1, cex = 2) +
  geom_abline()
Teams02

## Average number of plate appearances per game
pa_per_game <- Batting %>% filter(yearID == 2002) %>%
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>%
  pull(pa_per_game) %>%
  mean
pa_per_game

## Average number of plate appearances per inning
pa_per_game/9

## Computing the per-plate-appearance rates for players available in 2002 using previous data
players_ppa <- Batting %>% filter(yearID %in% 1999:2001) %>%
  group_by(playerID) %>%
  mutate(PA = AB + BB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G,
            triples = sum(X3B)/G,
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .)) 

