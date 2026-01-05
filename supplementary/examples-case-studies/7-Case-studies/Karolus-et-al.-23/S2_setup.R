#####################!!!!!!!!!!!!!!!!!!!!----------------- SETUP ----------------------
library(tidyverse)
df <- read.csv('data_cleaned_s2.csv')
df$condition <- as.factor(df$condition)
df$p_id <- as.factor(df$p_id)

df$tct <- df$tct/1000.0

#rename questions columns
df <- df %>% 
  rename(
    cQ1 = Q73_32,
    cQ2 = Q73_1,
    cQ3 = Q73_39,
    cQ4 = Q73_37,
    cQ5 = Q73_34,
    cQ6 = Q73_40,
    cQ7 = Q73_42
  )

#coarse conditions
df$coarse_condition <- ifelse(df$condition==1 | df$condition==5 | df$condition==6, "R", "C")
df$game_condition <- ifelse(df$condition==1 | df$condition==2, "A_None", ifelse(df$condition==3 | df$condition==5, "Emoji", "Slider"))

df$coarse_condition <- as.factor(df$coarse_condition)
df$game_condition <- as.factor(df$game_condition)