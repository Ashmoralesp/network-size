#load packages
library(rio)
library(tidyverse)

#import data
data <-
  import ("C:/Users/ashle/OneDrive/Desktop/anes2008_2009panel_dataset.dta")

#clean dataset
tidy_data <-
  select (data,caseid,w11zd21,w11zd22,w11zd23,w11zd24,w11zd25,
    w11zd26,w11zd27,w11zd28,w11zd29,der01,w9age,der03a,der05,
    der06,der08w9,w9h1,w9j1,w9zd4_1,w9zd4_2,w9zd4_3,w9zd1,
    w9zd23_1,w9zd23_2,w9zd23_3,w9zd12_1,w9zd13_1,w9zd14_1,
    w9zd15_1,w9zd16_1,w9zd12_2,w9zd13_2,w9zd14_2,w9zd15_2,
    w9zd16_2,w9zd12_3,w9zd13_3,w9zd14_3,w9zd15_3,w9zd16_3,
    der08w9,wgtl11,w9zd9_1,w9zd9_2,w9zd9_3) %>%
  mutate_at(
    c("w11zd21","w11zd22","w11zd23","w11zd24","w11zd25",
      "w11zd26","w11zd27","w11zd28","w11zd29"),
    funs(recode(.,
      `-7` = ".",
      `-6` = ".",
      `-5` = ".",
      `1` = "1",
      `2` = "0"))) %>%
  mutate_if(is.character, as.numeric)


#create base participatory index
tidy_data$part_index_base = tidy_data$w11zd21 + tidy_data$w11zd22 +
  tidy_data$w11zd23 +  tidy_data$w11zd24 + tidy_data$w11zd25 + 
  tidy_data$w11zd26 + tidy_data$w11zd27 +tidy_data$w11zd28 + 
  tidy_data$w11zd29

#create main participatory index
tidy_data$part_index_main = tidy_data$w11zd21 + tidy_data$w11zd26 + 
  tidy_data$w11zd27 + tidy_data$w11zd28 + tidy_data$w11zd29

#Recode Network Size Variables
ns_df <- tidy_data %>%
  mutate_at(c("w9zd4_1", "w9zd4_2", "w9zd4_3"), funs(recode(.,
    `1` = "1",
    `2` = "1",
    `3` = "1",
    `4` = "1",
    `5` = "1"))) %>%
  mutate_at(c("w9zd1"), funs(recode(., `1` = "1", `2` = "1"))) %>%
  mutate_if(is.character, as.numeric)

#Creating Total Network Size Variable
val_ns_df <- ns_df %>%
  mutate_at(c("w9zd1"), funs(recode(., `1` = "0"))) %>%
  mutate (Total = select(., w9zd4_1:w9zd4_3) %>%
            rowSums(na.rm = TRUE)) %>%
  mutate_if(is.character, as.numeric)

mean(val_ns_df$Total, na.rm = TRUE)

describe(val_ns_df$Total)

ggplot(data = val_ns_df, aes(x = Total)) +
  geom_density()
