#load packages
library(rio)
library(tidyverse)

#import data
data <- import ("data/original-data/anes2008_2009panel_dataset.dta")

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

#create main participatory index (not for NS analysis)
tidy_data$part_index_main = tidy_data$w11zd21 + tidy_data$w11zd26 + 
  tidy_data$w11zd27 + tidy_data$w11zd28 + tidy_data$w11zd29

#Recode Network Size Variables (initial recode)
ns_df <- tidy_data %>%
  mutate(w9zd4_1=recode(w9zd4_1, 
                  `1` = "1",
                  `2` = "1",
                  `3` = "1",
                  `4` = "1",
                  `5` = "1"))%>%
  mutate(w9zd4_2=recode(w9zd4_2, 
                  `1` = "1",
                  `2` = "1",
                  `3` = "1",
                  `4` = "1",
                  `5` = "1"))%>%
  mutate(w9zd4_3=recode(w9zd4_3, 
                  `1` = "1",
                  `2` = "1",
                  `3` = "1",
                  `4` = "1",
                  `5` = "1"))%>%
  mutate(w9zd1 = recode(w9zd1,
         `1` = "1",
         `2` = "1"))%>%
    mutate_if(is.character, as.numeric)

#Creating Total Network Size Variable - Initial Attempt Index (Not used in subsequent analysis for now)
val_ns_df <- ns_df %>%
  mutate_at(c("w9zd1"), funs(recode(., `1` = "0"))) %>%
  mutate (Total = select(., w9zd4_1:w9zd4_3) %>%
            rowSums(na.rm = TRUE)) %>%
  mutate_if(is.character, as.numeric)

mean(val_ns_df$Total, na.rm = TRUE)

describe(val_ns_df$Total)

ggplot(data = val_ns_df, aes(x = Total)) +
  geom_density()


#Isolating NS - as discussed last session

#Frequencies of original data
table(data$w9zd1)
table(data$w9zd4_1)
table(data$w9zd4_2)
table(data$w9zd4_3)

#Summary of NAs after selection of variables
summary (ns_df$w9zd4_1) #2115 NAs - these are the same for val_ns_df
summary (ns_df$w9zd4_2) #2218 NAs
summary (ns_df$w9zd4_3) #2345 NAs
summary (ns_df$w9zd1) #1584 NAs - different than w9zd4_1 - 531 received question but did not answer closeness at all

#Table of clean_ns_df
clean_ns_df <- ns_df%>%
  select (w9zd1, w9zd4_1,w9zd4_2,w9zd4_3) %>%
  rename ("rec_q" = "w9zd1")%>%
  rename ("id_1" = "w9zd4_1")%>%
  rename ("id_2" = "w9zd4_2")%>%
  rename ("id_3" = "w9zd4_3")%>%
  mutate_if(is.character, as.numeric)
as_tibble(clean_ns_df)
summary(clean_ns_df) 

table(clean_ns_df$`rec_q`)
table(clean_ns_df$`id_1`)
table(clean_ns_df$`id_2`)
table(clean_ns_df$`id_3`)
        
comp_tab <- apply(clean_ns_df,2, table)
comp_tab  


#Analysis with Political Participation - NAs as zeroes

#Main Political Participation Index in clean_ns_Df
clean_ns_df$part_index_main = tidy_data$w11zd21 + tidy_data$w11zd26 + 
  tidy_data$w11zd27 + tidy_data$w11zd28 + tidy_data$w11zd29

#Turn Drop NAs from people without question and turn rest NAs into 0
na_zero_df <- clean_ns_df

na_zero_df<- na_zero_df[!is.na(na_zero_df$rec_q),]
view(na_zero_df)

na_zero_df[is.na(na_zero_df)] <- 0
View(na_zero_df)

#Initial Basic Regressions with Political Participation Index and 4 vars separate
clean_fit <- lm(part_index_main ~ rec_q + id_1 + id_2 + id_3, data=na_zero_df)
summary(clean_fit)

#NS index
na_zero_df$ns_index = na_zero_df$rec_q + na_zero_df$id_1 + na_zero_df$id_2 + na_zero_df$id_3

#Index Regression
index_fit <- lm(part_index_main ~ ns_index, data=na_zero_df)
summary(index_fit)

#Plot of Index Regression
plot (part_index_main ~ ns_index, data=na_zero_df)
abline(index_fit)


#Analysis with Political Participation - NAs dropped and Zero Size Dropped

#Drop cases where no question AND no Network Size
na_drop_df <- clean_ns_df
na_drop_df<-na_drop_df[!is.na(na_drop_df$rec_q),]
na_drop_df <- na_drop_df[!is.na(na_drop_df$id_1),]

#Change the rest of NAs to 0 for the Network Sizes Beyond Zero
na_drop_df[is.na(na_drop_df)] <- 0
view(na_drop_df)

#Initial Basic Regressions with Political Participation Index and 4 vars separate
clean_fit <- lm(part_index_main ~ rec_q + id_1 + id_2 + id_3, data=na_drop_df)
summary(clean_fit)

#NS index
na_drop_df$ns_index = na_drop_df$rec_q + na_drop_df$id_1 + na_drop_df$id_2 + na_drop_df$id_3

#Index Regression
index_fit <- lm(part_index_main ~ ns_index, data=na_drop_df)
summary(index_fit)

#Plot of Index Regression
plot (part_index_main ~ ns_index, data=na_drop_df)
abline(index_fit)

