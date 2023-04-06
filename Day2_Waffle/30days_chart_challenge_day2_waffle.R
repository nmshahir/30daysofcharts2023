library(tidyverse)
library(readxl)
library(ggplot2)
library(ggtext)
library(waffle)

#Import package
Rainbow_Tomatoes_Garden_Fish_List <- read_excel("C:/Users/626568/Downloads/Rainbow Tomatoes Garden Fish List.xlsx")

# There are 63 "distinct" varieties of tinned fish but upon closer look some of them are "Tuna, Albacore" or "Tuna, Skipjack"
# which are both in the broader super category of "Tuna". SO TIME FOR SOME DATA WRANGLING/MUNGING

#I need you all to know that as a Marylander it deeply deeply pained me to not give "Crab" it's own category but instead
#fold it into "Shellfish" T___T
tinned_fish_collate <- Rainbow_Tomatoes_Garden_Fish_List %>%
  #mutate(fish_type_collate = if(str_contains(`Type of Fish`, "tuna", ignore.case = T),"Tuna",`Type of Fish`?))
  mutate(fish_type_collate = case_when(
    grepl("tuna",`Type of Fish`, ignore.case = T) ~ "Tuna",
    grepl("Lobster, Pollack, Scallop",`Type of Fish`, ignore.case = T) ~ "Mixed",
    grepl("Sardines, Mussels, Squid",`Type of Fish`, ignore.case = T) ~ "Mixed",
    grepl("Great silver smelt, haddock, saithe, Alaska pollack",`Type of Fish`, ignore.case = T) ~ "Mixed",
    grepl("Haddock, pollack, great silver smelt, pacific whiting, saithe",`Type of Fish`, ignore.case = T) ~ "Mixed",
    grepl("clam",`Type of Fish`, ignore.case = T) ~ "Shellfish",
    grepl("Salmon",`Type of Fish`, ignore.case = T) ~ "Salmon",
    grepl("Roe",`Type of Fish`, ignore.case = T) ~ "Roe",
    grepl("bass",`Type of Fish`, ignore.case=T) ~ "Sea Bass",
    grepl("Mussels", `Type of Fish`, ignore.case = T) ~ "Shellfish",
    grepl("Cockles", `Type of Fish`, ignore.case = T) ~ "Shellfish",
    grepl("Oyster", `Type of Fish`, ignore.case = T) ~ "Shellfish",
    grepl("Scallop", `Type of Fish`, ignore.case = T) ~ "Shellfish",
    grepl("Mackerel", `Type of Fish`, ignore.case = T) ~ "Mackerel",
    grepl("Sardine", `Type of Fish`, ignore.case = F) ~ "Sardines",
    grepl("crab", `Type of Fish`, ignore.case = T) ~ "Shellfish",
    grepl("lobster", `Type of Fish`, ignore.case = T) ~ "Shellfish",
    grepl("Squid", `Type of Fish`, ignore.case = T) ~ "Mollusks",
    grepl("Octopus", `Type of Fish`, ignore.case = T) ~ "Mollusks",
    grepl("Squid", `Type of Fish`, ignore.case = T) ~ "Mollusks",
    grepl("Cuttlefish", `Type of Fish`, ignore.case = T) ~ "Mollusks",
    grepl("Barnacles", `Type of Fish`, ignore.case = T) ~ "Shellfish",
    grepl("Anhovy", `Type of Fish`, ignore.case = T) ~ "Anchovy",
    grepl("Anchov", `Type of Fish`, ignore.case = T) ~ "Anchovy",
    TRUE ~ `Type of Fish`
  ) ) %>%
  group_by(fish_type_collate) %>%
  count() %>%
  arrange(desc(n))

#Okay, we've managed to get this down to 27 categories-- which is great! Except a plot with 27 colors is just not good
# data visualization practice. Thus, anything that has under 15 brands is going under "Other"

waffle_fish <- tinned_fish_collate %>%
  mutate(fish_category = case_when(
    n < 15 ~ "Other",
    TRUE ~ fish_type_collate
  )) %>%
  group_by(fish_category) %>%
  summarise(total = sum(n)) %>%
  arrange(desc(total))

#OKAY WE HAVE OUR DATA,but uh 658 brands is A Lot to Visualize. So we're going to SCALE THIS INSTEAD. 
#Note: since 658 is not a Nice Number and I have yet to hack partial squares for waffle charts in R. We're going to take 
#the floor of the scaled total 

scaled_waffle <- waffle_fish%>%
  mutate(scaled_total = floor((total/sum(total))*100))

#Now It's PLOTTING TIME
ggplot(scaled_waffle, aes(fill = fish_category, values = scaled_total)) +
  geom_waffle(colour = "white") +
  scale_fill_manual(name = "Type of Tinned Fish",
                    values = c("#c0003d","#01c085","#e3342e","#34c5ff","#d4ca41","#2f5191","#448200","#dabaf4","#00abb4","#803b62"),
                    labels = c("Anchovy","Herring","Mackerel","Mollusks","Other","Salmon", "Sardines","Shellfish","Sparts", "Tuna")) +
  coord_equal() +
  theme_void() +
  labs(title = "30 Days of Chart Challenge \n Day 2 - Waffle \n Variety of Tinned Fish Sold by Rainbow Tomatoes Garden LLC",
       subtitle = "1 square ~ 6.58 brands of tinned fish",
       caption = "Data source: Rainbow Tomatoes Garden LLC") +
  theme(
    plot.title =  element_text(hjust= 0, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_text(hjust = 1, face="italic")
  )
