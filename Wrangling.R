

library(dplyr)


#Selecting only the required columns to work on

new_dataset <- football.shooting.stats %>% select(Player, Nation, Squad, Pos, Sh, SoT, Dist, FK, PK, PKatt, Gls)
new_dataset
##rename columns

#Now filtering according to position as we only need to analyze shooting of offensive players
filterd_data <- new_dataset  %>% filter(Pos == "MF" | Pos == "FW" | Pos == "FW,MF")
filterd_data
dim(filterd_data)



