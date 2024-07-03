library(tidyverse)
library(dplyr)
library(stats)
library(tidyr)

#separating nationality of the player into two columns to short form and long form

separate_data <- filterd_data %>% separate(Nation, into = c("Prefix", "Nationality"), sep = " ")
separate_data

#now removing prefix column as it is unnecessaray
final_data <- separate_data %>% select(Player, Nationality, Squad, Pos, Sh, SoT, Dist, FK, PKatt, PK, Gls)

final_data


#K means:
install.packages("factoextra")
library(ggplot2)
library(factoextra)
install.packages("ggfortify")
library(ggfortify)


noNa <- na.omit(new_dataset)
noNa

noNa <- noNa %>% select(-PK, - PKatt)

noNa <- noNa %>% filter(Pos == "MF" | Pos == "FW")
noNa
position <- noNa$Pos
position

table(position)

#extracting the numeric columns
k_data <- noNa[5:9]
k_data



#filtering a sample


#Scaled data 
k_data_scaled <- scale(k_data)
k_data_scaled

#distance 
k_data <- dist(k_data_scaled)
k_data


#checking optimal no of clusters
fviz_nbclust(k_data_scaled, kmeans, method = "wss") + 
  labs(subtitle = "Elbow Graph")


#choosing 3 clusters

k_out <- kmeans(k_data_scaled, centers = 3, nstart = 10)
k_out
#between_SS / total_SS = 61.9% which means that 61.9% of data is similar to other values in the cluster

k_cluster <- k_out$cluster

fviz_cluster(list(data = k_data_scaled, cluster = k_cluster))
