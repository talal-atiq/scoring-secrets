library(ggplot2)
library(dplyr)
library(tidyr)

print(final_data)

final_data$Player <- iconv(final_data$Player, from = "UTF-8", to = "UTF-8")

##Goals-Shots relationship
ggplot(final_data, aes(x = Sh, y = Gls)) + 
  geom_point(color = "darkred", size = 3) +
  geom_smooth(method = "lm", color = "green", size = 1.5) +
  labs(x = "Shots Taken", y = "Goals Scored", title = "Goals-Shot relationship") +
  theme(panel.background = element_rect(fill = "yellow"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"))

##Goals-Shots on target relationship
ggplot(final_data, aes(x = SoT, y = Gls)) + 
  geom_point(color = "yellow", size = 3) +
  geom_smooth(method = "lm", color = "green", size = 1.5) +
  labs(x = "Shots on Target", y = "Goals Scored", title = "Goals-Shot on Target relationship") +
  theme(panel.background = element_rect(fill = "purple"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"))

##Goals-Distance relationship
ggplot(final_data, aes(x = Dist, y = Gls)) + 
  geom_point(color = "white", size = 3) +
  geom_smooth(method = "lm", color = "cyan", size = 1.5) +
  labs(x = "Distance From", y = "Goals Scored", title = "Goals-Distance From Goal relationship") +
  theme(panel.background = element_rect(fill = "brown"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"))

##Goals-Freekick Relationship
ggplot(final_data, aes(x = FK, y = Gls)) + 
  geom_point(color = "red", size = 3) +
  geom_smooth(method = "lm", color = "yellow", size = 1.5) +
  labs(x = "Freekicks", y = "Goals Scored", title = "Goals-Freekicks relationship") +
  theme(panel.background = element_rect(fill = "darkgreen"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"))

##Goals-Penalties Taken
ggplot(final_data, aes(x = PKatt, y = Gls)) + 
  geom_point(color = "yellow", size = 3) +
  geom_smooth(method = "lm", color = "navy", size = 1.5) +
  labs(x = "Penalties Taken", y = "Goals Scored", title = "Goals-Penalty Kicks Taken relationship") +
  theme(panel.background = element_rect(fill = "darkslategray"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"))
