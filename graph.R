library(ggplot2)
library(dplyr)
library(readxl)
# Loading in packages needed 

# Loading in data - change file path as appropriate
w145measurements <- read_excel("E:/O2 probe measurements/w145measurements.xlsx")
View(w145measurements)

# Renaming conditions, making them into factors and making sure they are in the right order
w145measurements$Condition <- factor(w145measurements$Condition, levels = c("Hypoxia", "CombopreIR", "CombopostIR", "Combo24hr", "Norm16hr", "Norm24hr"),
                                     labels = c("Hyp", "Hyp + IR pre-IR", "Hyp + IR post-IR", "Hyp + IR 24 hr", "Norm 16 hr", "Norm 24 hr"))
w145measurementswmean$Condition <- factor(w145measurementswmean$Condition, levels = c("Hypoxia", "CombopreIR", "CombopostIR", "Combo24hr", "Norm16hr", "Norm24hr"),
                                          labels = c("Hyp", "Hyp + IR pre-IR", "Hyp + IR post-IR", "Hyp + IR 24 hr", "Norm 16 hr", "Norm 24 hr"))


# Calculate means and standard errors per condition
w145measurementswmean <- w145measurements %>%
  group_by(Condition) %>%
  summarise(
    mean_oxygen = mean(Oxygen_conc),
    se_oxygen = sd(Oxygen_conc) / sqrt(n())
  )
View(w145measurementswmean)

# Plot per condition with raw data points and variation to clearly see the differences in oxygen concentration, no need to test for significance, just data display
ggplot(w145measurements, aes(x = Condition, y = Oxygen_conc)) +
  geom_bar(data = w145measurementswmean, aes(y = mean_oxygen), stat = "identity", fill = "lightblue", width = 0.6) +
  geom_errorbar(data = w145measurementswmean, aes(y = mean_oxygen, ymin = mean_oxygen - se_oxygen, ymax = mean_oxygen + se_oxygen), 
                width = 0.2, color = "black") +
  geom_point(size = 2, color = "black") +
  theme_classic() +
  labs(y = "Oxygen Concentration", x = "Condition")
