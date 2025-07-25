library(readxl)
library(emmeans)
library(dplyr)
library(lme4)
library(lmerTest)
library(patchwork)
library(ggplot2)
#Loading in packages needed: importing data, for GLMM testing and to filter through data

#Loading in data, replace with your file pathway
supersummary2 <- read_excel("E:/Cell Diameter Results/supersummary2.xlsx")

# Convert Day to factor 
# Ensure factors are correctly ordered and rename with appropriate units
supersummary2$Cell_line <- factor(supersummary2$Cell_line, levels = c("U87", "U251"))
supersummary2$Condition <- factor(supersummary2$Condition, levels = c("UT", "1UM", "10UM", "6GY", "8GY"))
supersummary2 <- supersummary2 %>%
  mutate(Condition = case_when(
    Condition == "1UM" ~ "1µM",
    Condition == "10UM" ~ "10µM",
    TRUE ~ Condition 
  ))

# Conduct an ANOVA to understand the effects of cell line and condition on cell diameter and if there are any interactions of interest
celldiametermodel <- aov(Cell_diameter ~ Cell_line * Condition, data = supersummary2)
summary(celldiametermodel)


# This section will give both a high-level analysis of the effect of day, cell line, condition and their interactions
# A mixed model is also fit to see specifics, such as each condition per cell line and day for full details
diametermodel <- lmer(Cell_diameter ~ Cell_line * Condition * Day + (1|Day), data = supersummary2)
summary(diametermodel)
anova(diametermodel)

# Get pairwise comparisons: Condition within each Day for further testing
emmdiameter <- emmeans(diametermodel, pairwise ~ Condition | Day, adjust = "tukey")
emmdiameter$contrasts  # This will show pairwise p-values
emmdiameter <- emmeans(diametermodel, pairwise ~ Condition | Cell_line * Day, adjust = "tukey")
emmdiameter$contrasts



#Assigning colours per condition
bw_palette <- c(
  "UT"   = "#CCCCCC",  
  "1µM"  = "#000000",
  "10µM" = "#333333", 
  "6GY"  = "#666666", 
  "8GY"  = "#999999"   
)


u87_plotup <- supersummary2 %>%
  filter(Cell_line == "U87", as.numeric(as.character(Day)) >= 6) %>%
  ggplot(aes(x = as.factor(Day), y = Cell_diameter, fill = Condition)) +
  geom_violin(position = position_dodge(width = 0.8), alpha = 0.5, width = 0.9, trim = FALSE) +
  geom_boxplot(
    position = position_dodge(width = 0.8),
    width = 0.2,
    outlier.shape = 16,
    outlier.size = 1.5,
    outlier.alpha = 0.6
  ) +
  stat_summary(
    aes(group = Condition),
    fun = mean,
    geom = "line",
    linewidth = 1.2, 
    color = "black"
  ) +
  stat_summary(
    aes(group = Condition),
    fun = mean,
    geom = "point",
    size = 2,
    color = "black"
  ) +
  facet_wrap(~Condition, nrow = 1) +
  scale_fill_manual(values = bw_palette) +
  scale_color_manual(values = bw_palette) +
  labs(
    title = "U87 Cell Diameter Over Time",
    x = "Day",
    y = "Cell Diameter (µm)"
  ) +
  theme_classic()
u87_plotup

u251_plotup <- supersummary2 %>%
  filter(Cell_line == "U251", as.numeric(as.character(Day)) >= 6) %>%
  ggplot(aes(x = as.factor(Day), y = Cell_diameter, fill = Condition)) +
  geom_violin(position = position_dodge(width = 0.8), alpha = 0.5, width = 0.9, trim = FALSE) +
  geom_boxplot(
    position = position_dodge(width = 0.8),
    width = 0.2,
    outlier.shape = 16,
    outlier.size = 1.5,
    outlier.alpha = 0.6
  ) +
  stat_summary(
    aes( group = Condition),
    fun = mean,
    geom = "line",
    linewidth = 1.2,
    color = "black"
  ) +
  stat_summary(
    aes(group = Condition),
    fun = mean,
    geom = "point",
    size = 2,
    color = "black"
  ) +
  facet_wrap(~Condition, nrow = 1) +
  scale_fill_manual(values = bw_palette) +
  scale_color_manual(values = bw_palette) +
  labs(
    title = "U87 Cell Diameter Over Time",
    x = "Day",
    y = "Cell Diameter (µm)"
  ) +
  theme_classic()
u251_plotup

print(u87_plotup)
print(u251_plotup)
combined_plotup <- u87_plotup / u251_plotup + 
  plot_layout(heights = c(1, 1))  
combined_plotup
