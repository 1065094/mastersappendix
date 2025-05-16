library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(patchwork)
#Loading in packages needed

#Loading in dataset, replace with your file
testingu251vu87 <- read_excel("C:/Users/EPSAy/Downloads/testingu251vu87.xlsx")
View(testingu251vu87)

#Making treatment a factor
testingu251vu87$Treatment <- factor(testingu251vu87$Treatment, levels = c("UT", "Etoposide"))

#Calculating what percentage of the cells are blue out of all cells counted per well and treatment
testingu251vu87 <- testingu251vu87 %>% mutate(Blue_Percent = (Blue_cells / Cell_count) * 100)

#Summarising the dataset by calculating the mean, median and standard deviation
testingu251vu87 %>%
  group_by(Cell_line, Treatment) %>%
  summarise(
    n = n(),
    mean_blue = mean(Blue_Percent, na.rm = TRUE),
    median_blue = median(Blue_Percent, na.rm = TRUE),
    sd_blue = sd(Blue_Percent, na.rm = TRUE),
    .groups = "drop"
  )


#Filtering per cell line to do testing and generate plots
u87_data <- filter(Rfriendlyallcellwblue, Cell_line == "U87")
u251_data <- filter(Rfriendlyallcellwblue, Cell_line == "U251")


# Run Wilcoxon rank test but using the exact version due to ties in the data and small sample sizes, provided you have bigger sample sizes, you can use regular Wilcoxon function
wilcox.exact(Blue_Percent ~ Treatment, data = u87_data)
wilcox.exact(Blue_Percent ~ Treatment, data = u251_data)

# Boxplot code for U87 cell line, using the output from the test above to signpoint significance
u87positivectrl <- ggplot(filter(testingu251vu87, Cell_line == "U87"),
       aes(x = Treatment, y = Blue_Percent, fill = Treatment)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 1, size = 2, aes(color = Treatment)) +
  geom_signif(comparisons = list(c("UT", "Etoposide")),
              annotations = "* (p = 0.02)",
              y_position = 85,
              tip_length = 0.01) +
  scale_color_manual(values = c("UT" = "#B4ECE7", "Etoposide" = "#5FB7DA")) +
  scale_y_continuous(limits = c(-1, 100), breaks = seq(0, 100, 25)) +
  theme_classic() +
  ylab("Blue Cells (%)") +
  theme(legend.position = "none") +
  ggtitle("U87")

#Boxplot code for U251
u251positivectrl <- ggplot(filter(testingu251vu87, Cell_line == "U251"),
       aes(x = Treatment, y = Blue_Percent, fill = Treatment)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 1, size = 2, aes(color = Treatment)) +
  geom_signif(comparisons = list(c("UT", "Etoposide")),
              annotations = "ns (p = 0.3)",
              y_position = 85,
              tip_length = 0.01) +
  scale_color_manual(values = c("UT" = "#B4ECE7", "Etoposide" = "#5FB7DA")) +
  scale_y_continuous(limits = c(-1, 100), breaks = seq(0, 100, 25)) +
  theme_classic() +
  ylab("Blue Cells (%)") +
  theme(legend.position = "none") + 
  ggtitle("U251")

# Putting both plots next to each other
u87positivectrl <- u87positivectrl + labs(title = "U87") + theme(plot.title = element_text(hjust = 0.5))
u251positivectrl <- u251positivectrl + labs(title = "U251") + theme(plot.title = element_text(hjust = 0.5))
u87positivectrl + u251positivectrl
