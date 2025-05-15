library(readxl)
library(dplyr)
library(ggplot2)
library(lme4)
library(emmeans)
library(ggpubr)
library(ggsignif)
# Loading in the packages needed

# Loading in the data - change file path as appropriate
returntonorm <- read_excel("E:/returntonormoxia/returntonormallthreesummary.xlsx")
View(returntonorm)

# Making sure the treatments are in the correct order to present and making them into factors
returntonorm$Treatment <- factor(returntonorm$Treatment, levels = c("UT", "H", "IR", "B", "E"),
                                      labels = c("Con", "Hyp", "IR", "Hyp + IR", "Etop"))
returntonorm$Block <- factor(returntonorm$Block, levels = c("1", "2", "3"))

# Summarising the data to calculate the mean 
returntonorm_avg <- returntonorm %>%
  group_by(Treatment, Block, Well) %>%
  summarise(
    MeanPercent = mean(Percent, na.rm = TRUE),
    .groups = "drop"
  )
View(returntonorm_avg)

# Calculating the proportion of blue cells - since given the rationale followed from figures 3.4 & 3.5, a GLMM will be applied again
returntonorm_avg <- returntonorm %>%
  mutate(PropBlue = Blue / All_cells) %>%
  group_by(Treatment, Block, Well) %>%
  summarise(
    PropBlue = mean(PropBlue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(MeanPercent = PropBlue * 100)
View(returntonorm_avg)

# Testing the appropriateness of the GLMM model
testDispersion(glmm_modelreturntonorm)
simulationOutput <- simulateResiduals(fittedModel = glmm_modelreturntonorm, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)

# GLMM testing
glmm_modelreturntonorm <- glmer(
  cbind(Blue, All_cells - Blue) ~ Treatment + (1 | Block) + (1 | Well),
  data = returntonorm,
  family = binomial(link= "logit")
)
glmm_modelreturntonorm

confint(glmm_modelreturntonorm, method = "Wald")

Anova(glmm_modelreturntonorm)

# Calculating estimated marginal means
emmreturntonorm <- emmeans(glmm_modelreturntonorm, ~ Treatment)
emmreturntonorm

# Doing pairwse comparisons
pairs(emmreturntonorm)
# Summarising to get p-values
pairs_emmreturntonorm <- pairs(emmreturntonorm, adjust = "tukey") %>%
  summary(infer = TRUE)

# Extracting the relevant pairwise comparisons to assess significance
sig_contrasts <- pairs_emmreturntonorm %>%
  filter(
    (contrast == "Hyp - Con") |
      (contrast == "Hyp + IR - Hyp") |
      (contrast == "Hyp + IR - IR")
  )


# Creating a data frame to allow the plotting of the EMM
emm_probsreturntonorm <- as.data.frame(summary(emmreturntonorm, type = "response"))
View(emm_probsreturntonorm)

# Code to plot graph with raw data points and EMM means with the confidence intervals
ggplot(data = returntonorm_avg, aes(x = Treatment, y = MeanPercent)) +
  geom_jitter(aes(color = Treatment, shape = Block),
              width = 0.2, size = 4, alpha = 0.6) +  # raw dots
  geom_point(data = emm_probsreturntonorm, 
             aes(x = Treatment, y = prob * 100), 
             inherit.aes = FALSE,
             size = 4, shape = 19, color = "black", stroke = 1.2) +  # model mean
  geom_errorbar(data = emm_probsreturntonorm,
                inherit.aes = FALSE,
                aes(x = Treatment, ymin = asymp.LCL * 100, ymax = asymp.UCL * 100),
                width = 0.1, color = "black") +  # CI bars
  scale_color_manual(values = c("Con" = "#B4ECE7", "Etop" = "#42BFF0", "IR" = "#5FB7DA", "Hyp" = "#B0D5E4", "Hyp + IR" = "#97DDFA")) +
  scale_shape_manual(values = c(19, 21, 15)) +
  ylab("Percent Blue Cells") +
  xlab(NULL) +
  scale_y_continuous(limits = c(-1, 120), breaks = seq(0, 100, 25)) +
  theme_classic() +
  ggtitle(NULL) +
  geom_signif(
    comparisons = list(
      c("Hyp", "Con"),
      c("Hyp + IR", "Hyp"),
      c("Hyp + IR", "IR")
    ),
    map_signif_level = function(p) {
      ifelse(p < 0.001, "***",
             ifelse(p < 0.01, "**",
                    ifelse(p < 0.05, "*", "ns")))
    },
    y_position = c(45, 100, 85),
    tip_length = 0.01,
    textsize = 4
  )

# Due to this data set having a lot of variation, a GLMM was not entirely appropriate, therefore a Wilcoxon test with a Bonferroni correction had to be calculated to verify significance but also to check if p-values needed to be adjusted
pairwise.wilcox.test(returntonorm$Percent, returntonorm$Treatment, p.adjust.method = "bonferroni")

compare_means(Percent ~ Treatment, data = returntonorm, method = "wilcox.test", p.adjust.method = "bonferroni")

