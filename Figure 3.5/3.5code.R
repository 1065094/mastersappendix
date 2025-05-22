library(readxl)
library(dplyr)
library(ggplot2)
library(lme4)
library(emmeans)
library(car)
library(ggpubr)
library(ggsignif)
library(DHARMa)
# Loading appropriate packages

# Loading in the data 

alldata164summary <- read_excel("E:/164FIVECONDITONS/alldata164summary.xlsx")
View(alldata164summary)

# Making sure the treatments are in the correct order to present and made into a factor
alldata164summary$Treatment <- factor(alldata164summary$Treatment, levels = c("UT", "H", "IR", "B", "E"),
                               labels = c("Con", "Hyp", "IR", "Hyp + IR", "Etop"))
alldata164summary$Block <- factor(alldata164summary$Block, levels = c("1", "2", "3"))

# Summarising the data to calculate the mean percentage of blue cells per treatment and well 
alldata164summary_avg <- alldata164summary %>%
  group_by(Treatment, Block, Well) %>%
  summarise(
    MeanPercent = mean(Percent, na.rm = TRUE),
    .groups = "drop"
  )
View(alldata164summary_avg)

# Calculating proportions to appropriately use a GLMM
alldata164summary_avg <- alldata164summary %>%
  mutate(PropBlue = Blue / All_cells) %>%
  group_by(Treatment, Block, Well) %>%
  summarise(
    PropBlue = mean(PropBlue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(MeanPercent = PropBlue * 100)
View(alldata164summary_avg)

# Checking if GLMM is appropriate using DHARMa
testDispersion(glmm_model164)
simulationOutput <- simulateResiduals(fittedModel = glmm_model164, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)

# GLMM testing
glmm_model164 <- glmer(
  cbind(Blue, All_cells - Blue) ~ Treatment + (1 | Block) + (1 | Well),
  data = alldata164summary,
  family = binomial(link= "logit")
)
glmm_model164

confint(glmm_model164, method = "Wald")

Anova(glmm_model164)

# EMM and pairwise comparisons
emm164 <- emmeans(glmm_model164, ~ Treatment)
emm164

pairs(emm164) 

pairs_emm164 <- pairs(emm164, adjust = "tukey") %>%
  summary(infer = TRUE)

# Extracting the relevant pairwise interactions for the conditions
sig_contrasts <- pairs_emm164 %>%
  filter(
    (contrast == "Hyp - Con") |
      (contrast == "Hyp + IR - Hyp") |
      (contrast == "Hyp + IR - IR")
  )

# Collecting all EMM data to put in a data frame to plot alongside raw data to display findings from GLMM
emm_probs164 <- as.data.frame(summary(emm164, type = "response"))
View(emm_probs164)


# Plot code with raw data and calculated data with significance
ggplot(data = alldata164summary_avg, aes(x = Treatment, y = MeanPercent)) +
  geom_jitter(aes(color = Treatment, shape = Block),
              width = 0.2, size = 4, alpha = 0.6) +  # raw dots
  geom_point(data = emm_probs164, 
             aes(x = Treatment, y = prob * 100), 
             inherit.aes = FALSE,
             size = 4, shape = 19, color = "black", stroke = 1.2) +  # model mean
  geom_errorbar(data = emm_probs164,
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
    y_position = c(35, 85, 100),
    tip_length = 0.01,
    textsize = 4
  )

# Conducting further tests to verify if GLMM significance was correct due to slight deviation from DHARMa test, was fine
compare_means(Percent ~ Treatment, data = alldata164summary, method = "wilcox.test", p.adjust.method = "bonferroni")
