library(readxl)
library(dplyr)
library(ggplot2)
library(lme4)
library(emmeans)
library(DHARMa)
library(ggpubr)
library(ggsignif)
library(car)
# Loading in the packages needed

# Load data
returntonorms <- read_excel("E:/returntonormoxia/returntonormallthreesummary.xlsx")

# Making sure the treatments are in the correct order to present and making them into factors
returntonorms$Treatment <- factor(returntonorms$Treatment, levels = c("UT", "H", "IR", "B", "E"),
                       labels = c("Con", "Hyp", "IR", "Hyp + IR", "Etop"))
returntonorms$Block <- factor(returntonorms$Block)
returntonorms$Well <- factor(returntonorms$Well)

# Creating observation-level random effect (OLRE) to account for overdispersion
returntonorms$Obs <- factor(seq_len(nrow(returntonorms)))
View(returntonorms)

# Summarising the data to calculate the mean 
# Calculating the proportion of blue cells - since given the rationale followed from figures 3.4 & 3.5, a GLMM will be applied again
returntonorms_avg <- returntonorms %>%
  mutate(PropBlue = Blue / All_cells) %>%
  group_by(Treatment, Block, Well) %>%
  summarise(MeanPercent = mean(PropBlue * 100, na.rm = TRUE), .groups = "drop")

# Fit GLMM with OLRE
glmm_modelreturntonorms <- glmer(
  cbind(Blue, All_cells - Blue) ~ Treatment + (1 | Block) + (1 | Well) + (1 | Obs),
  data = returntonorms,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)

# Check appropriateness of GLMM model
simulationOutput <- simulateResiduals(fittedModel = glmm_modelreturntonorms)
plot(simulationOutput)

# Get confidence intervals
confint_profile <- confint(glmm_modelreturntonorms, method = "profile")

# Calculating estimated marginal means
emmreturntonorms <- emmeans(glmm_modelreturntonorms, ~ Treatment)
emm_summaryreturntonorms <- summary(emmreturntonorms, type = "response", infer = TRUE)

# Pairwise comparisons (Tukey)
pairs_emmreturntonorms <- pairs(emmreturntonorms, adjust = "tukey") %>%
  summary(infer = TRUE)


# Plot
ggplot(data = returntonorms_avg, aes(x = Treatment, y = MeanPercent)) +
  geom_jitter(aes(color = Treatment, shape = Block),
              width = 0.2, size = 4, alpha = 0.6) +
  geom_point(data = emm_summaryreturntonorms,
             aes(x = Treatment, y = prob * 100),
             inherit.aes = FALSE,
             size = 4, shape = 19, color = "black") +
  geom_errorbar(data = emm_summaryreturntonorms,
                aes(x = Treatment, ymin = asymp.LCL * 100, ymax = asymp.UCL * 100),
                inherit.aes = FALSE,
                width = 0.1, color = "black") +
  scale_color_manual(values = c("Con" = "#B4ECE7", "Etop" = "#42BFF0", "IR" = "#5FB7DA", 
                                "Hyp" = "#B0D5E4", "Hyp + IR" = "#97DDFA")) +
  scale_shape_manual(values = c(19, 21, 15)) +
  ylab("Percent Blue Cells") +
  xlab(NULL) +
  scale_y_continuous(limits = c(-1, 120), breaks = seq(0, 100, 25)) +
  theme_classic() + geom_signif(comparisons = list(c("Hyp", "Con"),
                                                   c("Hyp + IR", "Hyp"),
                                                   c("Hyp + IR", "IR")),
                                map_signif_level = function(p) {
                                  ifelse(p < 0.001, "***",
                                         ifelse(p < 0.01, "**",
                                                ifelse(p < 0.05, "*", "ns")))
                                },
                                y_position = c(45, 100, 85),
                                tip_length = 0.01,
                                textsize = 4
  ) 
