library(readxl)
library(dplyr)
library(ggplot2)
library(lme4)
library(emmeans)
library(ggpubr)
library(ggsignif)
library(DHARMa)
library(car)
# Loading in packages

# Loading in the data
uteirthree <- read_excel("E:/uteandirthree/collatedrfriendlyresults.xlsx", sheet = 2)

# Making sure the treatments are in the correct order to present on the graph and making them a factor
uteirthree$Treatment <- factor(uteirthree$Treatment, levels = c("Untreated", "Irradiated", "Etoposide"))
uteirthree$Block <- factor(uteirthree$Block, levels = c("1", "2", "3"))

# Restructuring the data frame to work out the percentage of blue cells present out of all cells, including it as a proportion as this is appropriate for the upcoming statistical test
uteirthree <- uteirthree %>%
  mutate(All = `Blue` + `Not blue`,
         Percent = (Blue / All) * 100)

# Summarising the data to calculate the mean to account for extra readings per well for increased sample size
uteirthree_avg <- uteirthree %>%
  group_by(Treatment, Block, Well) %>%
  summarise(
    MeanPercent = mean(Percent, na.rm = TRUE),
    .groups = "drop"
  )

# Given the experimental setup, a GLMM seems most appropriate (see Figure 3.4 Read.me), so tests were conducted to confirm this with the dataset
testDispersion(glmm_model)
simulationOutput <- simulateResiduals(fittedModel = glmm_model, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)

# Since all is appropriate, GLMM testing goes forward
# Building a model to account for the fixed effect of treatment and the random effects of block (which refers to the repeat on different days) and wells (which are triplicates within a repeat) to ensure variability from these two random effects does not overpower the fixed effect
glmm_model <- glmer(
  cbind(Blue, `Not blue`) ~ Treatment + (1 | Block) + (1 | Well),
  data = uteirthree,
  family = binomial(link= "logit")
)
glmm_model

confint(glmm_model, method = "Wald")

# To provide a summary of the outcome
Anova(glmm_model)

# Following the creation of the GLMM model, estimated marginal means is calculated to understand the significance in interactions between treatments 
# This resulted in p-values of <.0001, and slope differences of 2.632 (UT V IR), 1.823 (UT V E), 0.809 (IR V E)
emm <- emmeans(glmm_model, ~ Treatment)
emm
emm_pairs <- pairs(emm) 
# Putting EMM in a data frame to use for plotting alongside raw data
emm_probs <- as.data.frame(summary(emm, type = "response"))
View(emm_probs)
emm_summary <- summary(emm_pairs, infer = TRUE)
# View the p-values and contrasts
emm_summary

# Plotting the graph with all three treatments and the findings from the GLMM
ggplot(data = uteirthree, aes(x = Treatment, y = Percent)) +
  geom_jitter(aes(color = Treatment, shape = Block),
              width = 0.2, size = 4, alpha = 0.6) +  # raw dots
  geom_point(data = emm_probs, 
             aes(x = Treatment, y = prob * 100), 
             inherit.aes = FALSE,
             size = 4, shape = 19, color = "black", stroke = 1.2) +  # model mean
  geom_errorbar(data = emm_probs,
                inherit.aes = FALSE,
                aes(x = Treatment, ymin = asymp.LCL * 100, ymax = asymp.UCL * 100), # upper and lower bounds
                width = 0.1, color = "black") + 
  scale_color_manual(values = c("Untreated" = "#B4ECE7", "Irradiated" = "#42BFF0", "Etoposide" = "#5FB7DA")) +
  scale_shape_manual(values = c(19, 21, 15)) +
  ylab("Percent Blue Cells") +
  xlab(NULL) +
  scale_y_continuous(limits = c(-1, 120), breaks = seq(0, 100, 25)) +
  theme_classic() +
  ggtitle(NULL) +
  geom_signif(
    comparisons = list(
      c("Untreated", "Irradiated"),
      c("Untreated", "Etoposide")
    ),
    map_signif_level = function(p) {
      ifelse(p < 0.001, "***",
             ifelse(p < 0.01, "**",
                    ifelse(p < 0.05, "*", "ns")))
    },
    y_position = c(100, 110),  
    tip_length = 0.01,
    textsize = 4
  )




  
