As mentioned in the code, I chose to do a Generalized Linear Mixed Model (GLMM) to understand the effect of treatment on the proportion of senescent (blue-stained) cells. The response variable was binomial (number of blue vs. not-blue cells), which made me choose the binomial function.
The fixed effect was Treatment, of which there were three: Untreated, Irradiated, Etoposide, and these were to be compared as UT V IR, UT V E.
A GLMM was also appropriate to account and mitigate for variability from random effects, which in this case were:
- Block: to account for experiments performed on different days, N = 3 (repeats), n = 45 (observations). Df was not reported due to the method of z-testing which results in Infinite degrees of freedom.
- Well: to account for triplicates within block
Accounting for random effects also helps reduce the risk of pseudo-replication or inflated type I error by acknowledging non-independence among repeated measures.
Estimated marginal means and pairwise comparisons were then used to assess treatment effects on the probability of cells appearing senescence.
Before conducting the GLMM, appropriateness was tested for by using DHARMa to confirm model fit, residual distribution, and dispersion assumptions. 

This approach allows for appropriate significance to be determined while controlling for both fixed experimental manipulations and random variability introduced by the design.

All data points were included, alongside the mean proportion calculated supplemented with confidence intervals to highlight variation and significance or lack thereof. Colours represent each condition and this is kept consistent across Figures from 3.4 onwards and similarly with shapes which represent the block as aforementioned.
