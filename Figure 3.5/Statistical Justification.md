As mentioned in the code, similar to Figure 3.4, I chose to do a Generalized Linear Mixed Model (GLMM) to understand the effect of treatment on the proportion of senescent (blue-stained) cells. The response variable was binomial (number of blue vs. not-blue cells), which made me choose the binomial function.
Since the dataset and the experimental set-up was similar, the logical approach was to follow the same statistical test.
The fixed effect was Treatment, of which there were five: Untreated, Hypoxia, Irradiated, Hypoxia and Irradiation, and Etoposide.
These were to be compared in the manner to answer the three questions outlined in the Results (3.5) of my dissertation and therefore the comparisons this time were:
- Con V Hyp
- Hyp V Hyp + IR
- IR V Hyp + IR
  
A GLMM was also appropriate to account and mitigate for variability from random effects, which in this case were:
- Block: to account for experiments performed on different days, N = 3 (repeats), n = 45 (observations). Df was not reported due to the method of z-testing which results in Infinite degrees of freedom.
- Well: to account for triplicates within block
Accounting for random effects also helps reduce the risk of pseudo-replication or inflated type I error by acknowledging non-independence among repeated measures.
Estimated marginal means and pairwise comparisons were then used to assess treatment effects on the probability of cells appearing senescence.
- All pairwise comparisons between all five conditions can be seen if the code for this is run
Before conducting the GLMM, appropriateness was tested for by using DHARMa to confirm model fit, residual distribution, and dispersion assumptions.
In this case, there was slight deviation from the test conducted (see 3.5DHARMA.md) though this was still largely appropriate.
- To err on the side of caution and verify GLMM significance aligned well, a Wilcoxon rank signed test was conducted alongside it with a Bonferroni correction. 

This approach allows for appropriate significance to be determined while controlling for both fixed experimental manipulations and random variability introduced by the design and corrects for any deviance.

All data points were included, alongside the mean proportion calculated supplemented with confidence intervals to highlight variation and significance or lack thereof. Colours represent each condition and this is kept consistent across Figures from 3.4 onwards and similarly with shapes which represent the block as aforementioned.


