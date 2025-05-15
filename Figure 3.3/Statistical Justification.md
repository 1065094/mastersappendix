I chose to do a Mann-Whitney U test (AKA Wilcoxon Rank) for this data, despite a t-test being the original thought, as comparing the means between the two groups per independent cell line seemed ideal. However, due to the assumption of the homogenenity of variance not being met, ties being present (repeated values across wells) and a small sample size due to it being a preliminary investigation (with there being n = 3 measurements per condition and cell line), Mann-Whitney U seemed like a better idea. 

The use of the exact test as opposed to Wilcoxon would ensure that the correct p-values were obtained despite limited repeats whilst avoiding parametric assumptions of normality and more importantly equal variance.

A boxplot was chosen to visualise the data effectively, as given the small sample size, the mean may have been misleading due to skew by extreme data points. Therefore, a median is more appropriate. Raw data was also included for transparency and to demonstrate if there was any variability between conditions.


