#CSV retrospective code for comparison of Ct values in matched pairs via wilcoxon signed rank test

library(tidyverse)

matched_pairs <- read_csv2("matched_pairs_comparison.csv")
attach(matched_pairs)

#Shapiro test
shapiro.test(CT_S_CSF_day1)
shapiro.test(CT_L_CSF_day1)
shapiro.test(CT_S_blood_day1)
shapiro.test(CT_L_blood_day1)

#Wilcoxon signed rank test
paired_wilcox_results_S <- wilcox.test(CT_S_CSF_day1, CT_S_blood_day1, paired = TRUE)
paired_wilcox_results_L <- wilcox.test(CT_L_CSF_day1, CT_L_blood_day1, paired = TRUE)






attach(day1_pairs_comparison)
shapiro.test(CT_S_CSF_day1)
shapiro.test(CT_L_CSF_day1)
shapiro.test(CT_S_blood_day1)
shapiro.test(CT_L_blood_day1)

