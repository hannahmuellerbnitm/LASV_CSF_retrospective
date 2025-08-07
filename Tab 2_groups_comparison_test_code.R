## Code for comparison of demographic data and reported symptoms in Table 2
# Load CSV data (Seperation semicolon, decimal comma)
data <- read.csv("symptoms_comparison.csv", sep = ";", dec = ",") 

# Definition of groups for comparison: CSF and plasma double positive: double_positive vs. plasma positive, but CSF negative: CSF_negative_lassa, vs. CSF and plasma double negative: double_negative
data$Group <- with(data, ifelse(CSF == 1 & Blood == 1, "double_positive",
                                ifelse(CSF == 0 & Blood == 1, "csf_negative_lassa",
                                       ifelse(CSF == 0 & Blood == 0, "double_negative", NA))))

# Definition of variables for comparison 
categorical_vars <- c("SEX", "WEAKNESS", "FEVER", "SORE_THROAT", "COUGH", "DYSPNEA", "CHEST_PAIN", 
                      "ABDOMINAL_PAIN", "DIARRHEA", "VOMITING", "HEADACHE", "SEIZURE", "BLEEDING", "Outcome")

numeric_var <- "AGE"

# Definition of data frame for results 
results <- data.frame(Variable=character(), Comparison=character(), Test=character(), p_value=numeric(), stringsAsFactors=FALSE)

# Test for categorial variables
test_categorical <- function(var, group1, group2) {
  subset_data <- subset(data, Group %in% c(group1, group2) & !is.na(data[[var]]))
  subset_data$Group <- factor(subset_data$Group, levels = c(group1, group2))
  tab <- table(factor(subset_data[[var]], levels = c(0, 1)), subset_data$Group)
  if (all(dim(tab) == c(2, 2))) {
    if (any(tab < 5)) {
      test <- "Fisher"
      p <- fisher.test(tab)$p.value
    } else {
      test <- "Chi-squared"
      p <- chisq.test(tab, correct = FALSE)$p.value
    }
    comparison <- paste(group1, "vs", group2)
    results <<- rbind(results, data.frame(Variable=var, Comparison=comparison, Test=test, p_value=p))
  }
}

# Function for numeric variable
test_numeric <- function(var, group1, group2) {
  subset_data <- subset(data, Group %in% c(group1, group2) & !is.na(data[[var]]))
  subset_data$Group <- factor(subset_data$Group, levels = c(group1, group2))
  group1_data <- subset_data[[var]][subset_data$Group == group1]
  group2_data <- subset_data[[var]][subset_data$Group == group2]
  p <- wilcox.test(group1_data, group2_data)$p.value
  comparison <- paste(group1, "vs", group2)
  results <<- rbind(results, data.frame(Variable=var, Comparison=comparison, Test="Mann-Whitney U", p_value=p))
}

### Calculation of comparisons ###

# Comparison 1: double_positive vs double_negative
for (var in categorical_vars) {
  test_categorical(var, "double_positive", "double_negative")
}
test_numeric(numeric_var, "double_positive", "double_negative")

# Comparison 2: double_positive vs csf_negative_lassa
for (var in categorical_vars) {
  test_categorical(var, "double_positive", "csf_negative_lassa")
}
test_numeric(numeric_var, "double_positive", "csf_negative_lassa")

# Saving results in CSV file
write.csv(results, "symptoms_comparison_results.csv", row.names = FALSE)
