library(readxl)
library(ggplot2)
library(dplyr)

# Import the Excel sheet
exp2 <- read_excel("C:\\Users\\James Doan\\Dropbox\\James\\Experiment 02 - Sex Differences\\Exp02_Data_Evaluation.xlsx", sheet = "2024-03-15_Eval2.0Sec_Raw")

# View the original dataset
View(exp2)

# Group by gender
gender <- exp2 %>% 
  group_by(Gender) %>% # Modifies df to prepare for analysis
  arrange(Gender) # Needed to change the df

# View the new gender df
View(gender)

# Calculate the number of each sex
gender_counts <- table(exp2$Gender)
female_counts <- gender_counts["F"]
male_counts <- gender_counts["M"]

# Calculate row averages across first two columns
gender$bgr_avg <- rowMeans(exp2[, c("Bgr1", "Bgr2")], na.rm = TRUE)

# Calculate protein expression levels
gender$Girk2expression <- gender$bgr_avg - gender$Girk2
gender$GAPDHexpression <- gender$bgr_avg - gender$GADPH

# Calculate Girk2:GAPDH Expression
gender$RatioGirk2GAPDH <- gender$Girk2expression/gender$GAPDHexpression

# Find avg of all ratios
avg_all_ratios <- mean(gender$RatioGirk2GAPDH)

# Calculate Percent Ratio
gender$PercentRatio <-  gender$RatioGirk2GAPDH*100/avg_all_ratios

# Filter for females only
female <- gender %>%
  filter(Gender == "F")
female_data$PercentRatio <-  female$RatioGirk2GAPDH*100/avg_all_ratios
female_average <- mean(female_data$PercentRatio, na.rm = TRUE)
female_stdev <- sd(female_data$PercentRatio)
View(female_data)

# Filter for males only
male <- gender %>%
  filter(Gender == "M")
male$PercentRatio <-  male$RatioGirk2GAPDH*100/avg_all_ratios
male_average <- mean(male_data$PercentRatio, na.rm = TRUE)
male_stdev <- sd(male_data$PercentRatio)
View(male_data)

# 2 tailed t-test
ttest <- twotail_ttest <- t.test(female$PercentRatio, male$PercentRatio)