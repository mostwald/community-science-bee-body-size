# Notes from Nature ITD Analysis

library(ggplot2)
library(dplyr)
library(tidyr)
library(car)
library(scales)

df <- read.csv("nfn_data_Feb25.csv")
df <- subset(df, T3_1.Were.you.able.to.measure.the.bee.!="No") #remove rows where community scientist was not able to measure the bee

# Get summary statistics
summary_df <- df %>%
  group_by(catalogNumber) %>%
  mutate(total_n = n()) %>%  # Add a column with the total count for each catalogNumber
  ungroup() %>%
  filter(total_n == 10) %>%  # Keep only rows where the total count is 10 or more (5 researcher 5 CS measurements)
  group_by(catalogNumber, measurer_category) %>%
  summarise(
    mean_itd = mean(itd_mm, na.rm = TRUE),
    median_itd = median(itd_mm, na.rm = TRUE),
    sd_itd = sd(itd_mm, na.rm = TRUE)
  )

# Subset for different measurer categories
community_scientist_df <- summary_df %>%
  filter(measurer_category == "community_scientist")
researcher_df <- summary_df %>%
  filter(measurer_category == "researcher")

# Assess normality
qqPlot(community_scientist_df$mean_itd)
qqPlot(log(community_scientist_df$mean_itd))
qqPlot(researcher_df$mean_itd)
qqPlot(log(researcher_df$mean_itd))


#### Are community scientist measurements different from researcher measurements? ####

wilcoxon_test_mean <- wilcox.test(mean_itd ~ measurer_category, data = summary_df, paired = TRUE)
wilcoxon_test_median <- wilcox.test(median_itd ~ measurer_category, data = summary_df, paired = TRUE)
wilcoxon_test_sd <- wilcox.test(sd_itd ~ measurer_category, data = summary_df, paired = TRUE)





#### Plot comparisons ####

vp1 <- ggplot(summary_df, aes(measurer_category, median_itd, fill=measurer_category)) +
  geom_violin() +
  geom_boxplot(width=0.1, outliers = FALSE, fill="white")+
  labs(x = NULL,y = "Median ITD", size=12) +
  theme_classic() +
  scale_x_discrete(labels=c("Community \nScientists", "Researchers")) +
  theme(axis.text.x = element_text(color="black", size = 10),axis.text.y = element_text(color="black", size = 10), legend.position = "none")

vp2 <- ggplot(summary_df, aes(measurer_category, mean_itd, fill=measurer_category)) +
  geom_violin() +
  geom_boxplot(width=0.1, outliers = FALSE, fill="white")+
  scale_y_continuous(labels=label_number(accuracy=0.1)) +
  labs(x = NULL,y = "Mean ITD", size=12) +
  theme_classic() +
  scale_x_discrete(labels=c("Community \nScientists", "Researchers")) +
  theme(axis.text.x = element_text(color="black", size = 10),axis.text.y = element_text(color="black", size = 10), legend.position = "none")

vp3 <- ggplot(summary_df, aes(measurer_category, sd_itd, fill=measurer_category)) +
  geom_violin() +
  geom_boxplot(width=0.1, outliers = FALSE, fill="white")+
  scale_y_continuous(labels=label_number(accuracy=0.1)) +
  labs(x = NULL,y = "Std. Dev. ITD", size=12) +
  theme_classic() +
  scale_x_discrete(labels=c("Community \nScientists", "Researchers")) +
  theme(axis.text.x = element_text(color="black", size = 10),axis.text.y = element_text(color="black", size = 10), legend.position = "none")

vp1 + scale_fill_brewer(palette="PuRd")
vp2 + scale_fill_brewer(palette="PuRd")
vp3 + scale_fill_brewer(palette="PuRd")


#### What is the percent error in the measurements? ####

# Convert to wide format
wide_summary_df <- summary_df %>%
  pivot_wider(
    names_from = measurer_category,
    values_from = c(mean_itd, sd_itd, median_itd),
    names_sep = "_"  # Use underscore to separate names
  )

# Calculate the percent error in means
wide_summary_df <- wide_summary_df %>%
  mutate(
    mean_percent_difference = (abs(mean_itd_community_scientist - mean_itd_researcher) / mean_itd_researcher) * 100
  )

# Calculate the percent error in medians
wide_summary_df <- wide_summary_df %>%
  mutate(
    median_percent_difference = (abs(median_itd_community_scientist - median_itd_researcher) / median_itd_researcher) * 100
  )

mean_mean <- mean(wide_summary_df$mean_percent_difference)
mean_median <- mean(wide_summary_df$median_percent_difference)
range(wide_summary_df$mean_percent_difference)

# calculate proportion of error values < 1
mean(wide_summary_df$median_percent_difference < 1, na.rm = TRUE)


#### Do CS over- or underestimate ITD? Calculate mean signed error ####

MSE_median <- mean(wide_summary_df$median_itd_community_scientist - wide_summary_df$median_itd_researcher)
MSE_mean <- mean(wide_summary_df$mean_itd_community_scientist - wide_summary_df$mean_itd_researcher)


#### Plot frequencies of percent error values ####

ggplot(wide_summary_df, aes(x=mean_percent_difference)) +
  geom_histogram(binwidth=5)+ 
  labs(x= "% error in mean measurements", y = "Frequency", size=12) +
  theme_classic() +
  theme(axis.text.x = element_text(color="black", size = 10),axis.text.y = element_text(color="black", size = 10)) +
  coord_cartesian(xlim = c(-2, 85), ylim = c(0, 320)) +
  geom_vline(xintercept=mean_mean, linetype="dashed", color = "#b575bf")

ggplot(wide_summary_df, aes(x=median_percent_difference)) +
  geom_histogram(binwidth=5)+ 
  labs(x= "% error in median measurements", y = "Frequency", size=12) +
  theme_classic() +
  theme(axis.text.x = element_text(color="black", size = 10),axis.text.y = element_text(color="black", size = 10)) +
  coord_cartesian(xlim = c(-2, 85), ylim = c(0, 320)) +
  geom_vline(xintercept=mean_median, linetype="dashed", color = "#b575bf")


# Calculate the effect size 
# Get the Z value from the test statistic
z_value_mean <- qnorm(wilcoxon_test_mean$p.value / 2)  # Divide p-value by 2 for two-tailed test
z_value_median <- qnorm(wilcoxon_test_median$p.value / 2)
                        
# Calculate the number of observations (N)
N_mean <- length(wide_summary_df$mean_itd_community_scientist) 
N_median <- length(wide_summary_df$median_itd_community_scientist) 

# Calculate the effect size (r)
r_mean <- z_value_mean / sqrt(N_mean)
r_median <- z_value_median / sqrt(N_median)

