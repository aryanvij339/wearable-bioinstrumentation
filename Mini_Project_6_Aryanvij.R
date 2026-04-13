# Mini-Project 6 Validation

# clear workspace
rm(list = ls())

# load packages
install.packages("magrittr")
library(tidyverse)
library(magrittr)

# load data
rrData <- read_csv("C:/Users/aryan/Downloads/rrData.csv") # adjust path to where your .csv file is, data should be 250 obs. x 4 variables
rrData$participant <- factor(rrData$participant) # make participant variable a factor
table(rrData$participant) # should be 10 repeats per participant


# LINE PLOT ----
# reshape the data into long format so that there are 4 columns: participant, time, feature (rr or rr_fft), and value
data_long <- rrData %>%
  pivot_longer(cols = c(rr, rr_fft), names_to = "feature", values_to = "value")

# line plot
ggplot(data_long, aes(x = time, y = value, color = feature, group = feature)) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(~ participant) +
  xlab("Elapsed Time (s)") +
  ylab("RR (brpm)") +
  labs(color = "Feature") +
  ggtitle("Figure 1: Line Plot")


# BAR PLOT ----
# find the mean and standard deviation within each participant-feature
summary_data <- data_long %>%
  group_by(participant, feature) %>%
  summarize(mean = mean(value), sd = sd(value), .groups = "drop") # fill in group_by() and summarize() functions, should be 50 obs. x 4 variables

# bar plot
ggplot(summary_data, aes(x = participant, y = mean, fill = feature)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  xlab("Participant") +
  ylab("RR (brpm)") +
  labs(fill = "Feature") +
  ggtitle("Figure 2: Bar Plot")


# SCATTER PLOT ----
# fit linear model to data, y = rr_fft, x = rr)
fit <- lm(rrData$rr_fft ~ rrData$rr)

# combine text for equation
eq <- substitute(italic(y) == a + b %.% italic(x)*", "~~italic(r)^2~"="~r2, 
                 list(a = format(unname(coef(fit)[1]), digits = 2),
                      b = format(unname(coef(fit)[2]), digits = 2),
                      r2 = format(summary(fit)$r.squared, digits = 2)))
text <- as.character(as.expression(eq))

# scatter plot
ggplot(rrData, aes(x = rr, y = rr_fft)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  xlab("RR (brpm)") +
  ylab("RR FFT (brpm)") +
  ggtitle("Figure 3: Scatter Plot") +
  annotate("text", x = 30, y = 30, label = text, parse = TRUE)


# BLAND-ALTMAN PLOT ----
# calculate and save the differences between the two measures and the averages of the two measures
rrData <- rrData %>%
  mutate(diff = rr - rr_fft, avg = (rr + rr_fft) / 2)

#compute the mean and limits of agreement (LoA)
mean_bias <- mean(rrData$diff)
sd_diff <- sd(rrData$diff)
upper_loa <- mean_bias + 1.96 * sd_diff
lower_loa <- mean_bias - 1.96 * sd_diff

# Bland-Altman plot
ggplot(rrData, aes(x = avg, y = diff)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = mean_bias, color = "darkgreen", linewidth = 1) +
  geom_hline(yintercept = upper_loa, color = "orange", linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = lower_loa, color = "orange", linetype = "dashed", linewidth = 1) +
  annotate("text", x = max(rrData$avg), y = upper_loa, label = paste("Upper LoA =", round(upper_loa, 2)), hjust = 1, vjust = -0.5) +
  annotate("text", x = max(rrData$avg), y = mean_bias, label = paste("Mean Bias =", round(mean_bias, 2)), hjust = 1, vjust = -0.5) +
  annotate("text", x = max(rrData$avg), y = lower_loa, label = paste("Lower LoA =", round(lower_loa, 2)), hjust = 1, vjust = 1.5) +
  xlab("Average of Measures (brpm)") +
  ylab("Difference Between Measures (rr - rr_fft) (brpm)") +
  ggtitle("Figure 4: Bland-Altman Plot")


# BOX PLOT ----
# box plot
ggplot(rrData, aes(x = participant, y = diff, fill = participant)) +
  geom_boxplot() +
  xlab("Participant") +
  ylab("Difference Between Measures (rr - rr_fft) (brpm)") +
  ggtitle("Figure 5: Box Plot") +
  theme(legend.position = "none")
