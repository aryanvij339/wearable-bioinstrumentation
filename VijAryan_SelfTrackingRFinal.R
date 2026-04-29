library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

DATA_PATH  <- "wearable_data.xlsx"
OUTPUT_DIR <- "figures"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

df <- read_excel(DATA_PATH, sheet = "wearable_data", skip = 1)

df <- df %>%
  mutate(
    Date      = as.Date(Date),
    Week      = sub("_.*", "", Day_Label),
    Weekday   = sub(".*_", "", Day_Label),
    Weekday   = factor(Weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri")),
    Week      = factor(Week, levels = paste0("Week", 1:4)),
    Day_Index = row_number()
  )

cat("Loaded", nrow(df), "rows of data\n")
print(head(df))

scale01 <- function(x) 100 * (x - min(x, na.rm = TRUE)) /
  (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

df_scaled <- df %>%
  mutate(
    Steps_Scaled     = scale01(Steps),
    HeartRate_Scaled = scale01(Heart_Rate),
    Sleep_Scaled     = scale01(Sleep_Hours),
    Stress_Scaled    = scale01(Stress)
  )

df_long <- df_scaled %>%
  select(Day_Index, Date, Week, Weekday,
         Steps_Scaled, HeartRate_Scaled, Sleep_Scaled, Stress_Scaled) %>%
  pivot_longer(
    cols = c(Steps_Scaled, HeartRate_Scaled, Sleep_Scaled, Stress_Scaled),
    names_to = "Measure", values_to = "Value"
  ) %>%
  mutate(
    Measure = recode(Measure,
                     Steps_Scaled     = "Steps",
                     HeartRate_Scaled = "Heart Rate",
                     Sleep_Scaled     = "Sleep Hours",
                     Stress_Scaled    = "Stress"),
    Measure = factor(Measure,
                     levels = c("Steps", "Heart Rate", "Sleep Hours", "Stress"))
  )
hi_step   <- df_scaled[which.max(df_scaled$Steps), ]
lo_sleep  <- df_scaled[which.min(df_scaled$Sleep_Hours), ]
hi_stress <- df_scaled[which.max(df_scaled$Stress), ]

fig2 <- ggplot(df_long, aes(x = Day_Index, y = Value, color = Measure)) +
  geom_line(size = 0.9) +
  geom_point(size = 1.8) +
  geom_vline(xintercept = hi_step$Day_Index,
             linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = lo_sleep$Day_Index,
             linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = hi_stress$Day_Index,
             linetype = "dashed", color = "grey40") +
  annotate("text", x = hi_step$Day_Index,   y = 102,
           label = "Highest step day",   hjust = 0.5, size = 3.2) +
  annotate("text", x = lo_sleep$Day_Index,  y = -2,
           label = "Lowest sleep day",   hjust = 0.5, size = 3.2) +
  annotate("text", x = hi_stress$Day_Index, y = 95,
           label = "Highest stress day", hjust = 0.5, size = 3.2) +
  scale_color_manual(values = c("Steps"      = "#2EB5E5",
                                "Heart Rate" = "#E57373",
                                "Sleep Hours"= "#9CCC65",
                                "Stress"     = "#BA68C8")) +
  labs(title    = "Four Weeks of Wearable Self-Tracking Data",
       subtitle = "Scaled comparison of steps, heart rate, sleep, and stress over time",
       x = "Day of Tracking", y = "Scaled Value (0-100)",
       color = "Measure") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

ggsave(file.path(OUTPUT_DIR, "fig2_time_series.png"),
       fig2, width = 10, height = 5.5, dpi = 200)
print(fig2)

heat_df <- df_long %>%
  group_by(Measure, Week, Weekday) %>%
  summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")

fig3 <- ggplot(heat_df, aes(x = Weekday, y = Week, fill = Value)) +
  geom_tile(color = "white", linewidth = 0.4) +
  facet_wrap(~ Measure, ncol = 2) +
  scale_fill_gradient(low = "#DEEBF7", high = "#08306B",
                      name = "Scaled\nValue", limits = c(0, 100)) +
  labs(title    = "Wearable Self-Tracking Heat Map",
       subtitle = "Four weeks of weekday patterns across activity, heart rate, sleep, and stress",
       x = "Weekday", y = "Tracking Week") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        panel.grid = element_blank())

ggsave(file.path(OUTPUT_DIR, "fig3_heatmap.png"),
       fig3, width = 10, height = 6, dpi = 200)
print(fig3)

ct <- cor.test(df$Steps, df$Stress, method = "pearson")
r_val <- round(unname(ct$estimate), 3)
p_val <- ct$p.value
p_str <- if (p_val < 0.001) "p < 0.001" else paste0("p = ", signif(p_val, 3))
n_val <- nrow(df)

lm_fit <- lm(Stress ~ Steps, data = df)
b0 <- round(unname(coef(lm_fit)[1]), 1)
b1 <- signif(unname(coef(lm_fit)[2]), 3)
eq_str <- paste0("y = ", b0, " + (", b1, ") \u00B7 x")

fig4 <- ggplot(df, aes(x = Steps, y = Stress)) +
  geom_smooth(method = "lm", se = TRUE,
              color = "#6F1A36", fill = "#F7CAAC", alpha = 0.4) +
  geom_point(aes(color = Week), size = 3, alpha = 0.85) +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(labels = label_comma()) +
  annotate("label",
           x = max(df$Steps) * 0.98, y = max(df$Stress) * 0.98,
           label = paste0(eq_str, "\nr = ", r_val, "    ", p_str,
                          "    n = ", n_val),
           hjust = 1, vjust = 1, size = 4,
           fill = "white", label.size = 0.3) +
  labs(title    = "Steps vs Stress: Daily Wearable Data",
       subtitle = "Pearson correlation across 4 tracking weeks (20 weekdays)",
       x = "Daily Steps", y = "Stress (0-100 index)",
       color = "Week") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right")

ggsave(file.path(OUTPUT_DIR, "fig4_steps_vs_stress.png"),
       fig4, width = 9, height = 5.5, dpi = 200)
print(fig4)

bar_df <- df %>%
  pivot_longer(cols = c(Steps, Heart_Rate, Sleep_Hours, Stress),
               names_to = "Measure", values_to = "Value") %>%
  mutate(Measure = recode(Measure,
                          Steps       = "Steps",
                          Heart_Rate  = "Heart Rate (bpm)",
                          Sleep_Hours = "Sleep Hours",
                          Stress      = "Stress"),
         Measure = factor(Measure,
                          levels = c("Steps", "Heart Rate (bpm)",
                                     "Sleep Hours", "Stress"))) %>%
  group_by(Measure, Weekday) %>%
  summarise(Mean = mean(Value), SD = sd(Value), .groups = "drop")

fig5 <- ggplot(bar_df, aes(x = Weekday, y = Mean, fill = Weekday)) +
  geom_col(color = "black", linewidth = 0.3, width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                width = 0.2, linewidth = 0.4) +
  facet_wrap(~ Measure, scales = "free_y", ncol = 2) +
  scale_fill_brewer(palette = "Set2") +
  labs(title    = "Mean Daily Value by Weekday",
       subtitle = "Bars show mean \u00B1 SD across 4 weeks of tracking",
       x = "Weekday", y = "Value") +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold"),
        legend.position = "none",
        strip.text    = element_text(face = "bold"))

ggsave(file.path(OUTPUT_DIR, "fig5_bar_plot.png"),
       fig5, width = 10, height = 6, dpi = 200)
print(fig5)

fig6 <- ggplot(df_long, aes(x = Weekday, y = Value, fill = Weekday)) +
  geom_boxplot(alpha = 0.85, outlier.shape = 21, outlier.size = 1.6) +
  facet_wrap(~ Measure, ncol = 2) +
  scale_fill_brewer(palette = "Set2") +
  labs(title    = "Distribution of Scaled Measures by Weekday",
       subtitle = "Box plots across 4 tracking weeks (values scaled to 0-100)",
       x = "Weekday", y = "Scaled Value (0-100)") +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold"),
        legend.position = "none",
        strip.text    = element_text(face = "bold"))

ggsave(file.path(OUTPUT_DIR, "fig6_box_plot.png"),
       fig6, width = 10, height = 6, dpi = 200)
print(fig6)

cat("\n====================================================================\n")
cat("MAIN TEST  Pearson: Steps vs Stress (n =", n_val, ")\n")
cat("====================================================================\n")
print(ct)

cat("\n--- Additional pairings (for reference / discussion) ---\n")
cat("\nSteps vs Sleep_Hours:\n");  print(cor.test(df$Steps,       df$Sleep_Hours))
cat("\nHeart_Rate vs Stress:\n");  print(cor.test(df$Heart_Rate,  df$Stress))
cat("\nSleep_Hours vs Stress:\n"); print(cor.test(df$Sleep_Hours, df$Stress))

summary_df <- df %>%
  summarise(
    n           = n(),
    mean_steps  = mean(Steps),
    mean_hr     = mean(Heart_Rate),
    mean_sleep  = mean(Sleep_Hours),
    mean_stress = mean(Stress),
    sd_steps    = sd(Steps),
    sd_hr       = sd(Heart_Rate),
    sd_sleep    = sd(Sleep_Hours),
    sd_stress   = sd(Stress)
  )
write.csv(summary_df, file.path(OUTPUT_DIR, "summary_stats.csv"), row.names = FALSE)

cat("\nAll figures saved to ./", OUTPUT_DIR, "/\n", sep = "")
cat("Done.\n")