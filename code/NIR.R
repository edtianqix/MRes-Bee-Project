
library(tidyverse)    
library(lme4)        
library(lmerTest)    


library(readxl)
df <- read_excel("Desktop/Master project/Pilot 1 wavelength and duration/pilot1try.xlsx")



df$food <- as.numeric(df$food)
df$HourNumeric <- as.numeric(gsub("\\D", "", df$Hour))
df$Wavelength[is.na(df$Wavelength)] <- "None"
df$TimePoint <- (df$Day - 1)*6 + df$HourNumeric

head(df)
str(df)

table(df$Group)
table(df$Day)
table(df$HourNumeric)
table(df$Wavelength)

summary(df$Temperature)


wavelength_labels <- c(`660` = "660nm", `850` = "850nm", `NA` = "Without NIR")

ggplot(df, aes(x = TimePoint, y = Temperature, color = Group)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  facet_wrap(~ Wavelength, labeller = as_labeller(wavelength_labels)) +
  theme_bw() +
  labs(
    x = "Time",
    y = "Temperature (°C)"
  ) +
  theme(
    strip.background = element_rect(fill = "transparent", color = NA), 
    panel.background = element_rect(fill = "transparent", color = NA), 
    plot.background = element_rect(fill = "transparent", color = NA)    
  )


shapiro.test(df$Temperature)
ggplot(df, aes(x = Temperature)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  theme_bw() +
  labs(
    title = "Histogram of Bumblebee Temperature",
    x = "Temperature (°C)",
    y = "Count"
  )

tapply(df$Temperature, df$Group, shapiro.test)

df$Group <- factor(df$Group, ordered = FALSE)
df$Group <- relevel(df$Group, ref = "I")


modelcolony <- lmer(
  Temperature ~ Group + Wavelength + TimePoint +food+
    (1 | Colony),
  data = df
)
summary(modelcolony)


modelnomass <- lmer(
  Temperature ~ Group + Wavelength + TimePoint +food+ Mass+
    (1 | Colony),
  data = df
)
summary(modelnomass)

anova(modelcolony, modelnomass)

df %>%
  group_by(Group) %>%
  summarise(
    mean = mean(Temperature, na.rm = TRUE),
    se = sd(Temperature, na.rm = TRUE) / sqrt(n())
  )


emm <- emmeans(modelnomass, pairwise ~ Group)
summary(emm$contrasts)
pairs(emm)

df_850_none <- subset(df, Wavelength %in% c("850", "NA"))
df_660_none <- subset(df, Wavelength %in% c("660", "NA"))


ggplot(df_660_none, aes(x = TimePoint, y = Temperature, color = Group)) +
  geom_point(alpha = 0.6) +                   
  geom_smooth(method = "lm", se = TRUE,       
              alpha = 0.2) +
  theme_bw() +
  labs(
    title = "Temperature Over Time (C, I, N5) at Wavelength = 660",
    x = "TimePoint",
    y = "Temperature (°C)"
  )



df_660N5 <- subset(df, Wavelength == "660" & Group %in% c("C", "I", "N5"))

df_850_none$Group <- factor(df_850_none$Group, levels = c("C", "I", "N5", "N10", "N20"))

df_660_none$Group <- factor(df_660_none$Group, levels = c("C", "I", "N5", "N10", "N20"))


p1 <- ggplot(df_850_none, aes(x = Group, y = food, fill = Group)) +
  geom_boxplot(outlier.size = 1.8, outlier.shape = 16, width = 0.7) +
  scale_fill_manual(values = my_colors) +
  labs(
    title = "850nm",
    x = "Treatment Groups",
    y = "Food Consumption per Day (g)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    legend.position = "none",
    panel.grid = element_blank()
  )

p2 <- ggplot(df_660_none, aes(x = Group, y = food, fill = Group)) +
  geom_boxplot(outlier.size = 1.8, outlier.shape = 16, width = 0.7) +
  scale_fill_manual(values = my_colors) +
  labs(
    title = "660nm",
    x = "Treatment Groups",
    y = "Food Consumption per Day (g)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    legend.position = "none",
    panel.grid = element_blank()
  )

library(patchwork)
p1 / p2


df$Group <- factor(df$Group, levels = c("C", "I", "N5", "N10", "N20"))

wavelength_labels <- c(
  "660" = "660nm with pesticide",
  "850" = "850nm with pesticide",
  "NA"  = "Without NIR"
)


my_colors <- c(
  "C" = "#fde725",     
  "I" = "#31688e",     
  "N10" = "#35b779",   
  "N20" = "#1fa187",
  "N5"  = "#b8de29"
)

ggplot(df, aes(x = TimePoint, y = Temperature, color = Group)) +
  geom_point(alpha = 0.6, size = 1, position = position_jitter(width = 0.2, height = 0)) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, size = 0.8, aes(group = Group)) +
  facet_wrap(~ Wavelength, labeller = as_labeller(wavelength_labels)) +
  scale_color_manual(values = my_colors) +      # <--- 这里换成你自定义颜色
  theme_bw() +
  labs(
    x = "Time",
    y = "Temperature (°C)"
  ) +
  theme(
    strip.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 8),
    strip.text = element_text(size = 10),
    legend.text = element_text(size = 10)
  )





