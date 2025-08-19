# Experiment 1 Can near-infrared irradiation mitigate the negative effect on bumblebees  (thorax temperature)
library(tidyverse)    
library(lme4)        
library(lmerTest)    
library(readxl)
library(ggplot2)

df <-read_excel("../data/NIR.xlsx")

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

#Visualisation
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


df_850_none <- subset(df, Wavelength %in% c("850", "NA"))
df_660_none <- subset(df, Wavelength %in% c("660", "NA"))


ggplot(df_660_none, aes(x = TimePoint, y = Temperature, color = Group)) +
  geom_point(alpha = 0.6) +                   
  geom_smooth(method = "lm", se = TRUE,       
              alpha = 0.2) +
  theme_bw() +
  labs(
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



df3 <- df %>%
  filter(Group %in% c("C", "I", "N5", "N10", "N20")) %>%
  droplevels()

shapiro_results2 <- df3 %>%
  group_by(Group) %>%
  summarise(
    n = n(),
    W = shapiro.test(Temperature)$statistic,
    p_value = shapiro.test(Temperature)$p.value,
    .groups = "drop"
  )

print(shapiro_results2)

ggplot(df3, aes(x = Temperature)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  facet_wrap(~ Group, scales = "free_y") +
  theme_bw() +
  labs(
    title = "Histogram of Bumblebee Temperature by Group",
    x = "Temperature (°C)",
    y = "Count"
  )


ggplot(df3, aes(sample = Temperature)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Group) +
  theme_bw() +
  labs(
    title = "QQ Plot of Bumblebee Temperature by Group",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )


df$Group <- factor(df$Group, ordered = FALSE)
df$Group <- relevel(df$Group, ref = "I")


#fit models
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



#Experiment 2 Can food quality mitigate the negative effect on bumblebees (thorax temperature)

library(tidyverse)
library(readxl)
library(ggplot2)
library(lme4)
library(pbkrtest)

df<- read_excel("../data/food1R.xlsx")
df <- df %>%
  mutate(
    Group = factor(Group),
    conc = as.numeric(gsub("%", "", conc)),
    Colony = factor(Colony),
    time = as.character(time),
    Day = factor(Day),
    Food = as.numeric(Food)  
  )

df$conc <- df$conc * 100
df$conc <- as.factor(df$conc)


df_summary <- df %>%
  filter(Group %in% c("C", "I")) %>%
  group_by(Hour, Group, conc) %>%
  summarise(
    mean_temp = mean(Tem, na.rm = TRUE),
    sd = sd(Tem, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  )

df$Group <- factor(df$Group, levels = c("C", "I"))  


my_colors <- c("#fde725","#31688e")

ggplot(df_summary, aes(x = Hour, y = mean_temp, color = Group, group = Group)) +
  geom_line(size = 0.6) +
  geom_point(size = 0.8, alpha = 0.9) +
  geom_errorbar(aes(ymin = mean_temp - se, ymax = mean_temp + se), width = 0.8, alpha = 0.6) +
  facet_wrap(~conc, labeller = labeller(conc = c("30" = "30%", "40" = "40%", "50" = "50%"))) +
  scale_color_manual(values = my_colors) +
  labs(
    x = "Hour (Cumulative Measurement Time)",
    y = "Average Thorax Temperature (°C)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank()
  )

ggplot(df_summary, aes(x = Hour, y = mean_temp, color = Group, group = Group)) +
  geom_line(size = 0.6) +
  geom_point(size = 0.8, alpha = 0.9) +
  geom_errorbar(aes(ymin = mean_temp - se, ymax = mean_temp + se), width = 0.8, alpha = 0.6) +
  facet_wrap(~conc, 
             labeller = labeller(conc = c("30" = "30%", "40" = "40%", "50" = "50%")),
             ncol = 1
  ) +
  scale_color_manual(values = my_colors) +
  scale_x_continuous(
    breaks = seq(min(df_summary$Hour), max(df_summary$Hour), by = 4)
  ) +
  labs(
    x = "Hour (Cumulative Measurement Time)",
    y = "Average Thorax Temperature (°C)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank()
  )


ggplot(df_summary_group, aes(x = Group, y = mean_temp, fill = Group)) +
  geom_col(position = position_dodge(), width = 0.6, alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_temp - se, ymax = mean_temp + se), width = 0.2) +
  facet_wrap(~conc, labeller = labeller(conc = c("30" = "30%", "40" = "40%", "50" = "50%"))) +
  scale_fill_manual(values = c("C" = "#d95f02", "I" = "#1b9e77")) +
  labs(
    title = "Effect of Pesticide on Thorax Temperature at Different Sucrose Concentrations",
    x = "Treatment Group",
    y = "Average Thorax Temperature (°C)"
  ) +
  theme_minimal(base_size = 14)


se <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))

df_bar <- df %>%
  filter(Group %in% c("C", "I"), conc %in% c(30, 40, 50)) %>%
  group_by(Group, conc) %>%
  summarise(
    mean_food = mean(Food, na.rm = TRUE),
    se_food = se(Food),
    .groups = "drop"
  )

ggplot(df_bar, aes(x = factor(conc), y = mean_food, fill = Group)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(
    aes(ymin = mean_food - se_food, ymax = mean_food + se_food),
    position = position_dodge(width = 0.7), width = 0.2
  ) +
  scale_fill_manual(values = c("#fde725","#31688e")) + 
  labs(
    x = "Sucrose Concentration (%)",
    y = "Mean Food Consumption (g)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.title = element_blank()
  )


ggplot(df, aes(x = factor(Colony), y = Tem)) +
  geom_boxplot(fill = "#a6cee3", alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "#1f78b4", size = 1.5) +
  labs(
    title = "T vs Parental Colony",
    x = "Parental Colony",
    y = "Thorax Temperature (°C)"
  ) +
  theme_minimal(base_size = 14)

conc_num <- suppressWarnings(as.numeric(as.character(df$conc)))
df <- df %>%
  mutate(
    Group = factor(Group, levels = c("C","I")),
    conc_f = factor(
      conc_num,
      levels = c(30,40,50),
      labels = c("30%", "40%", "50%")
    )
  )


shapiro_by_group_conc <- df %>%
  group_by(Group, conc_f) %>%
  summarise(
    n = n(),
    W = unname(shapiro.test(Tem)$statistic),
    p_value = shapiro.test(Tem)$p.value,
    .groups = "drop"
  ) %>%
  arrange(conc_f, Group)
print(shapiro_by_group_conc)

ggplot(df, aes(x = Tem)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  facet_grid(Group ~ conc_f, scales = "free_y") +
  theme_bw() +
  labs(
    title = "Histogram of Thorax Temperature by Group × Concentration",
    x = "Thorax Temperature (°C)",
    y = "Count"
  )

ggplot(df, aes(sample = Tem)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(Group ~ conc_f) +
  theme_bw() +
  labs(
    title = "QQ Plot of Thorax Temperature by Group × Concentration",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )


df$Group <- factor(df$Group, levels = c("C", "I"))
df$conc <- factor(df$conc)
df$Colony <- factor(df$Colony)


#fit models (Thorax Temperature)
mod_temp <- lmer(Tem ~ conc * Group + Food + Hour+ (1 | Colony), data = df)
summary(mod_temp)

em <- emmeans(mod_temp, ~ Group | conc)
pairs(em)
emm <- emmeans(mod_temp, ~ conc | Group)
pairs(emm, adjust = "tukey")    


mod_temp_mass <- lmer(Tem ~ conc * Group + Food + Hour + Mass + (1 | Colony), data = df)
anova(mod_temp, mod_temp_mass)
summary(mod_temp_mass)

emx <- emmeans(mod_temp_mass, ~ Group | conc)
summary(emx)
pairs(emx)
emmx <- emmeans(mod_temp_mass, ~ conc | Group)
pairs(emmx, adjust = "tukey")    
emmxx<- emmeans(mod_temp_mass, ~ conc)
pairs(emmxx)




conc_num <- suppressWarnings(as.numeric(as.character(df$conc)))
df <- df %>%
  mutate(
    Group = factor(Group, levels = c("C","I")),
    conc_f = factor(
      conc_num,
      levels = c(30,40,50),
      labels = c("30%", "40%", "50%")
    )
  )

shapiro_food_by_group_conc <- df %>%
  group_by(Group, conc_f) %>%
  summarise(
    n = n(),
    W = unname(shapiro.test(Food)$statistic),
    p_value = shapiro.test(Food)$p.value,
    .groups = "drop"
  ) %>%
  arrange(conc_f, Group)
print(shapiro_food_by_group_conc)


ggplot(df, aes(x = Food)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  facet_grid(Group ~ conc_f, scales = "free_y") +
  theme_bw() +
  labs(
    title = "Histogram of Food Consumption by Group × Concentration",
    x = "Food Consumption (g)",
    y = "Count"
  )

ggplot(df, aes(sample = Food)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(Group ~ conc_f) +
  theme_bw() +
  labs(
    title = "QQ Plot of Food Consumption by Group × Concentration",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )


#fit models (food consumption)
mod_food <- lmer(Food ~ conc * Group + Hour + (1 | Colony), data = df)
summary(mod_food)

emm <- emmeans(mod_food, pairwise ~ Group | conc)
summary(emm$contrasts)


emm_conc <- emmeans(mod_food, pairwise ~ conc | Group)
summary(emm_conc$contrasts)



mod_foodmass <- lmer(Food ~ conc * Group + Hour + Mass+ (1 | Colony), data = df)
summary(mod_foodmass)

anova(mod_food,mod_foodmass)

emms <- emmeans(mod_foodmass, pairwise ~ Group | conc)
summary(emms$contrasts)


emm_concs <- emmeans(mod_foodmass, pairwise ~ conc | Group)
summary(emm_concs$contrasts)



dfrecover <- read_excel("../data/recoverdata.xlsx")
dfrecover$Concentration <- as.factor(dfrecover$Concentration)
str(dfrecover)

#fit models (recovery)
mod_recover <- lmer(Tem ~ Phase * Group + Concentration + Day + (1 | Colony), data = dfrecover)
summary(mod_recover)


emmc2 <- emmeans(mod_recover, ~ Phase | Concentration + Group)
summary(emmc2)
pairs(emmc2, adjust = "tukey")    

emmc3<- emmeans(mod_recover, ~ Group | Concentration)
summary(emmc3)
pairs(emmc3, adjust = "tukey")   

emmc4<- emmeans(mod_recover, ~ Concentration | Group)
summary(emmc4)
pairs(emmc4, adjust = "tukey")   

mod_recovermass <- lmer(Tem ~ Phase * Group + Concentration + Day +Mass+ (1 | Colony), data = dfrecover)
summary(mod_recovermass)

anova(mod_recovermass, mod_recover)

emmc2m <- emmeans(mod_recovermass, ~ Phase | Concentration + Group)
summary(emmc2m)
pairs(emmc2, adjust = "tukey")    

emmc3m<- emmeans(mod_recovermass, ~ Group | Concentration)
summary(emmc3m)
pairs(emmc3, adjust = "tukey")    

emmc4m<- emmeans(mod_recovermass, ~ Concentration | Group)
summary(emmc4m)
pairs(emmc4m, adjust = "tukey")   

recoverrate <- read_excel("../data/recoverrate.xlsx")

recoverrate$Concentration <- as.factor(recoverrate$Concentration)

modrate <- lmer(Rate ~ Group * Concentration +Food + Day + (1 | Colony), data = recoverrate)
summary(modrate)

emma <- emmeans(modrate, ~ Group | Concentration)
result <- pairs(emma, adjust = "tukey")
summary(result)


emmb <- emmeans(modrate, ~ Concentration | Group)
result2 <- pairs(emmb, adjust = "tukey")
summary(result2)


modratemass <- lmer(Rate ~ Group * Concentration +Food + Day+ Mass + (1 | Colony), data = recoverrate)
summary(modratemass)

anova(modrate, modratemass)


se <- function(x) sd(x, na.rm = TRUE)/sqrt(sum(!is.na(x)))

df_plot <- dfrecover %>%
  group_by(Phase, Group, Concentration) %>%
  summarise(
    mean_temp = mean(Tem, na.rm = TRUE),
    se_temp = se(Tem),
    .groups = "drop"
  )

facet_labels <- c("0.3" = "30%", "0.4" = "40%", "0.5" = "50%")
df_plot$Concentration <- as.character(df_plot$Concentration)

dfrecover$Phase <- factor(dfrecover$Phase, levels = c("T", "R"))
df_plot$Phase   <- factor(df_plot$Phase,   levels = c("T", "R"))

ggplot() +
  geom_jitter(
    data = dfrecover,
    aes(x = Phase, y = Tem, color = Group, shape = Group),
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.5),
    size = 1.5,
    alpha = 0.6,
    show.legend = FALSE
  ) +
  geom_point(
    data = df_plot,
    aes(x = Phase, y = mean_temp, color = Group, shape = Group),
    position = position_dodge(width = 0.5),
    size = 3
  ) +
  geom_errorbar(
    data = df_plot,
    aes(x = Phase, ymin = mean_temp - se_temp, ymax = mean_temp + se_temp, color = Group),
    width = 0.2,
    size = 1,
    position = position_dodge(width = 0.5)
  ) +
  scale_color_manual(values = c("#fde725","#31688e")) +
  scale_shape_manual(values = c(16, 17)) +
  facet_wrap(~ Concentration, labeller = as_labeller(facet_labels)) +
  labs(
    x = "Phase",
    y = "Mean Thorax Temperature (°C)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.title = element_blank(),
    strip.background = element_rect(fill = "transparent", color = "black"),
    strip.text = element_text(size = 13)
  )

make_conc_f <- function(x) {
  x_chr <- as.character(x)
  x_chr <- gsub("%", "", x_chr)
  x_num <- suppressWarnings(as.numeric(x_chr))
  x_num <- ifelse(x_num <= 1.5, x_num * 100, x_num)  # 0.3->30
  x_num <- round(x_num)
  factor(x_num, levels = c(30, 40, 50),
         labels = c("30%", "40%", "50%"))
}

df <- df %>% mutate(conc_f = make_conc_f(conc))
emm <- emmeans(mod_temp, ~ conc | Group)        
emm_df <- as.data.frame(emm) %>%
  mutate(conc_f = make_conc_f(conc))

ggplot() +
  geom_point(
    data = emm_df,
    aes(x = conc_f, y = emmean, color = Group, shape = Group),
    position = position_dodge(width = 0.5),
    size = 4.5
  ) +
  geom_errorbar(
    data = emm_df,
    aes(x = conc_f, ymin = emmean - SE, ymax = emmean + SE, color = Group),
    width = 0.2, size = 1,
    position = position_dodge(width = 0.5)
  ) +
  geom_jitter(
    data = df,
    aes(x = conc_f, y = Tem, color = Group, shape = Group),
    position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.5),
    size = 1.5, alpha = 0.5, show.legend = FALSE
  ) +
  scale_x_discrete(limits = c("30%", "40%", "50%")) +
  scale_color_manual(values = c("C" = "#fde725", "I" = "#31688e")) +
  scale_shape_manual(values = c("C" = 16, "I" = 17)) +
  facet_wrap(~ Group) +
  labs(x = "Food Concentration (%)", y = "Thorax Temperature (°C)") +
  theme_classic(base_size = 13) +
  theme(legend.title = element_blank(),
        strip.background = element_rect(fill = "transparent", color = "black"),
        strip.text = element_text(size = 13))


emm_df <- as.data.frame(emmeans(mod_food, ~ conc | Group))
emm_df$conc <- as.factor(emm_df$conc)
df$conc <- as.factor(df$conc)
levels(emm_df$conc) <- c("30%", "40%", "50%")
levels(df$conc) <- c("30%", "40%", "50%")

facet_labels <- c("C" = "C", "I" = "I")

ggplot() +
  geom_point(
    data = emm_df,
    aes(x = conc, y = emmean, color = Group, shape = Group),
    position = position_dodge(width = 0.5),
    size = 4.5
  ) +
  geom_errorbar(
    data = emm_df,
    aes(x = conc, ymin = emmean - SE, ymax = emmean + SE, color = Group),
    width = 0.2,
    size = 1,
    position = position_dodge(width = 0.5)
  ) +
  geom_jitter(
    data = df,
    aes(x = conc, y = Food, color = Group, shape = Group),
    position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.5),
    size = 1.5, alpha = 0.5, show.legend = FALSE
  ) +
  scale_color_manual(values = c("C" = "#fde725", "I" = "#31688e")) +
  scale_shape_manual(values = c("C" = 16, "I" = 17)) +
  facet_wrap(~ Group, labeller = as_labeller(facet_labels)) +
  labs(
    x = "Food Concentration",
    y = "Food Consumption (g)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.title = element_blank(),
    strip.background = element_rect(fill = "transparent", color = "black"),
    strip.text = element_text(size = 13)
  )




