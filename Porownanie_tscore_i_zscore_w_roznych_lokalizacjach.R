library(dplyr)
library(purrr)
library(ggpubr)
library(tidyr)
library(scales)
library(rstatix)
library(ggplot2)
library(ggpubr)
library(ggsignif)

plik_dane_do_analizy <- "PNP.RDS"

if (file.exists(plik_dane_do_analizy)) {
  PNP <- readRDS(plik_dane_do_analizy)
} else {
  stop(sprintf("Nie znaleziono pliku: `%s`. Uruchom notatnik przygotowujący zbiór danych!", plik_dane_do_analizy))
}


# Set global theme
theme_set(theme_classic2(base_size = 13))

PHPT <- PNP %>%
  filter(group != "R")


score <- bind_cols(
  PHPT %>%
    filter(group == 'B') %>%
    select(LUMBAR = Tscore_lumbar, FEMUR = Tscore_femur_neck, RADIAL = Tscore_radial) %>%
    pivot_longer(cols = everything(), names_to = "GRUPA", values_to = "Tscore"),
  
  PHPT %>%
    filter(group == 'B') %>%
    select(zscore_lumbar, zscore_femur_neck, zscore_radial) %>%
    pivot_longer(cols = everything(), names_to = NULL, values_to = "zscore") %>%
    select(zscore)
)


scoreK <- bind_cols(
  PHPT %>%
    filter(group == 'K') %>%
    select(LUMBAR = Tscore_lumbar, FEMUR = Tscore_femur_neck, RADIAL = Tscore_radial) %>%
    pivot_longer(cols = everything(), names_to = "GRUPA", values_to = "Tscore"),
  
  PHPT %>%
    filter(group == 'K') %>%
    select(zscore_lumbar, zscore_femur_neck, zscore_radial) %>%
    pivot_longer(cols = everything(), names_to = NULL, values_to = "zscore") %>%
    select(zscore)
)


# porowananie Tscore
# ANOVA
anova_result <- aov(Tscore ~ GRUPA, data = score)
summary(anova_result)

# Tukey HSD
tukey <- TukeyHSD(anova_result)
tukey_df <- as.data.frame(tukey$GRUPA)
tukey_df$comparison <- rownames(tukey_df)

# Przygotowanie danych do adnotacji
tukey_df <- tukey_df %>%
  separate(comparison, into = c("group1", "group2"), sep = "-") %>%
  mutate(p_label = ifelse(`p adj` < 0.001, "***",
                          ifelse(`p adj` < 0.01, "**",
                                 ifelse(`p adj` < 0.05, "*", "ns"))))

# Tworzymy wykres
ggplot(score, aes(x = GRUPA, y = Tscore, fill = GRUPA)) +
  geom_boxplot(width = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5, color = "black") +
  geom_signif(comparisons = list(c("LUMBAR", "FEMUR"),
                                 c("LUMBAR", "RADIAL"),
                                 c("FEMUR", "RADIAL")),
              annotations = tukey_df$p_label,
              y_position = c(2, 2.2, 2.4),
              tip_length = 0.03) +
  labs(title = "T-score comparisons",
       y = "T-score", x = NULL) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 12))


# Tworzymy wykres z wartościami p
ggplot(score, aes(x = GRUPA, y = Tscore, fill = GRUPA)) +
  geom_boxplot(width = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5, color = "black") +
  geom_signif(comparisons = list(c("LUMBAR", "FEMUR"),
                                 c("LUMBAR", "RADIAL"),
                                 c("FEMUR", "RADIAL")),
              annotations = tukey_df$padj,
              y_position = c(2, 2.2, 2.4),
              tip_length = 0.03) +
  labs(title = "T-score comparisons",
       y = "T-score", x = NULL) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 12))


# porownanie Zscore
# ANOVA
anova_result_z <- aov(zscore ~ GRUPA, data = score)
summary(anova_result_z)

# Tukey HSD
tukey_z <- TukeyHSD(anova_result_z)
tukey_df_z <- as.data.frame(tukey_z$GRUPA)
tukey_df_z$comparison <- rownames(tukey_df_z)

# Przygotowanie danych do adnotacji
tukey_df_z <- tukey_df_z %>%
  separate(comparison, into = c("group1", "group2"), sep = "-") %>%
  mutate(p_label = ifelse(`p adj` < 0.001, "***",
                          ifelse(`p adj` < 0.01, "**",
                                 ifelse(`p adj` < 0.05, "*", "ns"))))

# Tworzymy wykres
ggplot(score, aes(x = GRUPA, y = zscore, fill = GRUPA)) +
  geom_boxplot(width = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5, color = "black") +
  geom_signif(comparisons = list(c("LUMBAR", "FEMUR"),
                                 c("LUMBAR", "RADIAL"),
                                 c("FEMUR", "RADIAL")),
              annotations = tukey_df_z$p_label,
              y_position = c(3.6, 3.8, 4.2),
              tip_length = 0.03) +
  labs(title = "Z-score comparisons",
       y = "Z-score", x = NULL) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 12))


# ========================
# porownanie T-score i z-score miedzy lokalizacjami w grupie kontrolnej
# porowananie Tscore
# ANOVA
anova_result_K <- aov(Tscore ~ GRUPA, data = scoreK)
summary(anova_result_K)

# Tworzymy wykres
ggplot(score, aes(x = GRUPA, y = Tscore, fill = GRUPA)) +
  geom_boxplot(width = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5, color = "black") +
  labs(title = "T-score comparisons",
       y = "T-score", x = NULL) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 12))


# porownanie Zscore
# ANOVA
anova_result_z <- aov(zscore ~ GRUPA, data = score)
summary(anova_result_z)


# Tworzymy wykres
ggplot(score, aes(x = GRUPA, y = zscore, fill = GRUPA)) +
  geom_boxplot(width = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5, color = "black") +
  labs(title = "Z-score comparisons",
       y = "Z-score", x = NULL) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 12))



# analiza w modelu logistycznym

analiza <- lm(BMD_neck ~ group + sex, PHPT)
summary(analiza)

analiza2 <- lm(BMD_neck ~ group + sex + age + BMI, PHPT)
summary(analiza2)

analiza3 <- lm(BMD_neck ~ group + sex + BMI, PHPT)
summary(analiza3)



