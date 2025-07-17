library(tidyverse)
library(readxl)
library(writexl)
library(psych)
library(lavaan)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(modelr)
library(broom)


PNP <- read_delim("PNP_densytometria_10_04_25.csv", delim = ";", 
                  escape_double = FALSE,locale = locale(decimal_mark = "."), trim_ws = TRUE)

# Usunięcie wierszy z brakami danych (więcej niż 20 braków w danych)
PNP <- PNP %>%
  filter(rowSums(is.na(.)) <= 20)

# Usunięcie kolumn z brakami danych
PNP <- PNP %>%
  select(where(~ !all(is.na(.))))


# Formatowanie wartości w kolumnach:
# stary sposób, kazda kolumna oddzielnie: # PNP$birthdate <- as.Date(PNP$birthdate, format = "%d.%m.%Y")
# nowy sposb, wszystkie kolumny z datami razem:
PNP[c("birthdate", "hosp_date", "DXA_date")] <- lapply(PNP[c("birthdate", "hosp_date", "DXA_date")], as.Date, format = "%d.%m.%Y")

# sprawdzenie czy udało się zmienić wartości na daty:
sapply(PNP$birthdate, inherits, what = "Date")

# nowa wersja przy pomocy pakietu tidyvierse:
PNP <- PNP %>%
  mutate(across(
    where(~ is.character(.) && all(grepl("^[-]?[0-9]+(\\.[0-9]+)?$", .[!is.na(.)]))),
    ~ as.numeric(gsub("−", "-", .))
  ))


# sprawdzenie czy udało się zmienić wartości na cyfry:
sapply(PNP$HF_risk, is.numeric)
sapply(PNP$Tscore_femur_neck, is.numeric)
sapply(PNP$Tscore_femur_total, is.numeric)


PC <- read_delim("PC_porownanie.csv", delim = ";", 
                 escape_double = FALSE,locale = locale(decimal_mark = "."), trim_ws = TRUE)

# Usunięcie wierszy z brakami danych (więcej niż 20 braków w danych)
PC <- PC %>%
  filter(rowSums(is.na(.)) <= 45)

PC <- PC %>%
  mutate(across(where(~ all(grepl("^[0-9.]+$", .[!is.na(.)]))), as.numeric))

PC[c("birthdate", "hosp_date", "DXA_date")] <- lapply(PC[c("birthdate", "hosp_date", "DXA_date")], as.Date, format = "%d.%m.%Y")

# zmiana zmiennych na typ as.character w zbiorze PC jak w zbiorze PNP, zeby bylo mozliwe polaczenie zbiorow:
# stary sposob, wszystkie kolumny pokolei:
# PC <- PC %>%
#  mutate(fracture = as.character(fracture))

PC <- PC %>%
  mutate(across(c("fracture", "parent_hip_fracture", "nicotine_dependence", "GC", "RA", "DXA_result", "VFA", "symptoms", "OFC_or_bone_pain", "nephrolithiasis", "neuromuscular_symptoms", "bisfosfonian_treatment"), as.character))

PNP <- bind_rows(PNP, PC)


PNP <- PNP %>%
  mutate(log_iPTH = log(iPTH),
         log_Ca = log(Ca),
         log_BMD = log(BMD_neck),
         log_MOF = log(MOF_risk),
         log_HF = log(HF_risk))

PNP <- PNP %>%
  mutate(age_split = case_when (age > 50 ~ "postM",
                                age < 50 ~ "preM"))


# Zapisz lokalnie zbiór danych przeznaczony do analizy danych dotyczących PNP
plik_dane_do_analizy <- "PNP.RDS"
saveRDS(PNP, plik_dane_do_analizy)
