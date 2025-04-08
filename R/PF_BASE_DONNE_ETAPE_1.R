library(tidyverse)
library(dplyr)
library(writexl)
library(stringr)
library(haven)
library(readxl)

# Étape 1: créer le base de données générales

# Avant de modifier le lien du fichier .sav, il faut mettre à jour la liste
# Étape 1: Supprimer le doublon F001_PD (2e à partir du bas)
# Étape 2: Supprimer le doublon F012AVG (garder celui en minuscule)
# Étape 3: Prendre l'ancienne version et copier coller les numéros de famille.

# Importer les résultats de Monkey Survey

df_smonkey <- read_sav("raw_data/questionnaires_temps_1_20250128.sav")

# Renommer les variables

df_smonkey <- df_smonkey %>%
  rename(id = q0005) %>%
  rename(vague = q0006) %>%
  rename_with(~str_replace(.x, "^q0007", "ees"), starts_with("q0007")) %>%
  rename_with(~str_replace(.x, "^q0008", "webwbs"), starts_with("q0008")) %>%
  rename_with(~str_replace(.x, "^q0009", "k6"), starts_with("q0009")) %>%
  rename_with(~str_replace(.x, "^q0010", "isp"), starts_with("q0010")) %>%
  rename_with(~str_replace(.x, "^q0034", "eqcc"), starts_with("q0034")) %>%
  rename_with(~str_replace(.x, "^q0035", "chaos"), starts_with("q0035")) %>%
  rename_with(~str_replace(.x, "^q0036", "gf6"), starts_with("q0036")) %>%
  rename_with(~str_replace(.x, "^q0037", "satisfaction"), starts_with("q0037")) %>%
  rename(isp_0012_B = q0011, sommeil_1 = q0012, sommeil_2 = q0013, sommeil_3 = q0014, sommeil_4 = q0015, sommeil_5 = q0016_0001, sommeil_6 = q0017, sommeil_7 = q0018) %>%
  rename(besoin_1 = q0019_0001, besoin_1_1 = q0020_0001, besoin_1_2 = q0021_0001,
         besoin_2 = q0022_0001, besoin_2_1 = q0023_0001, besoin_2_2 = q0024_0001,
         besoin_3 = q0025_0001, besoin_3_1 = q0026_0001,besoin_3_2 = q0027_0001,
         besoin_4 = q0028_0001, besoin_4_1 = q0029_0001, besoin_4_2 = q0030_0001,
         besoin_5 = q0031_0001, besoin_5_1 = q0032_0001, besoin_5_2 = q0033_0001) %>%
  rename(soutien_1 = q0038_0001, soutien_1_1 = q0039_0001,
         soutien_2 = q0040_0001, soutien_2_1 = q0041_0001,
         soutien_3 = q0042_0001, soutien_3_1 = q0043_0001,
         soutien_4 = q0044_0001, soutien_4_1 = q0045_0001,
         soutien_5 = q0046_0001, soutien_5_1 = q0047_0001,
         soutien_6 = q0048_0001, soutien_6_1 = q0050_0001, soutien_6_2 = q0049) %>%
  rename(conso_grossesse_tabac = q0051_0001, conso_grossesse_alcool = q0051_0002,
         conso_grossesse_cannabis = q0051_0003, conso_grossesse_autre = q0051_0004,
         conso_12mois_alcool = q0052_0001, conso_12mois_cannabis = q0052_0002,
         conso_12mois_autre = q0052_0003) %>%
  mutate(id = if_else(id == "F021", "F021_JSV", id)) # a être supprimé quand modification fait dans surveymonkey


# Calcul des scores pour les échelles de mesures

# Fonction pour inversé les items ISP. Le formulaire mettait un score inversé. Le score est crée avec la question 12 manquante.

reverse_item_isp <- function(x, max_score = 5) {
  return(max_score + 1 - x)
}
items_to_reverse_isp <- c("isp_0001", "isp_0002", "isp_0003", "isp_0004", "isp_0005", "isp_0006", "isp_0007", "isp_0008", "isp_0009", "isp_0015", "isp_0016")

df_smonkey <- df_smonkey %>%
  mutate(across(all_of(items_to_reverse_isp), reverse_item_isp, .names = "reversed_{.col}"))

df_smonkey <- df_smonkey %>%
  rowwise() %>%
  mutate(isp_score = sum(c_across(starts_with("reversed_isp")))) %>%
  ungroup()

# Fonction pour inversé les items CHAOS

reverse_item_chaos <- function(x, max_score = 5) {
  return(max_score + 1 - x)
}

items_to_reverse_chaos <- c("chaos_0001", "chaos_0004", "chaos_0006")

df_smonkey <- df_smonkey %>%
  mutate(across(all_of(items_to_reverse_chaos), reverse_item_chaos, .names = "reversed_{.col}"))

df_smonkey <- df_smonkey %>%
  rowwise() %>%
  mutate(ees_score = sum(c_across(c("chaos_0002", "chaos_0003", "chaos_0005")),
                         c_across(starts_with("reversed_chaos")))) %>%
  ungroup()


# Fonction pour inversé les items EES

reverse_item <- function(x, max_score = 4) {
  return(max_score + 1 - x)
}

items_to_reverse <- c("ees_0003", "ees_0005", "ees_0008", "ees_0009", "ees_0010")

df_smonkey <- df_smonkey %>%
  mutate(across(all_of(items_to_reverse), reverse_item, .names = "reversed_{.col}"))

df_smonkey <- df_smonkey %>%
  rowwise() %>%
  mutate(ees_score = sum(c_across(c("ees_0001", "ees_0002", "ees_0004", "ees_0006", "ees_0007")),
                         c_across(starts_with("reversed_ees")))) %>%
  ungroup()

# Creation DF pour conversion

raw_scores <- 7:35

metric_scores <- c(
  7.00, 9.51, 11.25, 12.40, 13.33, 14.08, 14.75, 15.32, 15.84, 16.36,
  16.88, 17.43, 17.98, 18.59, 19.25, 19.98, 20.73, 21.54, 22.35, 23.21,
  24.11, 25.03, 26.02, 27.03, 28.13, 29.31, 30.70, 32.55, 35.00
)

conversion_table <- data.frame(Raw_Score = raw_scores, Metric_Score = metric_scores)

# Ajouter WEMWBS

df_smonkey <- df_smonkey %>%
  rowwise() %>%
  mutate(webwbs_score_raw = sum(c_across(starts_with("webwbs_")))) %>%
  ungroup()

df_smonkey <- df_smonkey %>%
  left_join(conversion_table, by = c("webwbs_score_raw" = "Raw_Score")) %>%
  mutate(webwbs_metric_score = Metric_Score) %>%
  select(-Metric_Score)

# Ajouter le K6, ISP, EQCC, CHAOS et GF6

df_smonkey <- df_smonkey %>%
  rowwise() %>%
  mutate(k6_score = sum(c_across(starts_with("k6_")))) %>%
  mutate(eqcc_score = sum(c_across(starts_with("eqcc")))) %>%
  mutate(chaos_score = sum(reversed_chaos_0001, chaos_0002, chaos_0003, reversed_chaos_0004, chaos_0005, reversed_chaos_0006)) %>%
  mutate(gf6_score = mean(c_across(starts_with("gf6")))) %>%
  ungroup()

# Importer et merger les données sociodémographique


df_socio <- read_excel("raw_data/PF_base_de_donnees_sociodémographique_T1_20250225.xlsx", sheet = "Questionnaire_sociodémographiqu") %>%
  filter(vague == 1)

df_general <- left_join(df_socio, df_smonkey, by = "id")

write_xlsx(df_general, "data_base/PF_base_de_donnees_T1.xlsx")

