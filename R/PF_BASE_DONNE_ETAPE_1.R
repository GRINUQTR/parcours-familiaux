library(tidyverse)
library(writexl)
library(haven)
library(readxl)

# Importation des données
df_smonkey_t1 <- read_sav("raw_data/questionnaires_temps_1_20250128.sav")
df_smonkey_t2 <- read_sav("raw_data/questionnaires_temps_2_20250408.sav")

# Fonction de renommage en lot
rename_by_prefix <- function(data, prefix, new_prefix) {
  rename_with(data, ~str_replace(.x, paste0("^", prefix), new_prefix), starts_with(prefix))
}

# Fonction d'inversion
reverse_items <- function(df, items, max_score, prefix = "reversed_") {
  df %>%
    mutate(across(all_of(items), ~ max_score + 1 - ., .names = paste0(prefix, "{.col}")))
}

# Nettoyage de la base de données T1

# Renommage des variables t1 / selection des variables
df_smonkey_t1 <- df_smonkey_t1 %>%
  rename(id = q0005, vague = q0006) %>%
  rename_by_prefix("q0007", "ees") %>%
  rename_by_prefix("q0008", "webwbs") %>%
  rename_by_prefix("q0009", "k6") %>%
  rename_by_prefix("q0010", "isp") %>%
  rename_by_prefix("q0034", "eqcc") %>%
  rename_by_prefix("q0035", "chaos") %>%
  rename_by_prefix("q0036", "gf6") %>%
  rename_by_prefix("q0037", "satisfaction") %>%
  rename(
    isp_0012_B = q0011,
    sommeil_1 = q0012, sommeil_2 = q0013, sommeil_3 = q0014, sommeil_4 = q0015,
    sommeil_5 = q0016_0001, sommeil_6 = q0017, sommeil_7 = q0018,
    besoin_1 = q0019_0001, besoin_1_1 = q0020_0001, besoin_1_2 = q0021_0001,
    besoin_2 = q0022_0001, besoin_2_1 = q0023_0001, besoin_2_2 = q0024_0001,
    besoin_3 = q0025_0001, besoin_3_1 = q0026_0001, besoin_3_2 = q0027_0001,
    besoin_4 = q0028_0001, besoin_4_1 = q0029_0001, besoin_4_2 = q0030_0001,
    besoin_5 = q0031_0001, besoin_5_1 = q0032_0001, besoin_5_2 = q0033_0001,
    soutien_1 = q0038_0001, soutien_1_1 = q0039_0001,
    soutien_2 = q0040_0001, soutien_2_1 = q0041_0001,
    soutien_3 = q0042_0001, soutien_3_1 = q0043_0001,
    soutien_4 = q0044_0001, soutien_4_1 = q0045_0001,
    soutien_5 = q0046_0001, soutien_5_1 = q0047_0001,
    soutien_6 = q0048_0001, soutien_6_1 = q0050_0001, soutien_6_2 = q0049,
    conso_grossesse_tabac = q0051_0001, conso_grossesse_alcool = q0051_0002,
    conso_grossesse_cannabis = q0051_0003, conso_grossesse_autre = q0051_0004,
    conso_12mois_alcool = q0052_0001, conso_12mois_cannabis = q0052_0002,
    conso_12mois_autre = q0052_0003, date = StartDate) %>%
  mutate(date = mdy_hms(date)) %>%
  mutate(date = as_date(date)) %>%
  select(
    date, id, vague, isp_0012_B,
    starts_with("ees_"), starts_with("webwbs_"), starts_with("k6_"), starts_with("isp_"),
    starts_with("eqcc_"), starts_with("chaos_"), starts_with("gf6_"), starts_with("satisfaction_"),
    starts_with("sommeil_"), starts_with("besoin_"), starts_with("soutien_"), starts_with("conso_"))

# Gestion des données manquantes
# Remplace les -9 par des NA
df_smonkey_t1 <- df_smonkey_t1 %>%
  mutate(across(where(is.numeric), ~na_if(., -9)))

# DF des données manquantes
df_smonkey_t1_na <- tibble(
  column = names(df_smonkey_t1),
  n_na = colSums(is.na(df_smonkey_t1)))

# Imputation sur la moyenne pour EES (3 participants avec un items NA)

rosenberg_items <- paste0("ees_00", sprintf("%02d", 1:10))

df_smonkey_t1 <- df_smonkey_t1 %>%
  rowwise() %>%
  mutate(
    across(all_of(rosenberg_items), ~ ifelse(is.na(.),
                                             round(mean(c_across(all_of(rosenberg_items)), na.rm = TRUE)),
                                             .))) %>%
  ungroup()

# Imputatation sur la moyenne pour Isop_007 (3 données manquantes) et isp_0012 (Questions manquantes pour F001 à F012)
# Inversion des questions / Imputation isp_007 / imputation isp_0012_b
df_smonkey_t1 <- df_smonkey_t1 %>%
  reverse_items(c("isp_0001", "isp_0002", "isp_0003", "isp_0004", "isp_0005",
                  "isp_0006", "isp_0007", "isp_0008", "isp_0009", "isp_0015", "isp_0016"), 5) %>%
  rowwise() %>%
  mutate(reversed_isp_0007 = ifelse(
    is.na(reversed_isp_0007),
    round(mean(c_across(c("reversed_isp_0001", "reversed_isp_0002", "reversed_isp_0003",
                          "reversed_isp_0004", "reversed_isp_0005", "reversed_isp_0006",
                          "reversed_isp_0008", "reversed_isp_0009",
                          "reversed_isp_0015", "reversed_isp_0016")), na.rm = TRUE)), reversed_isp_0007),
    isp_0012_B = ifelse(is.na(isp_0012_B),
                             round(mean(c_across(starts_with("reversed_isp")), na.rm = TRUE)), isp_0012_B)) %>%
  ungroup()

# Inverser CHAOS et calculer le score
df_smonkey_t1 <- df_smonkey_t1 %>%
  reverse_items(c("chaos_0001", "chaos_0004", "chaos_0006"), 5) %>%
  rowwise() %>%
  mutate(chaos_score = sum(c_across(all_of(c("chaos_0002", "chaos_0003", "chaos_0005",
                                             "reversed_chaos_0001", "reversed_chaos_0004", "reversed_chaos_0006"))))) %>%
  ungroup()

# Inverser EES et calculer le score
df_smonkey_t1 <- df_smonkey_t1 %>%
  reverse_items(c("ees_0003", "ees_0005", "ees_0008", "ees_0009", "ees_0010"), 4) %>%
  rowwise() %>%
  mutate(
    ees_score = sum(c_across(all_of(c("ees_0001", "ees_0002", "ees_0004", "ees_0006", "ees_0007",
                                      "reversed_ees_0003", "reversed_ees_0005", "reversed_ees_0008",
                                      "reversed_ees_0009", "reversed_ees_0010"))))) %>%
  ungroup()

# Conversion WEMWBS
conversion_table <- data.frame(
  Raw_Score = 7:35,
  Metric_Score = c(7.00, 9.51, 11.25, 12.40, 13.33, 14.08, 14.75, 15.32, 15.84,
                   16.36, 16.88, 17.43, 17.98, 18.59, 19.25, 19.98, 20.73, 21.54,
                   22.35, 23.21, 24.11, 25.03, 26.02, 27.03, 28.13, 29.31, 30.70,
                   32.55, 35.00)
)

df_smonkey_t1 <- df_smonkey_t1 %>%
  rowwise() %>%
  mutate(webwbs_score_raw = sum(c_across(starts_with("webwbs_")))) %>%
  ungroup() %>%
  left_join(conversion_table, by = c("webwbs_score_raw" = "Raw_Score")) %>%
  rename(webwbs_metric_score = Metric_Score)

# Score K6, EQCC, GF6 et ISP
df_smonkey_t1 <- df_smonkey_t1 %>%
  rowwise() %>%
  mutate(
    k6_score = sum(c_across(starts_with("k6_")), na.rm = TRUE),
    eqcc_score = sum(c_across(starts_with("eqcc")), na.rm = TRUE),
    gf6_score = mean(c_across(starts_with("gf6")), na.rm = TRUE),
    isp_score = sum(c(c_across(starts_with("reversed_isp")), isp_0012_B), na.rm = TRUE)) %>%
  ungroup()


# Base de données des scores

df_smonkey_t1_score <- df_smonkey_t1 %>%
  select(date, id, vague, k6_score, eqcc_score, gf6_score, ees_score, webwbs_score_raw, webwbs_metric_score, chaos_score, isp_score)

# Vérifier des données abérrantes

vars <- c("k6_score", "eqcc_score", "gf6_score", "ees_score",
          "webwbs_score_raw", "webwbs_metric_score", "chaos_score", "isp_score")

df_outliers_t1 <- df_smonkey_t1_score

# Créer une liste vide pour mettre les informations en format long
outlier_list <- list()

# Appliquer en boucle à chaque variables
for (var in vars) {

  # Définir les IQR
  Q1 <- quantile(df_outliers_t1[[var]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df_outliers_t1[[var]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val

  # Définior le Z-score
  mean_val <- mean(df_outliers_t1[[var]], na.rm = TRUE)
  sd_val <- sd(df_outliers_t1[[var]], na.rm = TRUE)

  # Définir les limites
  df_outliers_t1 <- df_outliers_t1 %>%
    mutate(
      "{var}_zscore" := (.data[[var]] - mean_val) / sd_val,
      "{var}_outlier_iqr" := .data[[var]] < lower_bound | .data[[var]] > upper_bound,
      "{var}_outlier_z" := abs(.data[[paste0(var, "_zscore")]]) > 3
    )

  # calcul IQR
  df_outliers_t1 <- df_outliers_t1 %>%
    mutate(
      "{var}_iqr_score" := case_when(
        .data[[var]] < Q1 ~ (Q1 - .data[[var]]) / IQR_val,
        .data[[var]] > Q3 ~ (.data[[var]] - Q3) / IQR_val,
        TRUE ~ 0
      )
    )

  # IQR Données abérrantes
  outlier_iqr <- df_outliers_t1 %>%
    filter(.data[[paste0(var, "_outlier_iqr")]]) %>%
    select(
      id,
      value = all_of(var),
      iqr_score = all_of(paste0(var, "_iqr_score")),
      zscore = all_of(paste0(var, "_zscore"))
    ) %>%
    mutate(variable = var, method = "IQR")

  # Z-score Données abérrantes
  outlier_z <- df_outliers_t1 %>%
    filter(.data[[paste0(var, "_outlier_z")]]) %>%
    select(
      id,
      value = all_of(var),
      iqr_score = all_of(paste0(var, "_iqr_score")),
      zscore = all_of(paste0(var, "_zscore"))
    ) %>%
    mutate(variable = var, method = "Z-score")

  # Combiner les données
  outlier_list[[var]] <- bind_rows(outlier_iqr, outlier_z)
}

# Combiner dans un dataframe
outlier_summary <- bind_rows(outlier_list)



# Renommage des variables T2

df_smonkey_t2 <- df_smonkey_t2 %>%
  rename(id = q0001, vague = q0002, isp_0012 = q0006) %>%
  rename_by_prefix("q0003", "k6") %>%
  rename_by_prefix("q0004", "temp") %>%
  rename_by_prefix("q0005", "isp") %>%
  rename_by_prefix("q0007", "tas") %>%
  rename_by_prefix("q0008", "ecc") %>%
  rename_by_prefix("q0009", "ctq") %>%
  rename_by_prefix("q0010", "pce") %>%
  rename_by_prefix("q0037", "satisfaction")

# Gestion des données manquantes
# Remplace les -9 par des NA
df_smonkey_t2 <- df_smonkey_t2 %>%
  mutate(across(where(is.numeric), ~na_if(., -9)))

# DF des données manquantes
df_smonkey_t1_na <- tibble(
  column = names(df_smonkey_t1),
  n_na = colSums(is.na(df_smonkey_t1)))

# Score K6, ECC et PCE
# Vérifier si il y a des 7 dans PCE

df_has_7 <- df_smonkey_t2 %>%
  filter(if_any(starts_with("pce_"), ~ .x == 7)) %>%
  select(id, starts_with("pce_"))

df_smonkey_t2 <- df_smonkey_t2 %>%
  rowwise() %>%
  mutate(
    k6_score = sum(c_across(starts_with("k6_"))),
    ecc_score = sum(c_across(starts_with("ecc_"))),
    pce_score = sum(c_across(starts_with("pce_")))
  ) %>%
  ungroup()

# Score pour les facteurs de tempérament

df_smonkey_t2 <- df_smonkey_t2 %>%
  reverse_items(c("temp_0001", "temp_0002", "temp_0003", "temp_0004",
                  "temp_0007", "temp_0010", "temp_0013", "temp_0019"), 7) %>%
  rowwise() %>%
  mutate(temp_ac_score = sum(reversed_temp_0001, reversed_temp_0002, temp_0005, temp_0009,
                             temp_0014, temp_0015, reversed_temp_0019) / 7,
         temp_sad_score = sum(reversed_temp_0003, reversed_temp_0007, temp_0008, temp_0012,
                              temp_0016, temp_0017, reversed_temp_0010) / 7,
         temp_soc_score = sum(reversed_temp_0004, reversed_temp_0013, temp_0006, temp_0011,
                              temp_0018) / 5) %>%
  ungroup()

# Inverser les items ISP et calculer le score
df_smonkey_t2 <- df_smonkey_t2 %>%
  reverse_items(c("isp_0001", "isp_0002", "isp_0003", "isp_0004", "isp_0005",
                  "isp_0006", "isp_0007", "isp_0008", "isp_0009", "isp_0010", "isp_0011"), 5) %>%
  rowwise() %>%
  mutate(isp_score = sum(c_across(starts_with("reversed_isp")), isp_0012)) %>%
  ungroup()

# Score pour les facteurs de tempérament

df_smonkey_t2 <- df_smonkey_t2 %>%
  reverse_items(c("tas_0004", "tas_0005", "tas_0010", "tas_0018", "tas_0019"), 5) %>%
  rowwise() %>%
  mutate(tas_dif_score = sum(tas_0001, tas_0003, tas_0006, tas_0007,
                             tas_0009, tas_0013, tas_0014),
         tas_ddf_score = sum(reversed_tas_0004, tas_0002, tas_0011, tas_0012,
                              tas_0017),
         tas_eot_score = sum(reversed_tas_0005, reversed_tas_0010, reversed_tas_0018,
                              reversed_tas_0019, tas_0008, tas_0015, tas_0016, tas_0020),
         tas_total_score = sum(tas_0001, tas_0003, tas_0006, tas_0007,
                                tas_0009, tas_0013, tas_0014, reversed_tas_0004, tas_0002, tas_0011, tas_0012,
                                tas_0017, reversed_tas_0005, reversed_tas_0010, reversed_tas_0018,
                                reversed_tas_0019, tas_0008, tas_0015, tas_0016, tas_0020)) %>%
  ungroup()

# Score pour les facteurs de CTQ

df_smonkey_t2 <- df_smonkey_t2 %>%
  reverse_items(c("ctq_0002", "ctq_0005", "ctq_0007",
                  "ctq_0013", "ctq_0019", "ctq_0026", "ctq_0028"), 5) %>%
  rowwise() %>%
  mutate(
    ctq_emotional_abuse = sum(ctq_0003, ctq_0008, ctq_0014, ctq_0018, ctq_0025),
    ctq_physical_abuse  = sum(ctq_0009, ctq_0011, ctq_0012, ctq_0015, ctq_0017),
    ctq_sexual_abuse    = sum(ctq_0020, ctq_0021, ctq_0023, ctq_0024, ctq_0027),
    ctq_emotional_neglect = sum(reversed_ctq_0005, reversed_ctq_0007,
                                reversed_ctq_0013, reversed_ctq_0019, reversed_ctq_0028),
    ctq_physical_neglect = sum(ctq_0001, reversed_ctq_0002, ctq_0004,
                               ctq_0006, reversed_ctq_0026),
    ctq_total_score = sum(ctq_emotional_abuse, ctq_physical_abuse, ctq_sexual_abuse,
                          ctq_emotional_neglect, ctq_physical_neglect)
  ) %>%
  ungroup()

# Créer un df avec les scores

df_smonkey_t2_score <- df_smonkey_t2 %>%
  select(id, vague, k6_score, ecc_score, pce_score, isp_score, ctq_emotional_abuse, ctq_physical_abuse, ctq_sexual_abuse,
         ctq_emotional_neglect, ctq_physical_neglect, ctq_total_score, tas_dif_score, tas_ddf_score, tas_eot_score,
         tas_total_score, temp_ac_score, temp_sad_score,temp_soc_score)

# Merge des bases de données avec score

df_smonkey_t1_t2_score <- bind_rows(df_smonkey_t1_score, df_smonkey_t2_score)

# Importer et merger les données sociodémographique


df_socio <- read_excel("raw_data/PF_base_de_donnees_sociodémographique_T1_20250225.xlsx", sheet = "Questionnaire_sociodémographiqu") %>%
  filter(vague == 1)

df_general <- left_join(df_socio, df_smonkey, by = "id")

write_xlsx(df_general, "data_base/PF_base_de_donnees_T1.xlsx")

