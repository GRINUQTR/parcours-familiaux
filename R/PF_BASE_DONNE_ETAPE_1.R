library(tidyverse)
library(writexl)
library(haven)
library(readxl)

# Importation des données
df_smonkey_t1 <- read_sav("raw_data/questionnaires_temps_1_20250128.sav")

# Fonction de renommage en lot
rename_by_prefix <- function(data, prefix, new_prefix) {
  rename_with(data, ~str_replace(.x, paste0("^", prefix), new_prefix), starts_with(prefix))
}

# Renommage des variables
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
    conso_12mois_autre = q0052_0003
  )

# Fonction d'inversion
reverse_items <- function(df, items, max_score, prefix = "reversed_") {
  df %>%
    mutate(across(all_of(items), ~ max_score + 1 - ., .names = paste0(prefix, "{.col}")))
}

# Inverser les items ISP et calculer le score
df_smonkey_t1 <- df_smonkey_t1 %>%
  reverse_items(c("isp_0001", "isp_0002", "isp_0003", "isp_0004", "isp_0005",
                  "isp_0006", "isp_0007", "isp_0008", "isp_0009", "isp_0015", "isp_0016"), 5) %>%
  rowwise() %>%
  mutate(isp_score = sum(c_across(starts_with("reversed_isp")))) %>%
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
  mutate(ees_score = sum(c_across(all_of(c("ees_0001", "ees_0002", "ees_0004", "ees_0006", "ees_0007")),
                                  c_across(starts_with("reversed_ees"))))) %>%
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

# Autres scores
df_smonkey_t1 <- df_smonkey_t1 %>%
  rowwise() %>%
  mutate(
    k6_score = sum(c_across(starts_with("k6_"))),
    eqcc_score = sum(c_across(starts_with("eqcc"))),
    gf6_score = mean(c_across(starts_with("gf6")))
  ) %>%
  ungroup()

# Importer et merger les données sociodémographique


df_socio <- read_excel("raw_data/PF_base_de_donnees_sociodémographique_T1_20250225.xlsx", sheet = "Questionnaire_sociodémographiqu") %>%
  filter(vague == 1)

df_general <- left_join(df_socio, df_smonkey, by = "id")

write_xlsx(df_general, "data_base/PF_base_de_donnees_T1.xlsx")

