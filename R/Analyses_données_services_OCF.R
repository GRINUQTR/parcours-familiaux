library(tidyverse)

#Lecture des bases données
df_smonkey_t1 <- read_excel("data/df_smonkey_t1.xlsx") %>%
  mutate(id = if_else(id == "F021", "F021_JSV", id))%>%
  filter(vague == 1)

df_socio <- read_excel("raw_data/PF_base_de_donnees_sociodémographique_T1_20250319.xlsx", sheet = "Questionnaire_sociodémographiqu") %>%
  filter(vague == 1)

# Joindre les données
df_smonkey_t1_socio <- left_join(df_socio, df_smonkey_t1, by = "id")

# Lecture de la base de données pour les services
df_smonkey_t1_socio_service <- df_smonkey_t1_socio %>%
  filter(!grepl("_E$", id)) %>% #retirer les enfants
  filter(!id %in% c("F021_MG", "F006_PLT")) %>% #père avec données socio-démo seulement
  rowwise() %>%
  mutate(satisfaction_score = sum(c_across(starts_with("satisfaction_")), na.rm = TRUE)) %>%
  ungroup() %>%
  select(id, genre, modalite_garde, diplome_titre, occupation, revenu_familial, starts_with("besoin_"), ends_with("_score"))

#Description de l'échantillon
# Résumés statistiques pour genre, diplôme et occupation
resume_genre <- df_smonkey_t1_socio_service %>%
  count(genre) %>%
  mutate(pourcentage = round(100 * n / sum(n), 1))

resume_diplome <- df_smonkey_t1_socio_service %>%
  count(diplome_titre) %>%
  mutate(pourcentage = round(100 * n / sum(n), 1))

resume_occupation <- df_smonkey_t1_socio_service %>%
  count(genre, occupation) %>%
  group_by(genre) %>%
  mutate(
    pourcentage = round(100 * n / sum(n), 1)
  ) %>%
  ungroup()

# Visualisation : Genre
plot_genre <- df_smonkey_t1_socio_service %>%
  count(genre) %>%
  ggplot(aes(x = genre, y = n, fill = genre)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(title = "Répartition par genre", x = "Genre", y = "Nombre") +
  theme_minimal()

# Visualisation : Diplôme
plot_diplome <- df_smonkey_t1_socio_service %>%
  count(diplome_titre) %>%
  ggplot(aes(x = reorder(diplome_titre, -n), y = n, fill = diplome_titre)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(title = "Répartition selon le niveau de diplôme", x = "Diplôme", y = "Nombre") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# Visualisation : Occupation
plot_occupation <- df_smonkey_t1_socio_service %>%
  count(occupation) %>%
  ggplot(aes(x = reorder(occupation, -n), y = n, fill = occupation)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(title = "Répartition selon l’occupation", x = "Occupation", y = "Nombre") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# Affichage des tableaux en console (si nécessaire)
resume_genre
resume_diplome
resume_occupation

# Affichage des graphiques (dans l’ordre)
plot_genre
plot_diplome
plot_occupation

# Résumés statistiques pour modalite_garde et revenu_familial au niveau des familles
df_famille_unique <- df_smonkey_t1_socio_service %>%
  mutate(id_famille = substr(id, 1, 4)) %>%
  group_by(id_famille) %>%
  slice(1) %>%
  ungroup() %>%
  select(id_famille, modalite_garde, revenu_familial)

# Étape 2 : résumé statistique - modalité de garde
resume_modalite_garde <- df_famille_unique %>%
  count(modalite_garde) %>%
  mutate(pourcentage = round(100 * n / sum(n), 1))

# Étape 3 : résumé statistique - revenu familial
resume_revenu_familial <- df_famille_unique %>%
  count(revenu_familial) %>%
  mutate(pourcentage = round(100 * n / sum(n), 1))

# Étape 4 : visualisation - modalité de garde
plot_modalite_garde <- ggplot(resume_modalite_garde, aes(x = reorder(modalite_garde, -n), y = n, fill = modalite_garde)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(n, " (", pourcentage, "%)")), vjust = -0.5) +
  labs(
    title = "Répartition des familles selon la modalité de garde",
    x = "Modalité de garde",
    y = "Nombre de familles"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# Étape 5 : visualisation - revenu familial
plot_revenu_familial <- ggplot(resume_revenu_familial, aes(x = reorder(revenu_familial, -n), y = n, fill = revenu_familial)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(n, " (", pourcentage, "%)")), vjust = -0.5) +
  labs(
    title = "Répartition des familles selon le revenu familial",
    x = "Revenu familial",
    y = "Nombre de familles"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# Affichage des tableaux en console (si désiré)
resume_modalite_garde
resume_revenu_familial

# Affichage des graphiques
plot_modalite_garde
plot_revenu_familial


# Distribution des scores de satisfaction envers l'intervenant
df_smonkey_t1_socio_service_clean <- df_smonkey_t1_socio_service %>%
  filter(satisfaction_score > 0)

df_smonkey_t1_socio_service_clean %>% # échelle de 0 à 54
  summarise(
    min = min(satisfaction_score, na.rm = TRUE),
    max = max(satisfaction_score, na.rm = TRUE),
    mean = mean(satisfaction_score, na.rm = TRUE),
    median = median(satisfaction_score, na.rm = TRUE),
    sd = sd(satisfaction_score, na.rm = TRUE),
    n = n()
  )

ggplot(df_smonkey_t1_socio_service_clean, aes(x = satisfaction_score)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Distribution des scores de satisfaction",
       x = "Score de satisfaction",
       y = "Fréquence") +
  theme_minimal()

# Analyse des besoins des familles
df_smonkey_t1_socio_service %>%
  select(id, starts_with("besoin_")) %>%
  pivot_longer(cols = starts_with("besoin_"), names_to = "besoin", values_to = "score") %>%
  filter(!grepl("(_satisfaction|_ocf)$", besoin)) %>%  # exclut les colonnes se terminant par _satisfaction ou _ocf
  mutate(mentionne = ifelse(score >= 3, 1, 0)) %>%
  group_by(besoin) %>%
  summarise(
    n = n(),
    n_mentionne = sum(mentionne, na.rm = TRUE),
    pourcentage = round(100 * n_mentionne / n, 1)
  ) %>%
  mutate(besoin = recode(besoin,
                         "besoin_finance" = "Finances",
                         "besoin_soins" = "Soins",
                         "besoin_repit" = "Répit",
                         "besoin_domestique" = "Tâches domestiques",
                         "besoin_emotion" = "Soutien émotionnel")) %>%
  # Visualisation
  ggplot(aes(x = reorder(besoin, -pourcentage), y = pourcentage)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(pourcentage, "%")), vjust = -0.5, size = 4) +
  labs(
    title = "Proportion de participants ayant mentionné un besoin (score ≥ 3)",
    x = "Type de besoin",
    y = "Pourcentage (%)"
  ) +
  ylim(0, 100) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# Besoins mentionnés cumulées
df_besoins_profils <- df_smonkey_t1_socio_service %>%
  mutate(
    nb_besoins_mentionnes = rowSums(across(
      c(besoin_finance, besoin_soins, besoin_repit, besoin_domestique, besoin_emotion),
      ~ .x >= 3
    ), na.rm = TRUE)
  )

df_besoins_profils %>%
  count(nb_besoins_mentionnes) %>%
  mutate(pourcentage = round(100 * n / sum(n), 1))

df_besoins_profils %>%
  count(nb_besoins_mentionnes) %>%
  ggplot(aes(x = factor(nb_besoins_mentionnes), y = n)) +
  geom_col(fill = "darkorchid") +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(
    title = "Nombre de besoins mentionnés par participant·e",
    x = "Nombre de besoins (score ≥ 3)",
    y = "Nombre de participant·e·s"
  ) +
  theme_minimal()

# Satisfaction des besoins

df_smonkey_t1_socio_service %>%
  select(id, ends_with("_satisfaction")) %>%
  pivot_longer(
    cols = -id,  # exclut `id` du pivot
    names_to = "domaine",
    values_to = "satisfaction"
  ) %>%
  group_by(domaine) %>%
  summarise(
    n = sum(!is.na(satisfaction)),
    moyenne = round(mean(satisfaction, na.rm = TRUE), 2),
    ecart_type = round(sd(satisfaction, na.rm = TRUE), 2)
  ) %>%
  mutate(domaine = recode(domaine,
                          "besoin_finance_satisfaction" = "Finances",
                          "besoin_soins_satisfaction" = "Soins",
                          "besoin_repit_satisfaction" = "Répit",
                          "besoin_domestique_satisfaction" = "Tâches domestiques",
                          "besoin_emotion_satisfaction" = "Soutien émotionnel"))

df_smonkey_t1_socio_service %>%
  select(id, ends_with("_satisfaction")) %>%
  pivot_longer(cols = -id, names_to = "domaine", values_to = "satisfaction") %>%
  mutate(domaine = recode(domaine,
                          "besoin_finance_satisfaction" = "Finances",
                          "besoin_soins_satisfaction" = "Soins",
                          "besoin_repit_satisfaction" = "Répit",
                          "besoin_domestique_satisfaction" = "Tâches domestiques",
                          "besoin_emotion_satisfaction" = "Soutien émotionnel")) %>%
  ggplot(aes(x = domaine, y = satisfaction)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(
    title = "Distribution de la satisfaction reçue par domaine",
    x = "Domaine",
    y = "Score de satisfaction (1 à 6)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))



# Visualisation des services en générale

df_nb_service <- read_excel("raw_data/PF_base_de_donnees_sociodémographique_T1_20250414.xlsx", sheet = "Question services") %>%
  filter(vague == 1) %>%
  filter(!is.na(nb_services_total)) %>%  # garde seulement les lignes avec info
  distinct(id, .keep_all = TRUE)

df_long_nb_service <- df_nb_service %>%
  select(id, nb_services_public, nb_services_dpj, nb_services_comm, nb_services_prive) %>%
  pivot_longer(-id, names_to = "type_service", values_to = "nb_services")

df_long_nb_service %>%
  group_by(type_service) %>%
  summarise(total = sum(nb_services, na.rm = TRUE)) %>%
  ggplot(aes(x = type_service, y = total, fill = type_service)) +
  geom_col() +
  labs(title = "Total des services reçus par type", x = "Type de service", y = "Total") +
  theme_minimal()

