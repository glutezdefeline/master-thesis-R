library(tidyverse)
library(dplyr)
library(purrr)
library(FSA) # post-hoc test

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set working directory
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("C:/Users/privm/OneDrive/UMED/MAGISTERKA/MAG")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data = read.csv("ankieta.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Summarize 14 strategies (mini-COPE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data = data %>% 
  mutate(Aktywne_radzenie_sobie = (Moje_wysiłki_koncentrują_się_na_tym_aby_coś_z_tą_sytuacją_zrobić + Podejmuję_działania_aby_poprawić_tę_sytuację)/2,
         Planowanie = (Staram_się_wypracować_strategię_czy_plan_określający_co_należy_robić + Poważnie_zastanawiam_się_nad_tym_jakie_kroki_należy_podjąć)/2,
         Pozytywne_przewartościowanie = (Staram_się_zobaczyć_to_w_innym_bardziej_pozytywnym_świetle + Szukam_dobrych_stron_w_tym_co_się_zdarzyło)/2,
         Akceptacja = (Akceptuję_fakt_że_to_się_już_stało + Uczę_się_z_tym_żyć)/2,
         Poczucie_humoru = (Żartuję_na_ten_temat + Traktuję_tę_sytuację_jak_zabawę)/2,
         Zwrot_ku_religii = (Staram_się_znaleźć_ukojenie_w_religii_czy_w_swojej_wierze + Modlę_się_lub_medytuję)/2,
         Poszukiwanie_wsparcia_emocjonalnego = (Uzyskuję_wsparcie_emocjonalne_od_innych + Otrzymuję_otuchę_i_zrozumienie_od_innych)/2,
         Poszukiwanie_wsparcia_instrumentalnego = (Szukam_rady_i_pomocy_u_innych_odnośnie_tego_co_należy_robić + Otrzymuję_pomoc_lub_poradę_od_innych_osób)/2,
         Zajmowanie_się_czymś_innym = (Zajmuję_się_pracą_lub_innymi_czynnościami_żeby_o_tym_nie_myśleć + Robię_coś_aby_mniej_o_tym_myśleć_np_idę_do_kina_oglądam_TV_czytam_śnię_na_jawie_śpię_lub_robię_zakupy)/2,
         Zaprzeczanie = (Mówię_do_siebie_to_nieprawda + Nie_chcę_uwierzyć_że_to_na_prawdę_się_zdarzyło)/2,
         Wyładowanie = (Mówię_o_rzeczach_które_pozwalają_mi_uciec_od_nieprzyjemnych_uczuć + Ujawniam_swoje_negatywne_emocje)/2,
         Zażywanie_substancji_psychoaktywnych = (Piję_alkohol_lub_zażywam_inne_środki_aby_poczuć_się_lepiej + Piję_alkohol_lub_zażywam_inne_środki_co_pomaga_mi_przez_to_przejść)/2,
         Zaprzestanie_działań = (Rezygnuję_z_prób_osiągnięcia_celu + Rezygnuję_z_poradzenia_sobie_z_tym)/2,
         Obwinianie_siebie = (Krytykuję_samego_siebie + Obwiniam_siebie_za_to_co_się_stało)/2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute descriptive statistics for each strategy variable
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# List of strategy variables
strategy_vars = c("Aktywne_radzenie_sobie", "Planowanie", "Pozytywne_przewartościowanie", "Akceptacja",
                  "Poczucie_humoru", "Zwrot_ku_religii", "Poszukiwanie_wsparcia_emocjonalnego",
                  "Poszukiwanie_wsparcia_instrumentalnego", "Zajmowanie_się_czymś_innym", "Zaprzeczanie",
                  "Wyładowanie", "Zażywanie_substancji_psychoaktywnych", "Zaprzestanie_działań", "Obwinianie_siebie")

# Compute stats per variable and reshape to long format
stats_table = map_dfr(strategy_vars, function(var) {
  vec = data[[var]]
  tibble(
    Strategy = var,
    Mean = round(mean(vec, na.rm = TRUE), 2),
    SD = round(sd(vec, na.rm = TRUE), 2),
    Median = round(median(vec, na.rm = TRUE), 2),
    Q1 = round(quantile(vec, 0.25, na.rm = TRUE), 2),
    Q3 = round(quantile(vec, 0.75, na.rm = TRUE), 2),
    Min = round(min(vec, na.rm = TRUE), 2),
    Max = round(max(vec, na.rm = TRUE), 2)
  )
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute descriptive statistics for each strategy variable - gender
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. For each strategy, compute stats by gender
stats_table_by_gender = map_dfr(strategy_vars, function(var) {
  data %>%
    group_by(Płeć_nadana_podczas_narodzin) %>%
    summarise(
      Strategy = var,
      Mean = round(mean(.data[[var]], na.rm = TRUE), 2),
      SD = round(sd(.data[[var]], na.rm = TRUE), 2),
      Median = round(median(.data[[var]], na.rm = TRUE), 2),
      Q1 = round(quantile(.data[[var]], 0.25, na.rm = TRUE), 2),
      Q3 = round(quantile(.data[[var]], 0.75, na.rm = TRUE), 2),
      Min = round(min(.data[[var]], na.rm = TRUE), 2),
      Max = round(max(.data[[var]], na.rm = TRUE), 2),
      .groups = "drop"
    )
})

# 2. Mann-Whitney U test (Wilcoxon rank-sum) for each strategy
p_values_gender = map_dfr(strategy_vars, function(var) {
  test_result = wilcox.test(
    formula = as.formula(paste(var, "~ Płeć_nadana_podczas_narodzin")),
    data = data,
    exact = FALSE # Needed when sample sizes are large or have ties. It avoids exact calculation and uses a normal approximation.
  )
  
  tibble(
    Strategy = var,
    p_value = round(test_result$p.value, 3)
  )
})

# 3. Merge descriptive stats with p-values
stats_table_by_gender_with_p = stats_table_by_gender %>%
  left_join(p_values_gender, by = "Strategy") %>%
  relocate(p_value, .after = "Strategy")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute descriptive statistics for each strategy variable - degree level
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# create degree level variable
data = data %>% 
  mutate(Stopień_studiów_pielęgniarskich = recode(Stopień_oraz_rok_studiów_pielęgniarskich_na_którym_się_obecnie_znajdujesz,
                                                  "I st. I rok" = "I stopień",
                                                  "I st. II rok" = "I stopień",
                                                  "I st. III rok" =  "I stopień",
                                                  "II st. I rok" = "II stopień",
                                                  "II st. II rok" = "II stopień"))


# 1. For each strategy, compute stats by degree level
stats_table_by_degree_level = map_dfr(strategy_vars, function(var) {
  data %>%
    group_by(Stopień_studiów_pielęgniarskich) %>%
    summarise(
      Strategy = var,
      Mean = round(mean(.data[[var]], na.rm = TRUE), 2),
      SD = round(sd(.data[[var]], na.rm = TRUE), 2),
      Median = round(median(.data[[var]], na.rm = TRUE), 2),
      Q1 = round(quantile(.data[[var]], 0.25, na.rm = TRUE), 2),
      Q3 = round(quantile(.data[[var]], 0.75, na.rm = TRUE), 2),
      Min = round(min(.data[[var]], na.rm = TRUE), 2),
      Max = round(max(.data[[var]], na.rm = TRUE), 2),
      .groups = "drop"
    )
})

# 2. Mann-Whitney U test (Wilcoxon rank-sum) for each strategy
p_values_degree_level = map_dfr(strategy_vars, function(var) {
  test_result = wilcox.test(
    formula = as.formula(paste(var, "~ Stopień_studiów_pielęgniarskich")),
    data = data,
    exact = FALSE # Needed when sample sizes are large or have ties. It avoids exact calculation and uses a normal approximation.
  )
  
  tibble(
    Strategy = var,
    p_value = round(test_result$p.value, 3)
  )
})

# 3. Merge descriptive stats with p-values
stats_table_by_degree_level_with_p = stats_table_by_degree_level %>%
  left_join(p_values_degree_level, by = "Strategy") %>%
  relocate(p_value, .after = "Strategy")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute descriptive statistics for each strategy variable - wiek
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. For each strategy, compute stats by age
stats_table_by_age = map_dfr(strategy_vars, function(var) {
  data %>%
    group_by(Wiek) %>%
    summarise(
      Strategy = var,
      Mean = round(mean(.data[[var]], na.rm = TRUE), 2),
      SD = round(sd(.data[[var]], na.rm = TRUE), 2),
      Median = round(median(.data[[var]], na.rm = TRUE), 2),
      Q1 = round(quantile(.data[[var]], 0.25, na.rm = TRUE), 2),
      Q3 = round(quantile(.data[[var]], 0.75, na.rm = TRUE), 2),
      Min = round(min(.data[[var]], na.rm = TRUE), 2),
      Max = round(max(.data[[var]], na.rm = TRUE), 2),
      .groups = "drop"
    )
})

# 2. Kruskal-Wallis test for each strategy
p_values_age = map_dfr(strategy_vars, function(var) {
  test_result = kruskal.test(
    formula = as.formula(paste(var, "~ Wiek")),
    data = data)
  
  tibble(
    Strategy = var,
    p_value = round(test_result$p.value, 3)
  )
})

# 3. Merge descriptive stats with p-values
stats_table_by_age_with_p = stats_table_by_age %>%
  left_join(p_values_age, by = "Strategy") %>%
  relocate(p_value, .after = "Strategy")

# 4. Dunn test for each strategy
dunn_results_age = map_dfr(strategy_vars, function(var) {
  test = dunnTest(
    x = data[[var]],
    g = data$Wiek,
    method = "bonferroni"
  )
  
  tibble(
    Strategy = var,
    Comparison = test$res$Comparison,
    p_adj = round(test$res$P.adj, 3)
  )
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute descriptive statistics for each strategy variable - miejsce zamieszkania
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. For each strategy, compute stats by place of residence
stats_table_by_place_of_residence = map_dfr(strategy_vars, function(var) {
  data %>%
    group_by(Wielkość_miejscowości_z_której_pochodzisz_przed_rozpoczęciem_studiów) %>%
    summarise(
      Strategy = var,
      Mean = round(mean(.data[[var]], na.rm = TRUE), 2),
      SD = round(sd(.data[[var]], na.rm = TRUE), 2),
      Median = round(median(.data[[var]], na.rm = TRUE), 2),
      Q1 = round(quantile(.data[[var]], 0.25, na.rm = TRUE), 2),
      Q3 = round(quantile(.data[[var]], 0.75, na.rm = TRUE), 2),
      Min = round(min(.data[[var]], na.rm = TRUE), 2),
      Max = round(max(.data[[var]], na.rm = TRUE), 2),
      .groups = "drop"
    )
})

# 2. Kruskal-Wallis test for each strategy
p_values_place_of_residence = map_dfr(strategy_vars, function(var) {
  test_result = kruskal.test(
    formula = as.formula(paste(var, "~ Wielkość_miejscowości_z_której_pochodzisz_przed_rozpoczęciem_studiów")),
    data = data)
  
  tibble(
    Strategy = var,
    p_value = round(test_result$p.value, 3)
  )
})

# 3. Merge descriptive stats with p-values
stats_table_by_place_of_residence_with_p = stats_table_by_place_of_residence %>%
  left_join(p_values_place_of_residence, by = "Strategy") %>%
  relocate(p_value, .after = "Strategy")

# 4. Dunn test for each strategy
dunn_results_place_of_residence = map_dfr(strategy_vars, function(var) {
  test = dunnTest(
    x = data[[var]],
    g = data$Wielkość_miejscowości_z_której_pochodzisz_przed_rozpoczęciem_studiów,
    method = "bonferroni"
  )
  
  tibble(
    Strategy = var,
    Comparison = test$res$Comparison,
    p_adj = round(test$res$P.adj, 3)
  )
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute descriptive statistics for each strategy variable - wyksztalcenie
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. For each strategy, compute stats by age
stats_table_by_education = map_dfr(strategy_vars, function(var) {
  data %>%
    group_by(Uzyskane_wykształcenie_na_ten_moment) %>%
    summarise(
      Strategy = var,
      Mean = round(mean(.data[[var]], na.rm = TRUE), 2),
      SD = round(sd(.data[[var]], na.rm = TRUE), 2),
      Median = round(median(.data[[var]], na.rm = TRUE), 2),
      Q1 = round(quantile(.data[[var]], 0.25, na.rm = TRUE), 2),
      Q3 = round(quantile(.data[[var]], 0.75, na.rm = TRUE), 2),
      Min = round(min(.data[[var]], na.rm = TRUE), 2),
      Max = round(max(.data[[var]], na.rm = TRUE), 2),
      .groups = "drop"
    )
})

# 2. Kruskal-Wallis test for each strategy
p_values_education = map_dfr(strategy_vars, function(var) {
  test_result = kruskal.test(
    formula = as.formula(paste(var, "~ Uzyskane_wykształcenie_na_ten_moment")),
    data = data)
  
  tibble(
    Strategy = var,
    p_value = round(test_result$p.value, 3)
  )
})

# 3. Merge descriptive stats with p-values
stats_table_by_education_with_p = stats_table_by_education %>%
  left_join(p_values_education, by = "Strategy") %>%
  relocate(p_value, .after = "Strategy")

# 4. Dunn test for each strategy
dunn_results_education = map_dfr(strategy_vars, function(var) {
  test = dunnTest(
    x = data[[var]],
    g = data$Uzyskane_wykształcenie_na_ten_moment,
    method = "bonferroni"
  )
  
  tibble(
    Strategy = var,
    Comparison = test$res$Comparison,
    p_adj = round(test$res$P.adj, 3)
  )
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute descriptive statistics for each strategy variable - status materialny
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. For each strategy, compute stats by material status
stats_table_by_material_status = map_dfr(strategy_vars, function(var) {
  data %>%
    group_by(Status_materialny_subiektywna_ocena) %>%
    summarise(
      Strategy = var,
      Mean = round(mean(.data[[var]], na.rm = TRUE), 2),
      SD = round(sd(.data[[var]], na.rm = TRUE), 2),
      Median = round(median(.data[[var]], na.rm = TRUE), 2),
      Q1 = round(quantile(.data[[var]], 0.25, na.rm = TRUE), 2),
      Q3 = round(quantile(.data[[var]], 0.75, na.rm = TRUE), 2),
      Min = round(min(.data[[var]], na.rm = TRUE), 2),
      Max = round(max(.data[[var]], na.rm = TRUE), 2),
      .groups = "drop"
    )
})

# 2. Kruskal-Wallis test for each strategy
p_values_material_status = map_dfr(strategy_vars, function(var) {
  test_result = kruskal.test(
    formula = as.formula(paste(var, "~ Status_materialny_subiektywna_ocena")),
    data = data)
  
  tibble(
    Strategy = var,
    p_value = round(test_result$p.value, 3)
  )
})

# 3. Merge descriptive stats with p-values
stats_table_by_material_status_with_p = stats_table_by_material_status %>%
  left_join(p_values_material_status, by = "Strategy") %>%
  relocate(p_value, .after = "Strategy")

# 4. Dunn test for each strategy
dunn_results_material_status = map_dfr(strategy_vars, function(var) {
  test = dunnTest(
    x = data[[var]],
    g = data$Status_materialny_subiektywna_ocena,
    method = "bonferroni"
  )
  
  tibble(
    Strategy = var,
    Comparison = test$res$Comparison,
    p_adj = round(test$res$P.adj, 3)
  )
})
