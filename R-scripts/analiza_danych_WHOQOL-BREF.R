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
# Re-code Q3, Q4 and Q26 (WHOQOL-BREF)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data = data %>% 
  mutate(Jak_bardzo_ból_fizyczny_przeszkadzał_Ci_robić_to_co_powinieneśaś = recode(Jak_bardzo_ból_fizyczny_przeszkadzał_Ci_robić_to_co_powinieneśaś,
                                                                                   "1" = 5,
                                                                                   "2" = 4,
                                                                                   "3" = 3,
                                                                                   "4" = 2,
                                                                                   "5" = 1),
         W_jakim_stopniu_potrzebujesz_leczenia_medycznego_do_codziennego_funkcjonowania = recode(W_jakim_stopniu_potrzebujesz_leczenia_medycznego_do_codziennego_funkcjonowania,
                                                                                                 "1" = 5,
                                                                                                 "2" = 4,
                                                                                                 "3" = 3,
                                                                                                 "4" = 2,
                                                                                                 "5" = 1),
         Jak_często_doświadczałeśaś_negatywnych_uczuć_takich_jak_przygnębienie_rozpacz_lęk_depresja = recode(Jak_często_doświadczałeśaś_negatywnych_uczuć_takich_jak_przygnębienie_rozpacz_lęk_depresja,
                                                                                                             "1" = 5,
                                                                                                             "2" = 4,
                                                                                                             "3" = 3,
                                                                                                             "4" = 2,
                                                                                                             "5" = 1))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute domain scores (WHOQOL-BREF)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data = data %>% 
  mutate(Domena_fizyczna = rowMeans(across(c(Jak_bardzo_ból_fizyczny_przeszkadzał_Ci_robić_to_co_powinieneśaś, 
                                             W_jakim_stopniu_potrzebujesz_leczenia_medycznego_do_codziennego_funkcjonowania,
                                             Czy_masz_wystarczająco_energii_w_codziennym_życiu,
                                             Jak_dobrze_możesz_się_poruszać_w_swoim_otoczeniu,
                                             Czy_jesteś_zadowolonya_ze_swojego_snu,
                                             W_jakim_stopniu_jesteś_zadowolonya_ze_swojej_zdolności_wykonywania_codziennych_czynności,
                                             W_jakim_stopniu_jesteś_zadowolonya_ze_swojej_zdolności_do_pracy))) * 4,
         Domena_psychologiczna = rowMeans(across(c(Na_ile_cieszysz_się_życiem,
                                                   W_jakim_stopniu_oceniasz_że_Twoje_życie_ma_sens,
                                                   Jak_dobrze_jesteś_w_stanie_się_skoncentrować,
                                                   Czy_jesteś_w_stanie_zaakceptować_swój_wygląd_fizyczny,
                                                   Czy_jesteś_zadowolonya_z_siebie,
                                                   Jak_często_doświadczałeśaś_negatywnych_uczuć_takich_jak_przygnębienie_rozpacz_lęk_depresja))) * 4,
         Domena_relacji_społecznych = rowMeans(across(c(Czy_jesteś_zadowolonya_ze_swoich_osobistych_relacji_z_ludźmi,
                                                        Czy_jesteś_zadowolonya_ze_swojego_życia_intymnego,
                                                        Czy_jesteś_zadowolonya_z_oparcia_wsparcia_jakie_dostajesz_od_swoich_przyjaciół))) * 4,
         Domena_środowiskowa = rowMeans(across(c(Jak_bezpiecznie_czujesz_się_w_swoim_codziennym_życiu,
                                                 W_jakim_stopniu_Twoje_otoczenie_sprzyja_zdrowiu,
                                                 Czy_masz_wystarczająco_dużo_pieniędzy_na_swoje_potrzeby,
                                                 Na_ile_dostępne_są_informacje_których_możesz_potrzebować_w_codziennym_życiu,
                                                 W_jakim_zakresie_masz_sposobność_realizowania_swoich_zainteresowań,
                                                 Jak_bardzo_jesteś_zadowolony_ze_swoich_warunków_mieszkaniowych,
                                                 Jak_bardzo_jesteś_zadowolonya_z_dostępu_do_świadczeń_opieki_zdrowotnej,
                                                 Czy_jesteś_zadowolonya_z_komunikacji_transportu))) * 4)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute descriptive statistics for Q1, Q2 and each domain
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# List of domain variables
domain_vars = c("Jak_oceniasz_jakość_swojego_życia", "Czy_jesteś_zadowolonya_ze_swojego_zdrowia", 
                "Domena_fizyczna", "Domena_psychologiczna", "Domena_relacji_społecznych", "Domena_środowiskowa")

# Compute stats per variable and reshape to long format
stats_table = map_dfr(domain_vars, function(var) {
  vec = data[[var]]
  tibble(
    Domain = var,
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
# Compute descriptive statistics for each domain variable - gender
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. For each domain, compute stats by gender
stats_table_by_gender = map_dfr(domain_vars, function(var) {
  data %>%
    group_by(Płeć_nadana_podczas_narodzin) %>%
    summarise(
      Domain = var,
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

# 2. Mann-Whitney U test (Wilcoxon rank-sum) for each domain
p_values_gender = map_dfr(domain_vars, function(var) {
  test_result = wilcox.test(
    formula = as.formula(paste(var, "~ Płeć_nadana_podczas_narodzin")),
    data = data,
    exact = FALSE # Needed when sample sizes are large or have ties. It avoids exact calculation and uses a normal approximation.
  )
  
  tibble(
    Domain = var,
    p_value = round(test_result$p.value, 3)
  )
})

# 3. Merge descriptive stats with p-values
stats_table_by_gender_with_p = stats_table_by_gender %>%
  left_join(p_values_gender, by = "Domain") %>%
  relocate(p_value, .after = "Domain")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute descriptive statistics for each domain variable - degree level
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# create degree level variable
data = data %>% 
  mutate(Stopień_studiów_pielęgniarskich = recode(Stopień_oraz_rok_studiów_pielęgniarskich_na_którym_się_obecnie_znajdujesz,
                                                  "I st. I rok" = "I stopień",
                                                  "I st. II rok" = "I stopień",
                                                  "I st. III rok" =  "I stopień",
                                                  "II st. I rok" = "II stopień",
                                                  "II st. II rok" = "II stopień"))


# 1. For each domain, compute stats by degree level
stats_table_by_degree_level = map_dfr(domain_vars, function(var) {
  data %>%
    group_by(Stopień_studiów_pielęgniarskich) %>%
    summarise(
      Domain = var,
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

# 2. Mann-Whitney U test (Wilcoxon rank-sum) for each domain
p_values_degree_level = map_dfr(domain_vars, function(var) {
  test_result = wilcox.test(
    formula = as.formula(paste(var, "~ Stopień_studiów_pielęgniarskich")),
    data = data,
    exact = FALSE # Needed when sample sizes are large or have ties. It avoids exact calculation and uses a normal approximation.
  )
  
  tibble(
    Domain = var,
    p_value = round(test_result$p.value, 3)
  )
})

# 3. Merge descriptive stats with p-values
stats_table_by_degree_level_with_p = stats_table_by_degree_level %>%
  left_join(p_values_degree_level, by = "Domain") %>%
  relocate(p_value, .after = "Domain")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute descriptive statistics for each domain variable - wiek
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. For each domain, compute stats by age
stats_table_by_age = map_dfr(domain_vars, function(var) {
  data %>%
    group_by(Wiek) %>%
    summarise(
      Domain = var,
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

# 2. Kruskal-Wallis test for each domain
p_values_age = map_dfr(domain_vars, function(var) {
  test_result = kruskal.test(
    formula = as.formula(paste(var, "~ Wiek")),
    data = data)
  
  tibble(
    Domain = var,
    p_value = round(test_result$p.value, 3)
  )
})

# 3. Merge descriptive stats with p-values
stats_table_by_age_with_p = stats_table_by_age %>%
  left_join(p_values_age, by = "Domain") %>%
  relocate(p_value, .after = "Domain")

# 4. Dunn test for each domain
dunn_results_age = map_dfr(domain_vars, function(var) {
  test = dunnTest(
    x = data[[var]],
    g = data$Wiek,
    method = "bonferroni"
  )
  
  tibble(
    Domain = var,
    Comparison = test$res$Comparison,
    p_adj = round(test$res$P.adj, 3)
  )
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute descriptive statistics for each domain variable - miejsce zamieszkania
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. For each domain, compute stats by place of residence
stats_table_by_place_of_residence = map_dfr(domain_vars, function(var) {
  data %>%
    group_by(Wielkość_miejscowości_z_której_pochodzisz_przed_rozpoczęciem_studiów) %>%
    summarise(
      Domain = var,
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

# 2. Kruskal-Wallis test for each domain
p_values_place_of_residence = map_dfr(domain_vars, function(var) {
  test_result = kruskal.test(
    formula = as.formula(paste(var, "~ Wielkość_miejscowości_z_której_pochodzisz_przed_rozpoczęciem_studiów")),
    data = data)
  
  tibble(
    Domain = var,
    p_value = round(test_result$p.value, 3)
  )
})

# 3. Merge descriptive stats with p-values
stats_table_by_place_of_residence_with_p = stats_table_by_place_of_residence %>%
  left_join(p_values_place_of_residence, by = "Domain") %>%
  relocate(p_value, .after = "Domain")

# 4. Dunn test for each domain
dunn_results_place_of_residence = map_dfr(domain_vars, function(var) {
  test = dunnTest(
    x = data[[var]],
    g = data$Wielkość_miejscowości_z_której_pochodzisz_przed_rozpoczęciem_studiów,
    method = "bonferroni"
  )
  
  tibble(
    Domain = var,
    Comparison = test$res$Comparison,
    p_adj = round(test$res$P.adj, 3)
  )
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute descriptive statistics for each domain variable - wyksztalcenie
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. For each domain, compute stats by age
stats_table_by_education = map_dfr(domain_vars, function(var) {
  data %>%
    group_by(Uzyskane_wykształcenie_na_ten_moment) %>%
    summarise(
      Domain = var,
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

# 2. Kruskal-Wallis test for each domain
p_values_education = map_dfr(domain_vars, function(var) {
  test_result = kruskal.test(
    formula = as.formula(paste(var, "~ Uzyskane_wykształcenie_na_ten_moment")),
    data = data)
  
  tibble(
    Domain = var,
    p_value = round(test_result$p.value, 3)
  )
})

# 3. Merge descriptive stats with p-values
stats_table_by_education_with_p = stats_table_by_education %>%
  left_join(p_values_education, by = "Domain") %>%
  relocate(p_value, .after = "Domain")

# 4. Dunn test for each domain
dunn_results_education = map_dfr(domain_vars, function(var) {
  test = dunnTest(
    x = data[[var]],
    g = data$Uzyskane_wykształcenie_na_ten_moment,
    method = "bonferroni"
  )
  
  tibble(
    Domain = var,
    Comparison = test$res$Comparison,
    p_adj = round(test$res$P.adj, 3)
  )
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute descriptive statistics for each domain variable - status materialny
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. For each domain, compute stats by material status
stats_table_by_material_status = map_dfr(domain_vars, function(var) {
  data %>%
    group_by(Status_materialny_subiektywna_ocena) %>%
    summarise(
      Domain = var,
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

# 2. Kruskal-Wallis test for each domain
p_values_material_status = map_dfr(domain_vars, function(var) {
  test_result = kruskal.test(
    formula = as.formula(paste(var, "~ Status_materialny_subiektywna_ocena")),
    data = data)
  
  tibble(
    Domain = var,
    p_value = round(test_result$p.value, 3)
  )
})

# 3. Merge descriptive stats with p-values
stats_table_by_material_status_with_p = stats_table_by_material_status %>%
  left_join(p_values_material_status, by = "Domain") %>%
  relocate(p_value, .after = "Domain")

# 4. Dunn test for each domain
dunn_results_material_status = map_dfr(domain_vars, function(var) {
  test = dunnTest(
    x = data[[var]],
    g = data$Status_materialny_subiektywna_ocena,
    method = "bonferroni"
  )
  
  tibble(
    Domain = var,
    Comparison = test$res$Comparison,
    p_adj = round(test$res$P.adj, 3)
  )
})
