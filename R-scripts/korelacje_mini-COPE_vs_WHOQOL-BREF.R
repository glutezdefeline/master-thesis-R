library(tidyverse)
library(dplyr)
library(purrr)
library(extrafont)

#font_import() # run only once
loadfonts(device = "win")

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

# List of strategy variables
strategy_vars = c("Aktywne_radzenie_sobie", "Planowanie", "Pozytywne_przewartościowanie", "Akceptacja",
                  "Poczucie_humoru", "Zwrot_ku_religii", "Poszukiwanie_wsparcia_emocjonalnego",
                  "Poszukiwanie_wsparcia_instrumentalnego", "Zajmowanie_się_czymś_innym", "Zaprzeczanie",
                  "Wyładowanie", "Zażywanie_substancji_psychoaktywnych", "Zaprzestanie_działań", "Obwinianie_siebie")

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

# List of domain variables
domain_vars = c("Jak_oceniasz_jakość_swojego_życia", "Czy_jesteś_zadowolonya_ze_swojego_zdrowia", 
                "Domena_fizyczna", "Domena_psychologiczna", "Domena_relacji_społecznych", "Domena_środowiskowa")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run Spearman correlation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Function to compute Spearman correlation and return r and p
get_spearman = function(var1, var2, data) {
  result = cor.test(data[[var1]], data[[var2]], method = "spearman", exact = FALSE)
  tibble(
    strategy = var1,
    domain = var2,
    r = round(result$estimate, 2),
    p = round(result$p.value, 3)
  )
}

# Run pairwise correlations and bind results
correlation_results = expand.grid(strategy_vars, domain_vars, stringsAsFactors = FALSE) %>%
  rename(strategy = Var1, domain = Var2) %>%
  pmap_dfr(~get_spearman(..1, ..2, data))

# Pivot to wide format
correlation_wide = correlation_results %>%
  mutate(result = paste0("r = ", r, ", p = ", p)) %>%
  select(-r, -p) %>%
  pivot_wider(names_from = domain, values_from = result)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Correlation heatmap
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Labels for nicer axis names
strategy_labels = c(
  "Aktywne_radzenie_sobie" = "Aktywne radzenie sobie",
  "Planowanie" = "Planowanie",
  "Pozytywne_przewartościowanie" = "Pozytywne \nprzewartościowanie",
  "Akceptacja" = "Akceptacja",
  "Poczucie_humoru" = "Poczucie humoru",
  "Zwrot_ku_religii" = "Zwrot ku religii",
  "Poszukiwanie_wsparcia_emocjonalnego" = "Poszukiwanie wsparcia \nemocjonalnego",
  "Poszukiwanie_wsparcia_instrumentalnego" = "Poszukiwanie wsparcia \ninstrumentalnego",
  "Zajmowanie_się_czymś_innym" = "Zajmowanie się czymś innym",
  "Zaprzeczanie" = "Zaprzeczanie",
  "Wyładowanie" = "Wyładowanie",
  "Zażywanie_substancji_psychoaktywnych" = "Zażywanie substancji \npsychoaktywnych",
  "Zaprzestanie_działań" = "Zaprzestanie działań",
  "Obwinianie_siebie" = "Obwinianie siebie"
)

domain_labels = c(
  "Jak_oceniasz_jakość_swojego_życia" = "Jakość życia\n(subiektywna ocena)",
  "Czy_jesteś_zadowolonya_ze_swojego_zdrowia" = "Jakość zdrowia\n(subiektywna ocena)",
  "Domena_fizyczna" = "Domena fizyczna",
  "Domena_psychologiczna" = "Domena psychologiczna",
  "Domena_relacji_społecznych" = "Domena relacji społecznych",
  "Domena_środowiskowa" = "Domena środowiskowa"
)

# Function to compute Spearman r
get_spearman_r = function(var1, var2, data) {
  result = cor.test(data[[var1]], data[[var2]], method = "spearman", exact = FALSE)
  tibble(
    strategy = var1,
    domain = var2,
    r = round(result$estimate, 2)
  )
}

# Compute correlation matrix
correlation_matrix = expand.grid(strategy_vars, domain_vars, stringsAsFactors = FALSE) %>%
  rename(strategy = Var1, domain = Var2) %>%
  pmap_dfr(~get_spearman_r(..1, ..2, data))

correlation_matrix = correlation_matrix %>%
  mutate(strategy = factor(strategy, levels = strategy_vars),
         domain = factor(domain, levels = domain_vars),
         strategy_label = recode(strategy, !!!strategy_labels),
         domain_label = recode(domain, !!!domain_labels))

# plot a heatmap
correlation_matrix %>% 
  ggplot(aes(x = domain_label, y = strategy_label, fill = r)) +
  geom_tile(color = "white") +
  geom_text(aes(label = r), color = "black", size = 3, family = "Times New Roman") +
  scale_fill_gradient2(low = "#1F78B4", mid = "white", high = "#E31A1C", midpoint = 0,
                       name = expression(r[S])) +  # Subscript for Spearman r
  labs(x = "WHOQOL-BREF",
       y = "Mini-COPE") +
  theme_bw(base_size = 11, base_family = "Times New Roman") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

ggsave("Graphs/heatmap.png", width = 19, height = 16, units = "cm", dpi = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Correlation heatmap (only significant results)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Labels for nicer axis names
strategy_labels = c(
  "Aktywne_radzenie_sobie" = "Aktywne radzenie sobie",
  "Planowanie" = "Planowanie",
  "Pozytywne_przewartościowanie" = "Pozytywne \nprzewartościowanie",
  "Akceptacja" = "Akceptacja",
  "Poczucie_humoru" = "Poczucie humoru",
  "Zwrot_ku_religii" = "Zwrot ku religii",
  "Poszukiwanie_wsparcia_emocjonalnego" = "Poszukiwanie wsparcia \nemocjonalnego",
  "Poszukiwanie_wsparcia_instrumentalnego" = "Poszukiwanie wsparcia \ninstrumentalnego",
  "Zajmowanie_się_czymś_innym" = "Zajmowanie się czymś innym",
  "Zaprzeczanie" = "Zaprzeczanie",
  "Wyładowanie" = "Wyładowanie",
  "Zażywanie_substancji_psychoaktywnych" = "Zażywanie substancji \npsychoaktywnych",
  "Zaprzestanie_działań" = "Zaprzestanie działań",
  "Obwinianie_siebie" = "Obwinianie siebie"
)

domain_labels = c(
  "Jak_oceniasz_jakość_swojego_życia" = "Jakość życia\n(subiektywna ocena)",
  "Czy_jesteś_zadowolonya_ze_swojego_zdrowia" = "Jakość zdrowia\n(subiektywna ocena)",
  "Domena_fizyczna" = "Domena fizyczna",
  "Domena_psychologiczna" = "Domena psychologiczna",
  "Domena_relacji_społecznych" = "Domena relacji społecznych",
  "Domena_środowiskowa" = "Domena środowiskowa"
)

# Step 1: Compute both r and p values
get_spearman = function(var1, var2, data) {
  result = cor.test(data[[var1]], data[[var2]], method = "spearman", exact = FALSE)
  tibble(
    strategy = var1,
    domain = var2,
    r = round(result$estimate, 2),
    p = round(result$p.value, 3)
  )
}

# Step 2: Create correlation table
correlation_df = expand.grid(strategy_vars, domain_vars, stringsAsFactors = FALSE) %>%
  rename(strategy = Var1, domain = Var2) %>%
  pmap_dfr(~get_spearman(..1, ..2, data))

# Step 3: Filter for significant results only (p < 0.05)
correlation_sig = correlation_df %>%
  filter(p < 0.05) %>%
  mutate(strategy = factor(strategy, levels = strategy_vars),
         domain = factor(domain, levels = domain_vars),
         strategy_label = recode(strategy, !!!strategy_labels),
         domain_label = recode(domain, !!!domain_labels))

# Step 4: Plot heatmap for significant results
correlation_sig %>% 
  ggplot(aes(x = domain_label, y = strategy_label, fill = r)) +
  geom_tile(color = "white") +
  geom_text(aes(label = r), color = "black", size = 3, family = "Times New Roman") +
  scale_fill_gradient2(
    low = "#1F78B4", mid = "white", high = "#E31A1C", midpoint = 0,
    name = expression(r[S])  # Subscript for Spearman r
  ) +
  labs(x = "WHOQOL-BREF",
       y = "Mini-COPE") +
  theme_bw(base_size = 11, base_family = "Times New Roman") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

ggsave("Graphs/heatmap_significant.png", width = 17, height = 14, units = "cm", dpi = 300)
