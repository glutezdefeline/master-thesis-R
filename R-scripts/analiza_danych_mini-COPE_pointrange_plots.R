library(tidyverse)
library(dplyr)
library(wesanderson)
library(extrafont)

#font_import() # run only once
loadfonts(device = "win")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Choose color for plots
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wes_palette("Darjeeling1")
str(wes_palette("Darjeeling1"))

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
# Pointrange plot (plec) - significant results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# zazywanie substancji psychoaktywnych
meansd = data %>%
  group_by(Płeć_nadana_podczas_narodzin) %>%
  summarise(mean_value = mean(Zażywanie_substancji_psychoaktywnych, na.rm = TRUE),
            sd_value = sd(Zażywanie_substancji_psychoaktywnych, na.rm = TRUE))

ggplot() +
  # error bars (mean ± SD)
  geom_errorbar(data = meansd,
                aes(x = Płeć_nadana_podczas_narodzin,
                    ymin = mean_value - sd_value,
                    ymax = mean_value + sd_value,
                    color = Płeć_nadana_podczas_narodzin),
                width = 0.07, size = 1.15) +
  # mean points
  geom_point(data = meansd,
             aes(x = Płeć_nadana_podczas_narodzin,
                 y = mean_value,
                 color = Płeć_nadana_podczas_narodzin),
             shape = 18, size = 10) +
  # raw data
  geom_jitter(data = data,
              aes(x = Płeć_nadana_podczas_narodzin,
                  y = Zażywanie_substancji_psychoaktywnych),
              width = 0.15, height = 0,
              size = 2, alpha = 0.1, color = "gray30") +
  scale_color_manual(values = c("#E6A0C4", "#7294D4")) +
  labs(x = "Płeć", y = "Zażywanie substancji psychoaktywnych") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none")

ggsave("Graphs/pointrange_plots/pointrange_plot_plec_zazywanie_substancji_psychoaktywnych.png", width = 12, height = 12, units = "cm", dpi = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pointrange plot (wiek) - significant results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# planowanie
meansd = data %>%
  group_by(Wiek) %>%
  summarise(mean_value = mean(Planowanie, na.rm = TRUE),
            sd_value = sd(Planowanie, na.rm = TRUE))

ggplot() +
  # error bars (mean ± SD)
  geom_errorbar(data = meansd,
                aes(x = Wiek,
                    ymin = mean_value - sd_value,
                    ymax = mean_value + sd_value,
                    color = Wiek),
                width = 0.07, size = 1.15) +
  # mean points
  geom_point(data = meansd,
             aes(x = Wiek,
                 y = mean_value,
                 color = Wiek),
             shape = 18, size = 10) +
  # raw data
  geom_jitter(data = data,
              aes(x = Wiek,
                  y = Planowanie),
              width = 0.15, height = 0,
              size = 2, alpha = 0.1, color = "gray30") +
  scale_color_manual(values = c("#00A08A", "#F2AD00", "#5BBCD6")) +
  labs(x = "Wiek", y = "Planowanie") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none")

ggsave("Graphs/pointrange_plots/pointrange_plot_wiek_planowanie.png", width = 12, height = 12, units = "cm", dpi = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pointrange plot (miejsce zamieszkania) - significant results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#data$Wielkość_miejscowości_z_której_pochodzisz_przed_rozpoczęciem_studiów = factor(
#  data$Wielkość_miejscowości_z_której_pochodzisz_przed_rozpoczęciem_studiów,
#  levels = c(
#    "wieś",
#    "małe miasto do 20 tysięcy mieszkańców",
#    "średnie miasto od 20 tysięcy do 100 tysięcy mieszkańców",
#    "duże miasto od 100 tysięcy mieszkańców"))

# planowanie
#meansd = data %>%
#  group_by(Wielkość_miejscowości_z_której_pochodzisz_przed_rozpoczęciem_studiów) %>%
#  summarise(mean_value = mean(Planowanie, na.rm = TRUE),
#            sd_value = sd(Planowanie, na.rm = TRUE))

#ggplot() +
#  # error bars (mean ± SD)
#  geom_errorbar(data = meansd,
#                aes(x = Wielkość_miejscowości_z_której_pochodzisz_przed_rozpoczęciem_studiów,
#                    ymin = mean_value - sd_value,
#                    ymax = mean_value + sd_value,
#                    color = Wielkość_miejscowości_z_której_pochodzisz_przed_rozpoczęciem_studiów),
#                width = 0.07, size = 1.15) +
#  # mean points
#  geom_point(data = meansd,
#             aes(x = Wielkość_miejscowości_z_której_pochodzisz_przed_rozpoczęciem_studiów,
#                 y = mean_value,
#                 color = Wielkość_miejscowości_z_której_pochodzisz_przed_rozpoczęciem_studiów),
#             shape = 18, size = 10) +
#  # raw data
#  geom_jitter(data = data,
#              aes(x = Wielkość_miejscowości_z_której_pochodzisz_przed_rozpoczęciem_studiów,
#                  y = Planowanie),
#              width = 0.15, height = 0,
#              size = 2, alpha = 0.1, color = "gray30") +
#  scale_color_manual(values = c("#00A08A", "#F2AD00", "#5BBCD6", "#F98400")) +
#  scale_x_discrete(labels = c(
#    "wieś" = "wieś",
#    "małe miasto do 20 tysięcy mieszkańców" = "Miasto < 20 tys.\nmieszkańców",
#    "średnie miasto od 20 tysięcy do 100 tysięcy mieszkańców" = "Miasto 20 – 100 tys.\nmieszkańców",
#    "duże miasto od 100 tysięcy mieszkańców" = "Miasto > 100 tys.\nmieszkańców")) +
#  labs(x = "Miejsce zamieszkania", y = "Planowanie") +
#  theme_bw(base_family = "Times New Roman") +
#  theme(legend.position = "none")

#ggsave("Graphs/pointrange_plots/pointrange_plot_miejsce_zamieszkania_planowanie.png", width = 12, height = 12, units = "cm", dpi = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pointrange plot (wyksztalcenie) - significant results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# zajmowanie sie czyms innym
meansd = data %>%
  group_by(Uzyskane_wykształcenie_na_ten_moment) %>%
  summarise(mean_value = mean(Zajmowanie_się_czymś_innym, na.rm = TRUE),
            sd_value = sd(Zajmowanie_się_czymś_innym, na.rm = TRUE))

ggplot() +
  # error bars (mean ± SD)
  geom_errorbar(data = meansd,
                aes(x = Uzyskane_wykształcenie_na_ten_moment,
                    ymin = mean_value - sd_value,
                    ymax = mean_value + sd_value,
                    color = Uzyskane_wykształcenie_na_ten_moment),
                width = 0.07, size = 1.15) +
  # mean points
  geom_point(data = meansd,
             aes(x = Uzyskane_wykształcenie_na_ten_moment,
                 y = mean_value,
                 color = Uzyskane_wykształcenie_na_ten_moment),
             shape = 18, size = 10) +
  # raw data
  geom_jitter(data = data,
              aes(x = Uzyskane_wykształcenie_na_ten_moment,
                  y = Zajmowanie_się_czymś_innym),
              width = 0.15, height = 0,
              size = 2, alpha = 0.1, color = "gray30") +
  scale_color_manual(values = c("#00A08A", "#F2AD00", "#5BBCD6")) +
  labs(x = "Wykształcenie", y = "Zajmowanie się czymś innym") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none")

ggsave("Graphs/pointrange_plots/pointrange_plot_wyksztalcenie_zajmowanie_sie_czyms_innym.png", width = 12, height = 12, units = "cm", dpi = 300)

# zaprzeczanie
meansd = data %>%
  group_by(Uzyskane_wykształcenie_na_ten_moment) %>%
  summarise(mean_value = mean(Zaprzeczanie, na.rm = TRUE),
            sd_value = sd(Zaprzeczanie, na.rm = TRUE))

ggplot() +
  # error bars (mean ± SD)
  geom_errorbar(data = meansd,
                aes(x = Uzyskane_wykształcenie_na_ten_moment,
                    ymin = mean_value - sd_value,
                    ymax = mean_value + sd_value,
                    color = Uzyskane_wykształcenie_na_ten_moment),
                width = 0.07, size = 1.15) +
  # mean points
  geom_point(data = meansd,
             aes(x = Uzyskane_wykształcenie_na_ten_moment,
                 y = mean_value,
                 color = Uzyskane_wykształcenie_na_ten_moment),
             shape = 18, size = 10) +
  # raw data
  geom_jitter(data = data,
              aes(x = Uzyskane_wykształcenie_na_ten_moment,
                  y = Zaprzeczanie),
              width = 0.15, height = 0,
              size = 2, alpha = 0.1, color = "gray30") +
  scale_color_manual(values = c("#00A08A", "#F2AD00", "#5BBCD6")) +
  labs(x = "Wykształcenie", y = "Zaprzeczanie") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none")

ggsave("Graphs/pointrange_plots/pointrange_plot_wyksztalcenie_zaprzeczanie.png", width = 12, height = 12, units = "cm", dpi = 300)

# wyladowanie
meansd = data %>%
  group_by(Uzyskane_wykształcenie_na_ten_moment) %>%
  summarise(mean_value = mean(Wyładowanie, na.rm = TRUE),
            sd_value = sd(Wyładowanie, na.rm = TRUE))

ggplot() +
  # error bars (mean ± SD)
  geom_errorbar(data = meansd,
                aes(x = Uzyskane_wykształcenie_na_ten_moment,
                    ymin = mean_value - sd_value,
                    ymax = mean_value + sd_value,
                    color = Uzyskane_wykształcenie_na_ten_moment),
                width = 0.07, size = 1.15) +
  # mean points
  geom_point(data = meansd,
             aes(x = Uzyskane_wykształcenie_na_ten_moment,
                 y = mean_value,
                 color = Uzyskane_wykształcenie_na_ten_moment),
             shape = 18, size = 10) +
  # raw data
  geom_jitter(data = data,
              aes(x = Uzyskane_wykształcenie_na_ten_moment,
                  y = Wyładowanie),
              width = 0.15, height = 0,
              size = 2, alpha = 0.1, color = "gray30") +
  scale_color_manual(values = c("#00A08A", "#F2AD00", "#5BBCD6")) +
  labs(x = "Wykształcenie", y = "Wyładowanie") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none")

ggsave("Graphs/pointrange_plots/pointrange_plot_wyksztalcenie_wyladowanie.png", width = 12, height = 12, units = "cm", dpi = 300)

# zaprzestanie dzialan
meansd = data %>%
  group_by(Uzyskane_wykształcenie_na_ten_moment) %>%
  summarise(mean_value = mean(Zaprzestanie_działań, na.rm = TRUE),
            sd_value = sd(Zaprzestanie_działań, na.rm = TRUE))

ggplot() +
  # error bars (mean ± SD)
  geom_errorbar(data = meansd,
                aes(x = Uzyskane_wykształcenie_na_ten_moment,
                    ymin = mean_value - sd_value,
                    ymax = mean_value + sd_value,
                    color = Uzyskane_wykształcenie_na_ten_moment),
                width = 0.07, size = 1.15) +
  # mean points
  geom_point(data = meansd,
             aes(x = Uzyskane_wykształcenie_na_ten_moment,
                 y = mean_value,
                 color = Uzyskane_wykształcenie_na_ten_moment),
             shape = 18, size = 10) +
  # raw data
  geom_jitter(data = data,
              aes(x = Uzyskane_wykształcenie_na_ten_moment,
                  y = Zaprzestanie_działań),
              width = 0.15, height = 0,
              size = 2, alpha = 0.1, color = "gray30") +
  scale_color_manual(values = c("#00A08A", "#F2AD00", "#5BBCD6")) +
  labs(x = "Wykształcenie", y = "Zaprzestanie działań") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none")

ggsave("Graphs/pointrange_plots/pointrange_plot_wyksztalcenie_zaprzestanie_dzialan.png", width = 12, height = 12, units = "cm", dpi = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pointrange plot (stopien studiow pielegniarskich) - significant results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data = data %>% 
  mutate(Stopień_studiów_pielęgniarskich = recode(Stopień_oraz_rok_studiów_pielęgniarskich_na_którym_się_obecnie_znajdujesz,
                                                  "I st. I rok" = "I stopień",
                                                  "I st. II rok" = "I stopień",
                                                  "I st. III rok" =  "I stopień",
                                                  "II st. I rok" = "II stopień",
                                                  "II st. II rok" = "II stopień"))

# planowanie
meansd = data %>%
  group_by(Stopień_studiów_pielęgniarskich) %>%
  summarise(mean_value = mean(Planowanie, na.rm = TRUE),
            sd_value = sd(Planowanie, na.rm = TRUE))

ggplot() +
  # error bars (mean ± SD)
  geom_errorbar(data = meansd,
                aes(x = Stopień_studiów_pielęgniarskich,
                    ymin = mean_value - sd_value,
                    ymax = mean_value + sd_value,
                    color = Stopień_studiów_pielęgniarskich),
                width = 0.07, size = 1.15) +
  # mean points
  geom_point(data = meansd,
             aes(x = Stopień_studiów_pielęgniarskich,
                 y = mean_value,
                 color = Stopień_studiów_pielęgniarskich),
             shape = 18, size = 10) +
  # raw data
  geom_jitter(data = data,
              aes(x = Stopień_studiów_pielęgniarskich,
                  y = Planowanie),
              width = 0.15, height = 0,
              size = 2, alpha = 0.1, color = "gray30") +
  scale_color_manual(values = c("#D67236", "#F1BB7B")) +
  labs(x = "Stopień studiów pielęgniarskich", y = "Planowanie") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none")

ggsave("Graphs/pointrange_plots/pointrange_plot_stopien_studiow_planowanie.png", width = 12, height = 12, units = "cm", dpi = 300)



# zajmowanie sie czyms innym
meansd = data %>%
  group_by(Stopień_studiów_pielęgniarskich) %>%
  summarise(mean_value = mean(Zajmowanie_się_czymś_innym, na.rm = TRUE),
            sd_value = sd(Zajmowanie_się_czymś_innym, na.rm = TRUE))

ggplot() +
  # error bars (mean ± SD)
  geom_errorbar(data = meansd,
                aes(x = Stopień_studiów_pielęgniarskich,
                    ymin = mean_value - sd_value,
                    ymax = mean_value + sd_value,
                    color = Stopień_studiów_pielęgniarskich),
                width = 0.07, size = 1.15) +
  # mean points
  geom_point(data = meansd,
             aes(x = Stopień_studiów_pielęgniarskich,
                 y = mean_value,
                 color = Stopień_studiów_pielęgniarskich),
             shape = 18, size = 10) +
  # raw data
  geom_jitter(data = data,
              aes(x = Stopień_studiów_pielęgniarskich,
                  y = Zajmowanie_się_czymś_innym),
              width = 0.15, height = 0,
              size = 2, alpha = 0.1, color = "gray30") +
  scale_color_manual(values = c("#D67236", "#F1BB7B")) +
  labs(x = "Stopień studiów pielęgniarskich", y = "Zajmowanie się czymś innym") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none")

ggsave("Graphs/pointrange_plots/pointrange_plot_stopien_studiow_zajmowanie_sie_czyms_innym.png", width = 12, height = 12, units = "cm", dpi = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pointrange plot (status materialny) - significant results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data$Status_materialny_subiektywna_ocena = factor(
  data$Status_materialny_subiektywna_ocena,
  levels = c(
    "bardzo zły",
    "zły",
    "przeciętny",
    "dobry",
    "bardzo dobry"))

# poszukiwanie wsparcia emocjonalnego
meansd = data %>%
  group_by(Status_materialny_subiektywna_ocena) %>%
  summarise(mean_value = mean(Poszukiwanie_wsparcia_emocjonalnego, na.rm = TRUE),
            sd_value = sd(Poszukiwanie_wsparcia_emocjonalnego, na.rm = TRUE))

ggplot() +
  # error bars (mean ± SD)
  geom_errorbar(data = meansd,
                aes(x = Status_materialny_subiektywna_ocena,
                    ymin = mean_value - sd_value,
                    ymax = mean_value + sd_value,
                    color = Status_materialny_subiektywna_ocena),
                width = 0.07, size = 1.15) +
  # mean points
  geom_point(data = meansd,
             aes(x = Status_materialny_subiektywna_ocena,
                 y = mean_value,
                 color = Status_materialny_subiektywna_ocena),
             shape = 18, size = 10) +
  # raw data
  geom_jitter(data = data,
              aes(x = Status_materialny_subiektywna_ocena,
                  y = Poszukiwanie_wsparcia_emocjonalnego),
              width = 0.15, height = 0,
              size = 2, alpha = 0.1, color = "gray30") +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#F2AD00", "#5BBCD6", "#F98400")) +
  labs(x = "Status materialny", y = "Poszukiwanie wsparcia emocjonalnego") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none")

ggsave("Graphs/pointrange_plots/pointrange_plot_status_materialny_poszukiwanie_wsparcia_emocjonalnego.png", width = 12, height = 12, units = "cm", dpi = 300)

# poszukiwanie wsparcia instrumentalnego
meansd = data %>%
  group_by(Status_materialny_subiektywna_ocena) %>%
  summarise(mean_value = mean(Poszukiwanie_wsparcia_instrumentalnego, na.rm = TRUE),
            sd_value = sd(Poszukiwanie_wsparcia_instrumentalnego, na.rm = TRUE))

ggplot() +
  # error bars (mean ± SD)
  geom_errorbar(data = meansd,
                aes(x = Status_materialny_subiektywna_ocena,
                    ymin = mean_value - sd_value,
                    ymax = mean_value + sd_value,
                    color = Status_materialny_subiektywna_ocena),
                width = 0.07, size = 1.15) +
  # mean points
  geom_point(data = meansd,
             aes(x = Status_materialny_subiektywna_ocena,
                 y = mean_value,
                 color = Status_materialny_subiektywna_ocena),
             shape = 18, size = 10) +
  # raw data
  geom_jitter(data = data,
              aes(x = Status_materialny_subiektywna_ocena,
                  y = Poszukiwanie_wsparcia_instrumentalnego),
              width = 0.15, height = 0,
              size = 2, alpha = 0.1, color = "gray30") +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#F2AD00", "#5BBCD6", "#F98400")) +
  labs(x = "Status materialny", y = "Poszukiwanie wsparcia instrumentalnego") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none")

ggsave("Graphs/pointrange_plots/pointrange_plot_status_materialny_poszukiwanie_wsparcia_instrumentalnego.png", width = 12, height = 12, units = "cm", dpi = 300)

# zazywanie substancji psychoaktywnych
meansd = data %>%
  group_by(Status_materialny_subiektywna_ocena) %>%
  summarise(mean_value = mean(Zażywanie_substancji_psychoaktywnych, na.rm = TRUE),
            sd_value = sd(Zażywanie_substancji_psychoaktywnych, na.rm = TRUE))

ggplot() +
  # error bars (mean ± SD)
  geom_errorbar(data = meansd,
                aes(x = Status_materialny_subiektywna_ocena,
                    ymin = mean_value - sd_value,
                    ymax = mean_value + sd_value,
                    color = Status_materialny_subiektywna_ocena),
                width = 0.07, size = 1.15) +
  # mean points
  geom_point(data = meansd,
             aes(x = Status_materialny_subiektywna_ocena,
                 y = mean_value,
                 color = Status_materialny_subiektywna_ocena),
             shape = 18, size = 10) +
  # raw data
  geom_jitter(data = data,
              aes(x = Status_materialny_subiektywna_ocena,
                  y = Zażywanie_substancji_psychoaktywnych),
              width = 0.15, height = 0,
              size = 2, alpha = 0.1, color = "gray30") +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#F2AD00", "#5BBCD6", "#F98400")) +
  labs(x = "Status materialny", y = "Zażywanie substancji psychoaktywnych") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none")

ggsave("Graphs/pointrange_plots/pointrange_plot_status_materialny_zazywanie_substancji_psychoaktywnych.png", width = 12, height = 12, units = "cm", dpi = 300)
