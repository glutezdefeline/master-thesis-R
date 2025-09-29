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

# jakosc zycia
meansd = data %>%
  group_by(Status_materialny_subiektywna_ocena) %>%
  summarise(mean_value = mean(Jak_oceniasz_jakość_swojego_życia, na.rm = TRUE),
            sd_value = sd(Jak_oceniasz_jakość_swojego_życia, na.rm = TRUE))

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
                  y = Jak_oceniasz_jakość_swojego_życia),
              width = 0.15, height = 0,
              size = 2, alpha = 0.1, color = "gray30") +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#F2AD00", "#5BBCD6", "#F98400")) +
  labs(x = "Status materialny", y = "Jakość życia") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none")

ggsave("Graphs/pointrange_plots/pointrange_plot_status_materialny_jakosc_zycia.png", width = 12, height = 12, units = "cm", dpi = 300)

# jakosc zdrowia
meansd = data %>%
  group_by(Status_materialny_subiektywna_ocena) %>%
  summarise(mean_value = mean(Czy_jesteś_zadowolonya_ze_swojego_zdrowia, na.rm = TRUE),
            sd_value = sd(Czy_jesteś_zadowolonya_ze_swojego_zdrowia, na.rm = TRUE))

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
                  y = Czy_jesteś_zadowolonya_ze_swojego_zdrowia),
              width = 0.15, height = 0,
              size = 2, alpha = 0.1, color = "gray30") +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#F2AD00", "#5BBCD6", "#F98400")) +
  labs(x = "Status materialny", y = "Jakość zdrowia") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none")

ggsave("Graphs/pointrange_plots/pointrange_plot_status_materialny_jakosc_zdrowia.png", width = 12, height = 12, units = "cm", dpi = 300)

# domena fizyczna
meansd = data %>%
  group_by(Status_materialny_subiektywna_ocena) %>%
  summarise(mean_value = mean(Domena_fizyczna, na.rm = TRUE),
            sd_value = sd(Domena_fizyczna, na.rm = TRUE))

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
                  y = Domena_fizyczna),
              width = 0.15, height = 0,
              size = 2, alpha = 0.1, color = "gray30") +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#F2AD00", "#5BBCD6", "#F98400")) +
  labs(x = "Status materialny", y = "Domena fizyczna") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none")

ggsave("Graphs/pointrange_plots/pointrange_plot_status_materialny_domena_fizyczna.png", width = 12, height = 12, units = "cm", dpi = 300)

# domena srodowiskowa
meansd = data %>%
  group_by(Status_materialny_subiektywna_ocena) %>%
  summarise(mean_value = mean(Domena_środowiskowa, na.rm = TRUE),
            sd_value = sd(Domena_środowiskowa, na.rm = TRUE))

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
                  y = Domena_środowiskowa),
              width = 0.15, height = 0,
              size = 2, alpha = 0.1, color = "gray30") +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#F2AD00", "#5BBCD6", "#F98400")) +
  labs(x = "Status materialny", y = "Domena środowiskowa") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none")

ggsave("Graphs/pointrange_plots/pointrange_plot_status_materialny_domena_srodowiskowa.png", width = 12, height = 12, units = "cm", dpi = 300)
