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
# Bar plot (ilosc kobiet i mezczyzn)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data %>% 
  ggplot(aes(x = Płeć_nadana_podczas_narodzin)) +
  geom_bar(aes(fill = Płeć_nadana_podczas_narodzin), width = 0.6) +
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            vjust = -0.3,
            size = 4,
            family = "Times New Roman") +
  scale_fill_manual(values = c("#E6A0C4", "#7294D4")) +
  labs(x = "Płeć", y = "Liczba osób") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none")

ggsave("Graphs/bar_plot_kobieta_vs_mezczyzna.png", width = 12, height = 12, units = "cm", dpi = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Bar plot (wiek)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data %>% 
  ggplot(aes(x = Wiek)) +
  geom_bar(aes(fill = Wiek), width = 0.6) +
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            vjust = -0.3,
            size = 4,
            family = "Times New Roman") +
  scale_fill_manual(values = c("#00A08A", "#F2AD00", "#5BBCD6")) +
  labs(x = "Wiek", y = "Liczba osób") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none")

ggsave("Graphs/bar_plot_wiek.png", width = 12, height = 12, units = "cm", dpi = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Bar plot (wiek) - kobieta vs mezczyzna
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data %>% 
  ggplot(aes(x = Wiek)) +
  geom_bar(aes(fill = Wiek), width = 0.6) +
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            vjust = -0.3,
            size = 3,
            family = "Times New Roman") +
  scale_fill_manual(values = c("#00A08A", "#F2AD00", "#5BBCD6")) +
  labs(x = "Wiek", y = "Liczba osób") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "#273046", color = "#273046"), # Change facet_wrap background color
        strip.text = element_text(color = "white", face = "bold")) +  # Change facet text to white for contrast
  facet_wrap(~Płeć_nadana_podczas_narodzin)

ggsave("Graphs/bar_plot_wiek_kobieta_vs_mezczyzna.png", width = 15, height = 10, units = "cm", dpi = 300)

data %>% 
  ggplot(aes(x = Wiek, fill = Płeć_nadana_podczas_narodzin)) +
  geom_bar(position = "dodge", width = 0.6) +
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            position = position_dodge(width = 0.6),
            vjust = -0.3,
            size = 3,
            family = "Times New Roman") +
  scale_fill_manual(values = c("kobieta" = "#E6A0C4", "mężczyzna" = "#7294D4")) +
  labs(x = "Wiek", y = "Liczba osób", fill = "Płeć") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "bottom")

ggsave("Graphs/bar_plot_wiek_kobieta_vs_mezczyzna2.png", width = 11, height = 11, units = "cm", dpi = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Bar plot (miejsce zamieszkania)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data$Wielkość_miejscowości_z_której_pochodzisz_przed_rozpoczęciem_studiów = factor(
  data$Wielkość_miejscowości_z_której_pochodzisz_przed_rozpoczęciem_studiów,
  levels = c(
    "wieś",
    "małe miasto do 20 tysięcy mieszkańców",
    "średnie miasto od 20 tysięcy do 100 tysięcy mieszkańców",
    "duże miasto od 100 tysięcy mieszkańców"))

data %>% 
  ggplot(aes(x = Wielkość_miejscowości_z_której_pochodzisz_przed_rozpoczęciem_studiów)) +
  geom_bar(aes(fill = Wielkość_miejscowości_z_której_pochodzisz_przed_rozpoczęciem_studiów), width = 0.6) +
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            vjust = -0.3,
            size = 3,
            family = "Times New Roman") +
  scale_fill_manual(values = c("#00A08A", "#F2AD00", "#5BBCD6", "#F98400")) +
  scale_x_discrete(labels = c(
    "wieś" = "wieś",
    "małe miasto do 20 tysięcy mieszkańców" = "Miasto < 20 tys.\nmieszkańców",
    "średnie miasto od 20 tysięcy do 100 tysięcy mieszkańców" = "Miasto 20 – 100 tys.\nmieszkańców",
    "duże miasto od 100 tysięcy mieszkańców" = "Miasto > 100 tys.\nmieszkańców")) +
  labs(x = "Miejsce zamieszkania", y = "Liczba osób") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none")

ggsave("Graphs/bar_plot_miejsce_zamieszkania.png", width = 15, height = 10, units = "cm", dpi = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Bar plot (miejsce zamieszkania) - kobieta vs mezczyzna
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data %>% 
  ggplot(aes(x = Wielkość_miejscowości_z_której_pochodzisz_przed_rozpoczęciem_studiów)) +
  geom_bar(aes(fill = Wielkość_miejscowości_z_której_pochodzisz_przed_rozpoczęciem_studiów), width = 0.6) +
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            vjust = -0.3,
            size = 4,
            family = "Times New Roman") +
  scale_fill_manual(values = c("#00A08A", "#F2AD00", "#5BBCD6", "#F98400")) +
  scale_x_discrete(labels = c(
    "wieś" = "wieś",
    "małe miasto do 20 tysięcy mieszkańców" = "Miasto < 20 tys.\nmieszkańców",
    "średnie miasto od 20 tysięcy do 100 tysięcy mieszkańców" = "Miasto 20 – 100 tys.\nmieszkańców",
    "duże miasto od 100 tysięcy mieszkańców" = "Miasto > 100 tys.\nmieszkańców")) +
  labs(x = "Miejsce zamieszkania", y = "Liczba osób") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "#273046", color = "#273046"), # Change facet_wrap background color
        strip.text = element_text(color = "white", face = "bold")) +  # Change facet text to white for contrast
  facet_wrap(~Płeć_nadana_podczas_narodzin)

ggsave("Graphs/bar_plot_miejsce_zamieszkania_kobieta_vs_mezczyzna.png", width = 25, height = 15, units = "cm", dpi = 300)

data %>% 
  ggplot(aes(x = Wielkość_miejscowości_z_której_pochodzisz_przed_rozpoczęciem_studiów, fill = Płeć_nadana_podczas_narodzin)) +
  geom_bar(position = "dodge", width = 0.6) +
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            position = position_dodge(width = 0.6),
            vjust = -0.3,
            size = 4,
            family = "Times New Roman") +
  scale_fill_manual(values = c("kobieta" = "#E6A0C4", "mężczyzna" = "#7294D4")) +
  scale_x_discrete(labels = c(
    "wieś" = "wieś",
    "małe miasto do 20 tysięcy mieszkańców" = "Miasto < 20 tys.\nmieszkańców",
    "średnie miasto od 20 tysięcy do 100 tysięcy mieszkańców" = "Miasto 20 – 100 tys.\nmieszkańców",
    "duże miasto od 100 tysięcy mieszkańców" = "Miasto > 100 tys.\nmieszkańców")) +
  labs(x = "Miejsce zamieszkania", y = "Liczba osób") +
  labs(x = "Miejsce zamieszkania", y = "Liczba osób", fill = "Płeć") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "bottom")

ggsave("Graphs/bar_plot_miejsce_zamieszkania_kobieta_vs_mezczyzna2.png", width = 15, height = 15, units = "cm", dpi = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Bar plot (wyksztalcenie)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data %>% 
  ggplot(aes(x = Uzyskane_wykształcenie_na_ten_moment)) +
  geom_bar(aes(fill = Uzyskane_wykształcenie_na_ten_moment), width = 0.6) +
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            vjust = -0.3,
            size = 4,
            family = "Times New Roman") +
  scale_fill_manual(values = c("#00A08A", "#F2AD00", "#5BBCD6")) +
  scale_x_discrete(labels = c(
    "średnie ogólnokształcące" = "średnie\nogólnokształcące",
    "średnie zawodowe" = "średnie\nzawodowe",
    "wyższe" = "wyższe")) +
  labs(x = "Wykształcenie", y = "Liczba osób") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none")

ggsave("Graphs/bar_plot_wyksztalcenie.png", width = 12, height = 12, units = "cm", dpi = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Bar plot (wyksztalcenie) - kobieta vs mezczyzna
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data %>% 
  ggplot(aes(x = Uzyskane_wykształcenie_na_ten_moment)) +
  geom_bar(aes(fill = Uzyskane_wykształcenie_na_ten_moment), width = 0.6) +
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            vjust = -0.3,
            size = 3,
            family = "Times New Roman") +
  scale_fill_manual(values = c("#00A08A", "#F2AD00", "#5BBCD6")) +
  scale_x_discrete(labels = c(
    "średnie ogólnokształcące" = "średnie\nogólnokształcące",
    "średnie zawodowe" = "średnie\nzawodowe",
    "wyższe" = "wyższe")) +
  labs(x = "Wykształcenie", y = "Liczba osób") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "#273046", color = "#273046"), # Change facet_wrap background color
        strip.text = element_text(color = "white", face = "bold")) +  # Change facet text to white for contrast
  facet_wrap(~Płeć_nadana_podczas_narodzin)

ggsave("Graphs/bar_plot_wyksztalcenie_kobieta_vs_mezczyzna.png", width = 15, height = 10, units = "cm", dpi = 300)

data %>% 
  ggplot(aes(x = Uzyskane_wykształcenie_na_ten_moment, fill = Płeć_nadana_podczas_narodzin)) +
  geom_bar(position = "dodge", width = 0.6) +
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            position = position_dodge(width = 0.6),
            vjust = -0.3,
            size = 4,
            family = "Times New Roman") +
  scale_fill_manual(values = c("kobieta" = "#E6A0C4", "mężczyzna" = "#7294D4")) +
  scale_x_discrete(labels = c(
    "średnie ogólnokształcące" = "średnie\nogólnokształcące",
    "średnie zawodowe" = "średnie\nzawodowe",
    "wyższe" = "wyższe")) +
  labs(x = "Wykształcenie", y = "Liczba osób", fill = "Płeć") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "bottom")

ggsave("Graphs/bar_plot_wyksztalcenie_kobieta_vs_mezczyzna2.png", width = 15, height = 15, units = "cm", dpi = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Bar plot (stopien studiow pielegniarskich)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data = data %>% 
  mutate(Stopień_studiów_pielęgniarskich = recode(Stopień_oraz_rok_studiów_pielęgniarskich_na_którym_się_obecnie_znajdujesz,
                                                  "I st. I rok" = "I stopień",
                                                  "I st. II rok" = "I stopień",
                                                  "I st. III rok" =  "I stopień",
                                                  "II st. I rok" = "II stopień",
                                                  "II st. II rok" = "II stopień"))

data %>% 
  ggplot(aes(x = Stopień_studiów_pielęgniarskich)) +
  geom_bar(aes(fill = Stopień_studiów_pielęgniarskich), width = 0.6) +
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            vjust = -0.3,
            size = 4,
            family = "Times New Roman") +
  scale_fill_manual(values = c("#D67236", "#F1BB7B")) +
  labs(x = "Stopień studiów pielęgniarskich", y = "Liczba osób") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none")

ggsave("Graphs/bar_plot_stopien_studiow_pielegniarskich.png", width = 12, height = 12, units = "cm", dpi = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Bar plot (stopien studiow pielegniarskich) - kobieta vs mezczyzna
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data %>% 
  ggplot(aes(x = Stopień_studiów_pielęgniarskich)) +
  geom_bar(aes(fill = Stopień_studiów_pielęgniarskich), width = 0.6) +
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            vjust = -0.3,
            size = 4,
            family = "Times New Roman") +
  scale_fill_manual(values = c("#D67236", "#F1BB7B")) +
  labs(x = "Stopień studiów pielęgniarskich", y = "Liczba osób") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "#273046", color = "#273046"), # Change facet_wrap background color
        strip.text = element_text(color = "white", face = "bold")) +  # Change facet text to white for contrast
  facet_wrap(~Płeć_nadana_podczas_narodzin)

ggsave("Graphs/bar_plot_stopien_studiow_pielegniarskich_kobieta_vs_mezczyzna.png", width = 17, height = 12, units = "cm", dpi = 300)

data %>% 
  ggplot(aes(x = Stopień_studiów_pielęgniarskich, fill = Płeć_nadana_podczas_narodzin)) +
  geom_bar(position = "dodge", width = 0.6) +
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            position = position_dodge(width = 0.6),
            vjust = -0.3,
            size = 4,
            family = "Times New Roman") +
  scale_fill_manual(values = c("kobieta" = "#E6A0C4", "mężczyzna" = "#7294D4")) +
  labs(x = "Stopień studiów pielęgniarskich", y = "Liczba osób", fill = "Płeć") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "bottom")

ggsave("Graphs/bar_plot_stopien_studiow_pielegniarskich_kobieta_vs_mezczyzna2.png", width = 11, height = 11, units = "cm", dpi = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Bar plot (stopien oraz rok studiow pielegniarskich)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data$Stopień_oraz_rok_studiów_pielęgniarskich_na_którym_się_obecnie_znajdujesz = factor(
  data$Stopień_oraz_rok_studiów_pielęgniarskich_na_którym_się_obecnie_znajdujesz,
  levels = c(
    "I st. I rok",
    "I st. II rok",
    "I st. III rok",
    "II st. I rok",
    "II st. II rok"))

data %>% 
  ggplot(aes(x = Stopień_oraz_rok_studiów_pielęgniarskich_na_którym_się_obecnie_znajdujesz)) +
  geom_bar(aes(fill = Stopień_oraz_rok_studiów_pielęgniarskich_na_którym_się_obecnie_znajdujesz), width = 0.6) +
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            vjust = -0.3,
            size = 4,
            family = "Times New Roman") +
  scale_fill_manual(values = c("#FF0000", "#00A08A", "#F2AD00", "#5BBCD6", "#F98400")) +
  labs(x = "Stopień oraz rok studiów pielęgniarskich", y = "Liczba osób") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none")

ggsave("Graphs/bar_plot_stopien_oraz_rok_studiow_pielegniarskich.png", width = 15, height = 12, units = "cm", dpi = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Bar plot (stopien oraz rok studiow pielegniarskich) - kobieta vs mezczyzna
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data %>% 
  ggplot(aes(x = Stopień_oraz_rok_studiów_pielęgniarskich_na_którym_się_obecnie_znajdujesz)) +
  geom_bar(aes(fill = Stopień_oraz_rok_studiów_pielęgniarskich_na_którym_się_obecnie_znajdujesz), width = 0.6) +
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            vjust = -0.3,
            size = 4,
            family = "Times New Roman") +
  scale_fill_manual(values = c("#FF0000", "#00A08A", "#F2AD00", "#5BBCD6", "#F98400")) +
  labs(x = "Stopień oraz rok studiów pielęgniarskich", y = "Liczba osób") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "#273046", color = "#273046"), # Change facet_wrap background color
        strip.text = element_text(color = "white", face = "bold")) +  # Change facet text to white for contrast
  facet_wrap(~Płeć_nadana_podczas_narodzin)

ggsave("Graphs/bar_plot_stopien_oraz_rok_studiow_pielegniarskich_kobieta_vs_mezczyzna.png", width = 25, height = 15, units = "cm", dpi = 300)

data %>% 
  ggplot(aes(x = Stopień_oraz_rok_studiów_pielęgniarskich_na_którym_się_obecnie_znajdujesz, fill = Płeć_nadana_podczas_narodzin)) +
  geom_bar(position = "dodge", width = 0.6) +
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            position = position_dodge(width = 0.6),
            vjust = -0.3,
            size = 4,
            family = "Times New Roman") +
  scale_fill_manual(values = c("kobieta" = "#E6A0C4", "mężczyzna" = "#7294D4")) +
  labs(x = "Stopień oraz rok studiów pielęgniarskich", y = "Liczba osób", fill = "Płeć") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "bottom")

ggsave("Graphs/bar_plot_stopien_oraz_rok_studiow_pielegniarskich_kobieta_vs_mezczyzna2.png", width = 15, height = 15, units = "cm", dpi = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Bar plot (status materialny)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data$Status_materialny_subiektywna_ocena = factor(
  data$Status_materialny_subiektywna_ocena,
  levels = c(
    "bardzo zły",
    "zły",
    "przeciętny",
    "dobry",
    "bardzo dobry"))

data %>% 
  ggplot(aes(x = Status_materialny_subiektywna_ocena)) +
  geom_bar(aes(fill = Status_materialny_subiektywna_ocena), width = 0.6) +
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            vjust = -0.3,
            size = 4,
            family = "Times New Roman") +
  scale_fill_manual(values = c("#FF0000", "#00A08A", "#F2AD00", "#5BBCD6", "#F98400")) +
  labs(x = "Status materialny", y = "Liczba osób") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none")

ggsave("Graphs/bar_plot_status_materialny.png", width = 12, height = 12, units = "cm", dpi = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Bar plot (status materialny) - kobieta vs mezczyzna
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data %>% 
  ggplot(aes(x = Status_materialny_subiektywna_ocena)) +
  geom_bar(aes(fill = Status_materialny_subiektywna_ocena), width = 0.6) +
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            vjust = -0.3,
            size = 4,
            family = "Times New Roman") +
  scale_fill_manual(values = c("#FF0000", "#00A08A", "#F2AD00", "#5BBCD6", "#F98400")) +
  labs(x = "Status materialny", y = "Liczba osób") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "#273046", color = "#273046"), # Change facet_wrap background color
        strip.text = element_text(color = "white", face = "bold")) +  # Change facet text to white for contrast
  facet_wrap(~Płeć_nadana_podczas_narodzin)

ggsave("Graphs/bar_plot_status_materialny_kobieta_vs_mezczyzna.png", width = 25, height = 15, units = "cm", dpi = 300)

data %>% 
  ggplot(aes(x = Status_materialny_subiektywna_ocena, fill = Płeć_nadana_podczas_narodzin)) +
  geom_bar(position = "dodge", width = 0.6) +
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            position = position_dodge(width = 0.6),
            vjust = -0.3,
            size = 4,
            family = "Times New Roman") +
  scale_fill_manual(values = c("kobieta" = "#E6A0C4", "mężczyzna" = "#7294D4")) +
  labs(x = "Status materialny", y = "Liczba osób", fill = "Płeć") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "bottom")

ggsave("Graphs/bar_plot_status_materialny_kobieta_vs_mezczyzna2.png", width = 15, height = 15, units = "cm", dpi = 300)
