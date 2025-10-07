############################################################
#   Pierwsze zajęcia z metod aktuarialnych
#   Cel spotkania:
#     1) Obejrzeć realne dane portfela (polisy/ekspozycja/szkody)
#     2) Przypomnieć fundamenty R: wczytywanie, filtrowanie, grupowanie,
#        tworzenie nowych zmiennych, wykresy (ggplot2)
#     3) Zwrócić uwagę na ekspozycję (częstość = ClaimNb / Exposure)
#   Uwaga: bez modeli – tylko oglądamy i myślimy „po aktuarialnemu”
############################################################

## =======================
## 0) Pakiety i dane
## =======================
# Chcemy mieć spójne narzędzia do:
#  - transformacji (dplyr, tidyr), wykresów (ggplot2), pracy z kategoriami (forcats),
#  - formatów procentowych (scales), układania wielu wykresów (patchwork),
#  - tablic kontyngencji/mozaiki (vcd).
req = c("dplyr","ggplot2","forcats","scales","patchwork","tidyr","vcd")
new = setdiff(req, rownames(installed.packages()))
if(length(new)) install.packages(new, repos = "https://cloud.r-project.org")
invisible(lapply(req, library, character.only = TRUE))

# Dane: użyj lokalnego pliku 
dane = read.table("dane_fr_liczba", header = TRUE, dec=".", sep="", stringsAsFactors = FALSE)

## =======================
## 1) Szybka inspekcja – co mamy w danych?
## =======================
head(dane)      # pierwsze wiersze
tail(dane)      # ostatnie wiersze
dim(dane)       # wymiary tabeli
names(dane)     # nazwy kolumn
str(dane)       # struktura zmiennych
summary(dane$ClaimNb)  # podsumowanie liczby szkód
summary(dane$DrivAge)  # podsumowanie wieku kierowców

# Porządkowanie typów i nazewnictwa.
# Uwaga: brak kolumny Exposure — ekspozycja=1 dla każdej polisy.
dane = dane %>%
  rename_with(~ gsub("\\.", "_", .x)) %>%    # nazwy bez kropek są wygodniejsze
  mutate(
    ClaimNb  = as.numeric(ClaimNb),          # liczba szkód w roku
    DrivAge  = suppressWarnings(as.numeric(DrivAge)),
    VehClass = as.factor(VehClass),
    MaritalStatus = as.factor(MaritalStatus),
    # "Częstość" na rok = ClaimNb (bo ekspozycja=1 na rekord)
    freq = ClaimNb,
    # Przedziały wieku: ułatwiają tabele krzyżowe i wykresy
    DrivAge_band = cut(DrivAge, breaks = c(17,25,35,45,55,65,100))
  )


## Sprawdzenie braków w kluczowych zmiennych

# Lista zmiennych, które analizujemy w dalszym kodzie
vars_check = c("DrivAge_band","DrivGender","MaritalStatus","VehClass", "ClaimNb")

# Policzmy, ile NA w każdej z nich
na_report = dane %>% summarise(across(all_of(vars_check), ~sum(is.na(.))))
print(na_report)
# ^ u nas nie ma braków danych bo dane są specjalnie przygotowane

## =======================
## 2) Podstawowe operacje (dplyr) 
## =======================
# Polisy ze szkodami (szybki podgląd)
dane %>% filter(ClaimNb > 0) %>% head()

# Sortowanie: gdzie widać ekstremalne liczby szkód?
# 15 pierwszych polis z największą liczbą szkód
dane %>%
  select(DrivAge, DrivGender, MaritalStatus, VehClass, ClaimNb) %>%
  arrange(desc(ClaimNb)) %>%
  head(15)
#nie ma jednoznacznych prawidłowości, ewentualnie klasa pojazdu (te tańsze niż droższe xd)

# Agregacja: szkody wg przedziałów wieku.
# Ponieważ ekspozycja=1, "częstość" w grupie = średnia szkód na polisę.
agg_age = dane %>%
  group_by(DrivAge_band) %>%
  summarise(
    polisy   = n(),
    szkody   = sum(ClaimNb),
    czestosc = mean(ClaimNb),               # średnia liczba szkód na polisę (rocznie)
    odsetek_szodowych = mean(ClaimNb > 0)   # udział polis ze szkodą
  ) %>%
  ungroup()
agg_age

## =======================
## 3) Podstawowe wykresy
## =======================
# Pomagają wychwycić błędy, ogony, niespodzianki.

# Histogram wieku
p1 = ggplot(dane, aes(x = DrivAge)) +
  geom_histogram(bins = 40) +
  labs(title="Rozkład wieku kierowców", x="Wiek", y="Liczba polis")

# Rozkład liczby szkód (dyskretna)
p2 = ggplot(dane, aes(x = factor(ClaimNb))) +
  geom_bar() +
  labs(title="Rozkład liczby szkód (surowy)", x="Liczba szkód w roku", y="Liczba polis")

# „Częstość” wg wieku = średnia szkód na polisę
p3 = ggplot(agg_age, aes(x = DrivAge_band, y = czestosc)) +
  geom_col() +
  scale_y_continuous() +
  labs(title="Średnia liczba szkód na polisę wg wieku", x="Przedział wieku", y="Średnia szkód / polisę / rok")

(p1 | p2) / p3  # układ 2×1 

# Boxplot: rozkład ClaimNb wg klasy pojazdu (jeśli zmienna jest)
ggplot(dane, aes(x = forcats::fct_infreq(VehClass), y = ClaimNb)) +
  geom_boxplot(outlier.alpha = 0.25) +
  coord_flip() +
  labs(title="Liczba szkód wg klasy pojazdu", x="Klasa pojazdu (od najczęstszej)", y="Liczba szkód (rocznie)")


## =======================
## 4) Ciekawsze przekroje – mozaika / heatmapa / udziały
## =======================
# Udział polis z >=1 szkodą wg klasy pojazdu
dane %>%
  mutate(has_claim = factor(ClaimNb > 0, c(FALSE, TRUE), c("0 szkód", ">=1 szkoda"))) %>%
  count(VehClass, has_claim) %>%
  group_by(VehClass) %>% mutate(p = n/sum(n)) %>% ungroup() %>%
  ggplot(aes(x = VehClass, y = p, fill = has_claim)) +
  geom_col(position = "fill") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(title="Udział polis ze szkodą wg klasy pojazdu", x="Klasa pojazdu", y="Udział")

table(dane$VehClass, exclude = T) # kategorie nieuporządkowane wg wartości samochodu

# Ręczny porządek klas (od najtańszych do najdroższych)
levels_order = c("Cheapest","Cheaper","Cheap",
                  "Medium low","Medium","Medium high",
                  "Expensive","More expensive","Most expensive")

# Zmieniamy faktor na uporządkowany
dane = dane %>%
  mutate(VehClass = factor(VehClass, levels = levels_order, ordered = TRUE))

# Sprawdźmy nowy porządek
levels(dane$VehClass)

#####
## =======================
## VehClass × has_claim: mozaika vs stacked %
## =======================
# MOZAIKA (vcd): czy udział polis szkodowych jest taki sam w każdej klasie auta?
dane = dane %>%
  mutate(
    has_claim = factor(ClaimNb > 0, c(FALSE, TRUE), c("0 szkód", ">=1 szkoda"))
  ) %>%
  droplevels()

dane %>%
  group_by(DrivGender) %>%
  summarise(n = n(), p_claim = mean(has_claim == ">=1 szkoda"))

tab_vh = xtabs(~ DrivGender + has_claim, data = dane, drop.unused.levels = TRUE)
vcd::mosaic(tab_vh, shade = TRUE, legend = TRUE, labeling_args = list(rot_labels = c(0,0,0,0)))

# szybkie statystyki asocjacji 
vcd::assocstats(tab_vh)

###############################################################
# Interpretacja wyników vcd::assocstats(tab_vh) dla DrivGender × has_claim
#
# Testy zależności (tablica 2×2):
#  - Pearson X^2 = 20.115, df = 1, p ≈ 7.29e-06
#  - Likelihood Ratio (G^2) = 20.086, df = 1, p ≈ 7.41e-06
#  → Statystycznie odrzucamy hipotezę niezależności płci i wystąpienia szkody.
#
# Wielkość efektu (siła zależności):
#  - Phi = 0.018, Cramer's V = 0.018, Contingency Coeff. ≈ 0.018
#  → Skale Phi/V mają zakres [0,1]; reguła kciuka (Cohen): ~0.1 mały, ~0.3 średni, ~0.5 duży.
#    Tutaj V = 0.018 → efekt jest *ekstremalnie mały* (praktycznie znikomy).
#
# Jak to czytać razem z mozaiką (shade=TRUE):
#  - Kolory to reszty Pearsona: czerwony = mniej niż oczekiwano przy niezależności,
#    niebieski = więcej niż oczekiwano. U nas:
#      * F, ">=1 szkoda" – czerwony (trochę mniej niż oczekiwano),
#      * F, "0 szkód"   – niebieski (trochę więcej niż oczekiwano),
#      * M, "0 szkód"   – czerwony (trochę mniej niż oczekiwano),
#      * M, ">=1 szkoda"– blisko oczekiwanego.
#  - p-value jest bardzo małe, bo próba jest duża: *statystycznie* różnica istnieje,
#    ale *praktycznie* (V ≈ 0.018) jest minimalna.
#
# Wnioski dydaktyczne:
#  - Nie myl "istotne statystycznie" z "istotne biznesowo". Przy dużych N nawet mikroróżnice są istotne.
#  - Efekt płci jest tu bardzo słaby.
#  - Zanim wyciągniemy wnioski o "ryzyku płci", sprawdźmy zmienne pośredniczące (wiek, klasa pojazdu, itp).
#
# Uwaga regulacyjna: w UE płeć nie może być używana w taryfie!!! (Gender Directive).
#      Traktuj to wyłącznie jako ćwiczenie i przykład interpretacji mozaiki.

###############################################################

# Heatmapa „częstości” wg DrivAge_band × uproszczonej klasy pojazdu.
# Częstość = mean(ClaimNb) w komórce, bo ekspozycja=1.
dane = dane %>% mutate(VehClass6 = forcats::fct_lump(VehClass, n = 6))
grid2 = dane %>%
  group_by(DrivAge_band, VehClass6) %>%
  summarise(freq_mean = mean(ClaimNb), .groups="drop")

ggplot(grid2, aes(VehClass6, DrivAge_band, fill = freq_mean)) +
  geom_tile(color = "white") +
  # bardziej kontrastowa paleta czerwieni z kilkoma odcieniami
  scale_fill_gradientn(
    colours = c("#fef0ef", "#fcbba1", "#fc9272", "#ef3b2c", "#99000d"),
    limits = c(min(grid2$freq_mean, na.rm=TRUE),
               max(grid2$freq_mean, na.rm=TRUE)),
    name = "Średnia szkód"
  ) +
  labs(
    title = "Średnia liczba szkód na polisę: wiek × 7 najczęstszych klas pojazdu",
    x = "Klasa pojazdu (po złączeniu rzadkich)", y = "Przedział wieku"
  ) +
  theme_minimal(base_size = 12)



## =======================
## 5) „Decyle ryzyka ex post” i koncentracja szkód (bez ekspozycji)
## =======================
# Uwaga: bez ekspozycji decyle robimy po ClaimNb (na polisę).
# To „surowa” segmentacja i będzie miała dużo zer – OK na rozgrzewkę.

# Decyle wg ClaimNb (średnia szkód na polisę w decylu)
dec = dane %>%
  mutate(decyl = dplyr::ntile(ClaimNb, 10)) %>%  
  group_by(decyl) %>%
  summarise(
    polisy   = n(),
    szkody   = sum(ClaimNb),
    czestosc = mean(ClaimNb)
  ) %>% ungroup()

ggplot(dec, aes(x = decyl, y = czestosc)) +
  geom_point() + geom_line() +
  scale_x_continuous(breaks = 1:10) +
  labs(title="Średnia liczba szkód na polisę w decylach ClaimNb",
       x="Decyl (1 = niższa, 10 = wyższa)", y="Średnia szkód / polisę")

# Krzywa koncentracji: % szkód vs % polis 
lift_df = dane %>%
  arrange(desc(ClaimNb)) %>%                  # sortujemy malejąco po liczbie szkód
  transmute(one = 1, claims = ClaimNb) %>%    # zostawiamy tylko licznik polis i liczby szkód
  mutate(
    cum_polis  = cumsum(one) / sum(one),      # skumulowany % polis
    cum_claims = cumsum(claims) / sum(claims) # skumulowany % szkód
  )


ggplot(lift_df, aes(x = cum_polis, y = cum_claims)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) + # linia „brak koncentracji”
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title="Koncentracja szkód: % szkód vs % polis (sortowanie po ClaimNb)",
       x="% polis", y="% szkód")

# Prosty wskaźnik Pareto 80/20 – jaki % polis generuje ~80% szkód?
pareto_80 = with(lift_df, min(cum_polis[cum_claims >= 0.8]))
message(sprintf("[PARETO] ~80%% szkód generuje top %.1f%% polis.", 100*pareto_80))

