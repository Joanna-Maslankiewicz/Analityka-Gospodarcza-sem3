# =============================================================================
# ZASTOSOWANIE MODELOWANIA PREDYKCYJNEGO W UBEZPIECZENIACH
# CWICZENIA 2 - TARYFIKACJA GLM I GAM (135 minut)
# WERSJA ROZSZERZONA: WYKRESY NA EKRANIE, KALIBRACJA, KOMENTARZE
# =============================================================================
#
# CEL: porownac dwumodelowe taryfy skladki czystej:
#   czestosc (Poisson GLM / Poisson GAM) x wysokosc szkody (Gamma GLM / GAM).
# Do dyspozycji sa cztery warianty: GLM+GLM, GLM+GAM, GAM+GLM, GAM+GAM.
#
# ZBIORY: 60% train, 20% valid, 20% test, podzial NA POZIOMIE POLISY.
# Wazne: wybor modelu na VALID; TEST tylko do koncowej oceny.
#
# DANE: CASdatasets::freMTPL2freq i CASdatasets::freMTPL2sev.
# Szkody o kwotach <= 0 sa pomijane przy estymacji severity Gamma.
# W ocenie skladki poslugujemy sie suma dodatnich kwot z rejestru szkód
# (LossPositive), a NIE koniecznie pelnym ksiegowym kosztem szkód.
#
# PAKIETY: CASdatasets, mgcv, ggplot2.
# Przed zajeciami nalezy je zainstalowac, np.:
install.packages(c("mgcv", "ggplot2"))
install.packages("xts", repos = "https://cloud.r-project.org")
install.packages("CASdatasets", repos = "https://dutangc.perso.math.cnrs.fr/RRepository/pub/", type = "source")
#
# OBSLUGA WYKRESOW:
#   - Kazdy wykres zostaje WYSWIETLONY na ekranie przez print(p).
#   - W RStudio mozna wracac do rysunkow w panelu Plots (strzalki historii).
#   - Dodatkowo wykresy sa zapisywane jako oddzielne PNG i wspolny PDF.
#   - Podczas wykladu ustaw tryb_krokowy <- TRUE, aby po kazdym rysunku
#     zatwierdzac przejscie klawiszem Enter (dziala w konsoli interaktywnej).
#   - Jesli nie chcesz pokazywac wszystkich dodatkowych rysunkow, zmien
#     wykresy_rozszerzone <- FALSE.
#
# WAZNE: wartosci liczbowe i wykresy powstaja DOPIERO PO URUCHOMIENIU R.
# =============================================================================

# 0. USTAWIENIA I PAKIETY ------------------------------------------------------

set.seed(1234)                # Powtarzalny wybor polis i podzial na zbiory.
limit_polis <- 80000L         # Maksymalna liczba polis; Inf = cala baza.
wyswietlaj_wykresy <- TRUE    # TRUE = pokazuj rysunki w panelu Plots / oknie R.
zapisuj_wykresy <- TRUE       # TRUE = zapisuj osobne PNG + wielostronicowy PDF.
tryb_krokowy <- FALSE         # TRUE = zatrzymuj sie po kazdym rysunku (Enter).
wykresy_rozszerzone <- TRUE  # TRUE = dodatkowe wykresy diagnostyczne.
folder_wynikow <- "CW2_GLM_GAM_wyniki"

# Pakiety sprawdzamy PRZED estymacja, aby bledy byly zrozumiale.
for (pkg in c("CASdatasets", "mgcv", "ggplot2")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Brakuje pakietu '%s'. Zainstaluj go przed uruchomieniem.", pkg))
  }
}
# Sam zapis mgcv::gam() nie zawsze wystarcza w starszych instalacjach.
# Dolaczenie pakietu zapewnia dostep do funkcji wewnetrznych rodziny GAM.
suppressPackageStartupMessages(library(mgcv))
suppressPackageStartupMessages(library(ggplot2))

# Wszystkie pliki wynikowe umieszczamy w jednym folderze, aby nie zasmiecac
# katalogu roboczego i latwo skopiowac wyniki do sprawozdania.
dir.create(folder_wynikow, recursive = TRUE, showWarnings = FALSE)

# Kolory dobrano tak, by odroznienie obserwacji, GLM i GAM bylo czytelne
# zarowno na projektorze, jak i w wydruku.
kol_obserw <- "#E88A28"     # Pomaranczowy = wartosci rzeczywiste.
kol_glm    <- "#2463A0"     # Niebieski = model GLM.
kol_gam    <- "#00958D"     # Morski = model GAM.
kol_granat <- "#1C304D"     # Granat = tytuly i etykiety.
kol_szary  <- "#607389"     # Kolor pomocniczy.
kolory_modeli <- c("Obserwowana" = kol_obserw,
                   "GLM" = kol_glm, "GAM" = kol_gam,
                   "Prognozowana" = kol_glm)

# Forma liczbowa do osi: np. 12 500 zamiast 12500, bez notacji naukowej.
etykiety_liczb <- function(x) {
  vapply(x, function(v) {
    if (!is.finite(v)) return(as.character(v))
    if (v != 0 && abs(v) < 1) {
      return(formatC(v, format = "f", digits = 2, decimal.mark = ","))
    }
    format(round(v), big.mark = " ", trim = TRUE, scientific = FALSE)
  }, character(1))
}

# Wspolny motyw dla wszystkich wykresow. Czcionki sa wieksze niz domyslne,
# poniewaz wykresy beda prezentowane studentom z rzutnika.
motyw_cw2 <- function() {
  ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title = element_text(size = 19, face = "bold", colour = kol_granat,
                                margin = margin(b = 5)),
      plot.subtitle = element_text(size = 11.5, colour = kol_szary,
                                   margin = margin(b = 12)),
      plot.caption = element_text(size = 9.5, colour = kol_szary, hjust = 0),
      axis.title = element_text(size = 12.5, face = "bold", colour = kol_granat),
      axis.text = element_text(colour = kol_granat, size = 11),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "#E4EBF1", linewidth = 0.35),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 11.5),
      plot.margin = margin(15, 22, 15, 15)
    )
}

# Rejestr gotowych wykresow: posluzy na koniec do wygenerowania jednego PDF.
# Srodowisko (environment) pozwala dopisywac rysunki wewnatrz funkcji.
rejestr_rysunkow <- new.env(parent = emptyenv())
rejestr_rysunkow$lista <- list()

pokaz_wykres <- function(nazwa, p) {
  # Funkcja jest wspolnym sposobem wyswietlania wszystkich ggplotow.
  # Kluczowe print(p): wewnatrz funkcji i petli ggplot nie rysuje sie sam.
  if (!inherits(p, "ggplot")) stop("Obiekt p musi byc wykresem ggplot.")
  rejestr_rysunkow$lista[[nazwa]] <- p
  nr <- length(rejestr_rysunkow$lista)
  cat(sprintf("\nWYKRES %02d: %s\n", nr, nazwa))
  if (wyswietlaj_wykresy) print(p)
  if (zapisuj_wykresy) {
    # 12 x 6,75 cala = 16:9, wygodne w slajdach i na projektorze.
    sciezka <- file.path(folder_wynikow,
                          sprintf("%02d_%s.png", nr, sub("^[0-9]+_", "", nazwa)))
    ggplot2::ggsave(filename = sciezka, plot = p,
                    width = 12, height = 6.75, units = "in",
                    dpi = 160, bg = "white")
  }
  if (tryb_krokowy && wyswietlaj_wykresy && interactive()) {
    invisible(readline("Nacisnij Enter, aby przejsc do kolejnego wykresu... "))
  }
  invisible(p)
}

# 1. WCZYTANIE, CZYSZCZENIE I PODZIAL -----------------------------------------

# CASdatasets przechowuje osobno: tablice polis i tablice szkod.
# Jedna polisa moze miec zero, jedna lub kilka szkod.
data("freMTPL2freq", package = "CASdatasets")
data("freMTPL2sev", package = "CASdatasets")
pol <- freMTPL2freq
szk <- freMTPL2sev

potrzebne_pol <- c("IDpol", "ClaimNb", "Exposure", "DrivAge", "VehAge",
                   "BonusMalus", "Area")
potrzebne_szk <- c("IDpol", "ClaimAmount")
stopifnot(all(potrzebne_pol %in% names(pol)),
          all(potrzebne_szk %in% names(szk)))

# Konwersja identyfikatora do tekstu eliminuje roznice typow podczas match().
pol$IDpol <- as.character(pol$IDpol)
szk$IDpol <- as.character(szk$IDpol)
cat("Polisy przed kontrola:", nrow(pol), "\n")
cat("Szkody przed kontrola:", nrow(szk), "\n")
print(colSums(is.na(pol[, potrzebne_pol, drop = FALSE])))

# Do modelu Poissona wymagamy Exposure>0, aby log(Exposure) istnial.
# Pozostale predyktory nie moga miec nieskonczonych wartosci ani brakow.
ok <- is.finite(pol$Exposure) & pol$Exposure > 0 &
  is.finite(pol$ClaimNb) & pol$ClaimNb >= 0 &
  is.finite(pol$DrivAge) & is.finite(pol$VehAge) &
  is.finite(pol$BonusMalus) & !is.na(pol$Area) &
  !is.na(pol$IDpol) & nzchar(pol$IDpol)
pol <- pol[ok, , drop = FALSE]
if (anyDuplicated(pol$IDpol)) stop("IDpol w tabeli polis musi byc unikalne.")
if (any(abs(pol$ClaimNb - round(pol$ClaimNb)) > 1e-9))
  stop("ClaimNb musi byc liczba calkowita, nieujemna.")
pol$Area <- factor(pol$Area)
cat("Poprawnych polis:", nrow(pol), "\n")

# Rozsadny limit probki skraca czas estymacji GAM w pracowni komputerowej.
# Ta sama lista wylosowanych polis musi byc uzyta przy analizie severity.
if (nrow(pol) > limit_polis) {
  pol <- pol[sample.int(nrow(pol), limit_polis), , drop = FALSE]
}

# 60/20/20: losujemy NUMERY POLIS, nie numery pojedynczych szkod.
# W ten sposob szkody z jednej polisy nie przedostana sie do roznych zbiorow.
n <- nrow(pol)
if (n < 30L) stop("Za malo polis do podzialu train/valid/test.")
los <- sample.int(n)
n_train <- floor(0.60 * n)
n_valid <- floor(0.20 * n)
pol$zbior <- NA_character_
pol$zbior[los[seq_len(n_train)]] <- "train"
pol$zbior[los[n_train + seq_len(n_valid)]] <- "valid"
pol$zbior[los[seq.int(n_train + n_valid + 1L, n)]] <- "test"
stopifnot(!anyNA(pol$zbior))
cat("Polisy w podzbiorach:\n")
print(table(pol$zbior))

# Do Gamma GLM i GAM nalezy uzyc DODATNICH kwot pojedynczych szkod.
# NA, Inf i kwoty <=0 nie sa dopuszczalna odpowiedzia dla Gamma.
licz_niedodatnie <- sum(!is.finite(szk$ClaimAmount) |
                         szk$ClaimAmount <= 0)
cat("Niedodatnie lub brakujace kwoty szkody:", licz_niedodatnie, "\n")
szk_pos <- szk[is.finite(szk$ClaimAmount) & szk$ClaimAmount > 0,
               , drop = FALSE]
# Zachowujemy tylko szkody nalezace do polis wybranych powyzej.
szk_pos <- szk_pos[szk_pos$IDpol %in% pol$IDpol,
                   , drop = FALSE]

# match() podaje numer WIERSZA polisy w tabeli pol. Gdy polisa ma wiele szkod,
# ta sama pozycja idx wystapi kilka razy - to zamierzone zachowanie.
idx <- match(szk_pos$IDpol, pol$IDpol)
stopifnot(!anyNA(idx))
for (v in c("DrivAge", "VehAge", "BonusMalus", "Area", "zbior")) {
  szk_pos[[v]] <- pol[[v]][idx]
}
stopifnot(!anyNA(szk_pos$zbior))
szk_pos$Area <- factor(szk_pos$Area, levels = levels(pol$Area))

# Suma dodatnich kwot na polise - cel do porownania skladki czystej.
# UWAGA METODOLOGICZNA: nie obejmuje rozliczen niedodatnich ani dodatkowych
# rezerw. Jesli ClaimNb nie odpowiada liczbie tych rekordow, wskazujemy roznice.
if (nrow(szk_pos) == 0L) stop("Brak dodatnich szkod w wylosowanej probie.")
sumy <- aggregate(ClaimAmount ~ IDpol, data = szk_pos, FUN = sum)
names(sumy)[2L] <- "LossPositive"
pos <- match(pol$IDpol, sumy$IDpol)
pol$LossPositive <- sumy$LossPositive[pos]
pol$LossPositive[is.na(pol$LossPositive)] <- 0
pol$NumPositiveRecords <- tabulate(match(szk_pos$IDpol, pol$IDpol),
                                   nbins = nrow(pol))
rozbieznosci <- sum(pol$ClaimNb != pol$NumPositiveRecords)
cat("Polisy: ClaimNb rozni sie od liczby dodatnich rekordow:",
    rozbieznosci, "z", nrow(pol), "\n")
if (rozbieznosci > 0L) {
  cat("UWAGA: interpretuj iloczyn czestosc x severity z uwzglednieniem", 
      "tej niespojnosci danych.\n")
}

pol_train <- pol[pol$zbior == "train", , drop = FALSE]
pol_valid <- pol[pol$zbior == "valid", , drop = FALSE]
pol_test  <- pol[pol$zbior == "test",  , drop = FALSE]
szk_train <- szk_pos[szk_pos$zbior == "train", , drop = FALSE]
szk_valid <- szk_pos[szk_pos$zbior == "valid", , drop = FALSE]
szk_test  <- szk_pos[szk_pos$zbior == "test",  , drop = FALSE]
cat("Dodatnie szkody train/valid/test:",
    nrow(szk_train), nrow(szk_valid), nrow(szk_test), "\n")
if (nrow(szk_train) < 200L || nrow(szk_valid) == 0L || nrow(szk_test) == 0L)
  stop("Za malo dodatnich szkod w ktoryms zbiorze.")

# 2. WIZUALIZACJA DANYCH PRZED ESTYMACJA --------------------------------------

# WYKRES 01 - Dominacja zer w zmiennej ClaimNb.
# Uczniowie zobacza, dlaczego liczba szkod wymaga rozkladu dyskretnego.
kat_liczby <- factor(pmin(as.integer(pol_train$ClaimNb), 4L),
                     levels = 0:4, labels = c("0", "1", "2", "3", "4+"))
tab_liczby <- as.data.frame(table(kat_liczby))
names(tab_liczby) <- c("LiczbaSzkod", "LiczbaPolis")
tab_liczby$Udzial <- 100 * tab_liczby$LiczbaPolis /
  sum(tab_liczby$LiczbaPolis)
p <- ggplot(tab_liczby, aes(x = LiczbaSzkod, y = Udzial)) +
  geom_col(fill = kol_glm, width = 0.68) +
  geom_text(aes(label = paste0(round(Udzial, 1), "%")),
            vjust = -0.4, size = 4.2, colour = kol_granat) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Ile szkód występuje na jednej polisie?",
       subtitle = "Zbiór uczący | ostatnia kategoria obejmuje co najmniej 4 szkody",
       x = "Liczba szkód w obserwowanym okresie", y = "Udział polis [%]",
       caption = "Duża liczba zer jest naturalna dla niskich częstości szkód.") +
  motyw_cw2()
pokaz_wykres("01_liczba_szkod", p)

# WYKRES 02 - Rozklad ekspozycji; czemu offset(log(Exposure)) jest niezbedny?
p <- ggplot(pol_train, aes(x = Exposure)) +
  geom_histogram(bins = 35, fill = kol_gam, colour = "white", linewidth = 0.25) +
  geom_vline(xintercept = median(pol_train$Exposure),
             linetype = "dashed", colour = kol_granat, linewidth = 1) +
  labs(title = "Ekspozycja polis – różnice długości ochrony",
       subtitle = "Linia przerywana oznacza medianę ekspozycji",
       x = "Ekspozycja w latach ochrony", y = "Liczba polis",
       caption = "Polisy o różnej ekspozycji nie są bezpośrednio porównywalne według ClaimNb.") +
  scale_y_continuous(labels = etykiety_liczb) + motyw_cw2()
pokaz_wykres("02_ekspozycja", p)

# WYKRES 03 - Silna asymetria kwot; os X na skali logarytmicznej.
# To jest transformacja OSI wykresu, a nie transformacja zmiennej w Gamma GLM.
p <- ggplot(szk_train, aes(x = ClaimAmount)) +
  geom_histogram(bins = 50, fill = kol_obserw, colour = "white", linewidth = 0.2) +
  scale_x_log10(labels = etykiety_liczb) +
  geom_vline(xintercept = median(szk_train$ClaimAmount),
             colour = kol_granat, linewidth = 1.0, linetype = "dashed") +
  labs(title = "Wysokości pojedynczych szkód – rozkład prawostronny",
       subtitle = "Zbiór uczący | oś kwot w skali log10; linia = mediana",
       x = "Kwota dodatniej szkody [zł; skala log10]", y = "Liczba szkód",
       caption = "Model gamma przewiduje średnią dodatnią kwotę, nie jej logarytm.") +
  scale_y_continuous(labels = etykiety_liczb) + motyw_cw2()
pokaz_wykres("03_rozklad_wysokosci", p)

# 3. ESTYMACJA MODELI CZESTOSCI I WYSOKOSCI -------------------------------

# Dla modelu liczby szkod uzywamy wszystkich polis, lacznie z zerowymi.
# Offset log(Exposure) w predyktorze wymusza proporcjonalnosc E(N|X,E)
# do ekspozycji E. Nie szacujemy wspolczynnika przy offsetcie.
formula_n_glm <- ClaimNb ~ DrivAge + VehAge + BonusMalus + Area +
  offset(log(Exposure))

# W GAM zamieniamy liniowe zaleznosci trzech zmiennych liczbowych na funkcje
# gladkie s(x). 'k' oznacza rozmiar bazy, a NIE automatycznie stopien swobody.
# 'bs="cr"' = penalizowany splajn regresyjny kubiczny.
formula_n_gam <- ClaimNb ~
  s(DrivAge, k = 9, bs = "cr") +
  s(VehAge, k = 7, bs = "cr") +
  s(BonusMalus, k = 9, bs = "cr") +
  Area + offset(log(Exposure))

cat("\nSZACOWANIE CZESTOSCI - Poisson GLM / GAM...\n")
mN_glm <- glm(formula_n_glm, family = poisson(link = "log"),
              data = pol_train)
mN_gam <- mgcv::gam(formula_n_gam, family = poisson(link = "log"),
                    method = "REML", data = pol_train)
cat("GLM czestosc - wspolczynniki:\n")
print(summary(mN_glm)$coefficients)
cat("GAM czestosc - parametry i efektywne stopnie swobody EDF:\n")
print(summary(mN_gam))

#exp z parametrów:
exp(mN_glm$coefficients)

# ! interpretacja DrivAge: jeśli wiek kierowcy rośnie o 1 rok, to liczba szkód rośnie o 0,6%.
# ! interpretacja area: W obszarze B średnia liczba szkód jest większa o 13% niż w obszarze A.

# GAM - nie ma parametrów, jak zbadać wpływ wieku kierowcy na liczbę szkód? -> liczymy wartość dla 20 i 21 roku, liczymy różnicę, i to jest wykładnik
# GLM: (exp^(beta1)-1)*100%
# GAM: exp^(s(DrivAge(21)-(20)))-1


# Dyspersja Pearsona: czy rozproszenie reszt jest wieksze niz oczekiwane
# dla Poissona? Sam wynik >1 nie dowodzi automatycznie koniecznosci NB.
phi_glm <- sum(residuals(mN_glm, type = "pearson")^2) /
  df.residual(mN_glm)
phi_gam <- sum(residuals(mN_gam, type = "pearson")^2) /
  df.residual(mN_gam)
cat("Dyspersja Pearsona GLM / GAM:", round(phi_glm, 3), "/",
    round(phi_gam, 3), "\n")

# Wysokosc szkody (severity): wiersz = pojedyncza dodatnia szkoda.
# NIE ma offsetu ekspozycji - kwoty pojedynczej szkody nie mnozymy
# przez dlugosc trwania polisy.
formula_s_glm <- ClaimAmount ~ DrivAge + VehAge + BonusMalus + Area
formula_s_gam <- ClaimAmount ~
  s(DrivAge, k = 8, bs = "cr") +
  s(VehAge, k = 7, bs = "cr") +
  s(BonusMalus, k = 8, bs = "cr") + Area
cat("\nSZACOWANIE WYSOKOSCI - Gamma GLM / GAM...\n")
mS_glm <- glm(formula_s_glm, family = Gamma(link = "log"),
              data = szk_train)
mS_gam <- mgcv::gam(formula_s_gam, family = Gamma(link = "log"),
                    method = "REML", data = szk_train)
cat("GLM wysokosc - wspolczynniki:\n")
print(summary(mS_glm)$coefficients)
cat("GAM wysokosc - parametry i EDF:\n")
print(summary(mS_gam))

# 4. PROFILE TARYFOWE: GLM VS GAM -----------------------------------------
# Na jednym wykresie pokazujemy obie prognozy przy identycznych cechach X.
# Zmieniamy TYLKO jeden czynnik, pozostale utrzymujac na wybranym poziomie.
# Uwaga: wstega to przyblizony przedzial ufnosci sredniej odpowiedzi GAM
# wynikajacy z SE predyktora (nie przedzial prognozy pojedynczej szkody).

# Poziom referencyjny Area wybieramy wedlug najczestszej kategorii w train.
area_ref <- names(sort(table(pol_train$Area), decreasing = TRUE))[1L]

# Zakres osi bez skrajnych 1% obserwacji ogranicza problem ekstrapolacji,
# ale nie oznacza, ze skrajne dane usuwamy z estymacji.
profil_modeli <- function(zmienna, rodzaj = c("czestosc", "wysokosc"),
                          n_punktow = 110L) {
  rodzaj <- match.arg(rodzaj)
  if (!(zmienna %in% c("DrivAge", "VehAge", "BonusMalus")))
    stop("Niedozwolona zmienna profilu.")

  # 'newdata' tworzymy z tabeli pol_train, aby zachowac typy kolumn.
  nowa <- pol_train[rep(1L, n_punktow), , drop = FALSE]
  nowa$DrivAge <- median(pol_train$DrivAge)
  nowa$VehAge <- median(pol_train$VehAge)
  nowa$BonusMalus <- median(pol_train$BonusMalus)
  nowa$Area <- factor(rep(area_ref, n_punktow), levels = levels(pol$Area))
  nowa$Exposure <- 1  # Dla czestosci oczekujemy 'szkod na rok'.

  q <- as.numeric(quantile(pol_train[[zmienna]], c(0.01, 0.99)))
  if (q[1] == q[2]) stop("Brak zmiennosci wybranej cechy.")
  wartosci <- seq(q[1], q[2], length.out = n_punktow)
  nowa[[zmienna]] <- wartosci

  if (rodzaj == "czestosc") {
    mdl_glm <- mN_glm
    mdl_gam <- mN_gam
    tytul <- paste("Częstość szkód: wpływ zmiennej", zmienna)
    os_y <- "Oczekiwana liczba szkód / rok ochrony"
  } else {
    mdl_glm <- mS_glm
    mdl_gam <- mS_gam
    tytul <- paste("Wysokość szkody: wpływ zmiennej", zmienna)
    os_y <- "Oczekiwana dodatnia kwota szkody [zł]"
  }

  # Przewidywania na skali predyktora; exp() przenosi je na skale sredniej.
  # Dla log-link: mu = exp(eta). W obu modelach nowa$Exposure=1.
  pred_glm <- predict(mdl_glm, newdata = nowa, type = "link")
  pred_gam <- predict(mdl_gam, newdata = nowa,
                      type = "link", se.fit = TRUE)
  d <- data.frame(
    X = wartosci,
    GLM = exp(as.numeric(pred_glm)),
    GAM = exp(as.numeric(pred_gam$fit)),
    Dol = exp(as.numeric(pred_gam$fit) - 1.96 * as.numeric(pred_gam$se.fit)),
    Gor = exp(as.numeric(pred_gam$fit) + 1.96 * as.numeric(pred_gam$se.fit))
  )
  # Jesli chodzi o czestosc, predykcja modelu poissonowskiego dla E=1
  # jest juz oczekiwana liczba szkod na 1 rok ochrony.
  etykiety_y <- if (rodzaj == "czestosc") {
    function(x) formatC(x, format = "f", digits = 3, decimal.mark = ",")
  } else {
    etykiety_liczb
  }
  ggplot(d, aes(x = X)) +
    geom_ribbon(aes(ymin = Dol, ymax = Gor), fill = kol_gam, alpha = 0.16) +
    geom_line(aes(y = GLM, colour = "GLM"), linewidth = 1.15) +
    geom_line(aes(y = GAM, colour = "GAM"), linewidth = 1.35) +
    scale_colour_manual(values = kolory_modeli) +
    scale_y_continuous(labels = etykiety_liczb) +
    labs(title = tytul,
         subtitle = paste0("Pozostałe cechy na medianach, Area = ", area_ref,
                           " | wstęga = ok. 95% PU średniej GAM"),
         x = zmienna, y = os_y,
         caption = "Profil warunkowy: zmieniamy jedną cechę, inne pozostają stałe.") +
    motyw_cw2()
}

# WYKRESY 04-06: trzy profile dla czestosci szkod.
pokaz_wykres("04_czestosc_wiek_kierowcy",
             profil_modeli("DrivAge", "czestosc"))
pokaz_wykres("05_czestosc_wiek_pojazdu",
             profil_modeli("VehAge", "czestosc"))
pokaz_wykres("06_czestosc_bonus_malus",
             profil_modeli("BonusMalus", "czestosc"))

# WYKRESY 07-09: te same cechy, ale inny parametr ryzyka (srednia szkoda).
pokaz_wykres("07_wysokosc_wiek_kierowcy",
             profil_modeli("DrivAge", "wysokosc"))
pokaz_wykres("08_wysokosc_wiek_pojazdu",
             profil_modeli("VehAge", "wysokosc"))
pokaz_wykres("09_wysokosc_bonus_malus",
             profil_modeli("BonusMalus", "wysokosc"))

# Przykład interpretacyjny: jesli krzywe GLM/GAM sa zblizone, elastycznosc GAM
# moze nie dawac znacznej korzysci. Ostateczny wybor rozstrzyga walidacja.

# 5. WALIDACJA SKLADOWYCH MODELI -------------------------------------------
# Dla modeli czestosci: Poisson deviance na WALIDACJI.
# y = liczba zdarzen; mu = przewidywana liczba zdarzen z uwzglednieniem E.
# Przy y=0 przyjmujemy skladnik y*log(y/mu) rowny 0 (granica).
dev_pois <- function(y, mu) {
  if (length(y) != length(mu) || any(!is.finite(mu)) || any(mu <= 0))
    stop("Niepoprawna prognoza Poissona.")
  sklad <- numeric(length(y))
  dodatnie <- y > 0
  sklad[dodatnie] <- y[dodatnie] * log(y[dodatnie] / mu[dodatnie])
  mean(2 * (sklad - y + mu))
}

# W modelu Gamma y>0 i mu>0; dewiancja bada wzgledne rozbieznosci.
dev_gamma <- function(y, mu) {
  if (length(y) != length(mu) || any(y <= 0) ||
      any(!is.finite(mu)) || any(mu <= 0))
    stop("Niepoprawna prognoza Gamma.")
  mean(2 * (y / mu - log(y / mu) - 1))
}

# Prognozy liczby szkod (nie rocznej czestosci!) na pol_valid.
mu_n_glm_valid <- as.numeric(predict(mN_glm, newdata = pol_valid,
                                     type = "response"))
mu_n_gam_valid <- as.numeric(predict(mN_gam, newdata = pol_valid,
                                     type = "response"))
tab_freq_valid <- data.frame(
  Model = c("GLM Poisson", "GAM Poisson"),
  MeanDeviance = c(dev_pois(pol_valid$ClaimNb, mu_n_glm_valid),
                   dev_pois(pol_valid$ClaimNb, mu_n_gam_valid)),
  OE = c(sum(pol_valid$ClaimNb) / sum(mu_n_glm_valid),
         sum(pol_valid$ClaimNb) / sum(mu_n_gam_valid))
)
cat("\nWALIDACJA CZESTOSCI:\n")
print(tab_freq_valid)

# OE < 1 -> modele przeszacowują liczbę szkód bo w mianowniku wartość oczekiwana jest większa od licznika
# OE - obserwowane / oczekiwane

# Prognozy dodatniej kwoty pojedynczej szkody na szk_valid.
mu_s_glm_valid <- as.numeric(predict(mS_glm, newdata = szk_valid,
                                     type = "response"))
mu_s_gam_valid <- as.numeric(predict(mS_gam, newdata = szk_valid,
                                     type = "response"))
tab_sev_valid <- data.frame(
  Model = c("GLM Gamma", "GAM Gamma"),
  MeanDeviance = c(dev_gamma(szk_valid$ClaimAmount, mu_s_glm_valid),
                   dev_gamma(szk_valid$ClaimAmount, mu_s_gam_valid)),
  MAE = c(mean(abs(szk_valid$ClaimAmount - mu_s_glm_valid)),
          mean(abs(szk_valid$ClaimAmount - mu_s_gam_valid)))
)
cat("\nWALIDACJA WYSOKOSCI:\n")
print(tab_sev_valid)

# Zachowujemy wyniki poprzedniej wersji: osobny wybor modeli skladowych.
# 'which.min' zwraca POZYCJE najmniejszej wartosci w tabeli walidacyjnej.
wybrany_n <- if (which.min(tab_freq_valid$MeanDeviance) == 1L) "glm" else "gam"
wybrany_s <- if (which.min(tab_sev_valid$MeanDeviance) == 1L) "glm" else "gam"
wybrany_pp_skladowe <- paste("pp", wybrany_n, wybrany_s, sep = "_")
cat("Najlepszy model czestosci (dewiancja):", wybrany_n, "\n")
cat("Najlepszy model wysokosci (dewiancja):", wybrany_s, "\n")
cat("Ich polaczenie:", wybrany_pp_skladowe, "\n")


# WYKRES 10 - jak dobrze modele odtwarzaja udzialy polis z 0,1,2,3,4+ szkodami?
# Dla roznych ekspozycji trzeba uzyc SREDNIEJ z rozkladow Poissona,
# a nie jednego rozkladu o srednim mu (mieszanina rozkladow).
kat_test <- 0:4
obs_k <- as.numeric(table(factor(pmin(as.integer(pol_valid$ClaimNb), 4L),
                                 levels = kat_test))) / nrow(pol_valid)
udz_pois <- function(mu) {
  c(vapply(0:3, function(k) mean(dpois(k, lambda = mu)), numeric(1)),
    mean(ppois(3, lambda = mu, lower.tail = FALSE)))
}
pr_k <- rbind(
  data.frame(Kategoria = c("0", "1", "2", "3", "4+"),
             Procent = 100 * obs_k, Seria = "Obserwowana"),
  data.frame(Kategoria = c("0", "1", "2", "3", "4+"),
             Procent = 100 * udz_pois(mu_n_glm_valid), Seria = "GLM"),
  data.frame(Kategoria = c("0", "1", "2", "3", "4+"),
             Procent = 100 * udz_pois(mu_n_gam_valid), Seria = "GAM")
)
pr_k$Seria <- factor(pr_k$Seria, levels = c("Obserwowana", "GLM", "GAM"))
p <- ggplot(pr_k, aes(x = Kategoria, y = Procent, fill = Seria)) +
  geom_col(position = position_dodge(width = 0.79), width = 0.72) +
  scale_fill_manual(values = kolory_modeli) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Czy modele odtwarzają rozkład liczby szkód?",
       subtitle = "Zbiór walidacyjny | porównanie udziałów obserwowanych z przewidywanymi",
       x = "Liczba szkód na polisę", y = "Udział polis [%]",
       caption = "Predykcja: średnia prawdopodobieństw Poissona dla każdej polisy z jej ekspozycją.") +
  motyw_cw2()
pokaz_wykres("10_rozklad_poissona_walidacja", p)

# 6. WYSOKOSC SZKODY - KALIBRACJA GRUPOWA ---------------------------------
# Inaczej niz dla skladki, srednia severity jest liczona po rekordach szkod,
# a nie po polisach i NIE jest wazona ekspozycja polisy.

# Funkcja przypisuje cechy ilosciowe do kategori do analizy kalibracji.
# Liczby progowe sa dydaktyczne; w rzeczywistej taryfie wymagaja uzasadnienia.
grupy_cech <- function(d, cecha) {
  if (cecha == "Area") return(factor(d$Area, levels = levels(pol$Area)))
  if (cecha == "DrivAge") {
    return(cut(d$DrivAge, breaks = c(-Inf, 24, 34, 49, 64, Inf),
               labels = c("do 24", "25-34", "35-49", "50-64", "65+")))
  }
  if (cecha == "VehAge") {
    return(cut(d$VehAge, breaks = c(-Inf, 2, 5, 10, 15, Inf),
               labels = c("0-2", "3-5", "6-10", "11-15", "16+")))
  }
  if (cecha == "BonusMalus") {
    return(cut(d$BonusMalus, breaks = c(-Inf, 60, 80, 100, 120, Inf),
               labels = c("do 60", "61-80", "81-100", "101-120", "121+")))
  }
  if (cecha == "Exposure") {
    return(cut(d$Exposure, breaks = c(-Inf, 0.25, 0.50, 0.75, 1, Inf),
               labels = c("do 0,25", "(0,25;0,50]", "(0,50;0,75]",
                          "(0,75;1]", "ponad 1")))
  }
  stop("Nieznana cecha grupujaca: ", cecha)
}

# Tworzymy dane zagregowane osobno dla kazdej grupy.
# 'n' to liczba pojedynczych SZKOD, nie liczba polis w grupie.
kalibracja_severity <- function(d, grupa) {
  g <- factor(grupa)
  lv <- levels(g)
  res <- lapply(lv, function(nazwa) {
    id <- which(as.character(g) == nazwa)
    if (!length(id)) return(NULL)
    data.frame(Grupa = nazwa, Nszkod = length(id),
               Obserwowana = mean(d$ClaimAmount[id]),
               GLM = mean(d$pred_glm[id]),
               GAM = mean(d$pred_gam[id]))
  })
  out <- do.call(rbind, res)
  if (is.null(out)) stop("Brak grup z dodatnimi szkodami.")
  out$Grupa <- factor(out$Grupa, levels = lv)
  out
}

# Dla ggplot przygotowujemy format dlugi: 3 krzywe w jednej tabeli.
wykres_severity_grupy <- function(tab, tytul, os_x) {
  d <- rbind(
    data.frame(Grupa = tab$Grupa, Kwota = tab$Obserwowana,
               Seria = "Obserwowana"),
    data.frame(Grupa = tab$Grupa, Kwota = tab$GLM, Seria = "GLM"),
    data.frame(Grupa = tab$Grupa, Kwota = tab$GAM, Seria = "GAM")
  )
  d$Seria <- factor(d$Seria, levels = c("Obserwowana", "GLM", "GAM"))
  ggplot(d, aes(x = Grupa, y = Kwota, colour = Seria, group = Seria)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 3.4) +
    scale_colour_manual(values = kolory_modeli) +
    scale_y_continuous(labels = etykiety_liczb) +
    labs(title = tytul,
         subtitle = "Zbiór walidacyjny | średnia kwota na jedną dodatnią szkodę",
         x = os_x, y = "Średnia kwota szkody [zł]",
         caption = "Dla grup o małej liczbie szkód średnia obserwowana może być niestabilna.") +
    motyw_cw2()
}

szk_valid$pred_glm <- mu_s_glm_valid
szk_valid$pred_gam <- mu_s_gam_valid

sev_wiek <- kalibracja_severity(szk_valid, grupy_cech(szk_valid, "DrivAge"))
cat("\nSEVERITY: wiek kierowcy (walidacja):\n")
print(sev_wiek)
pokaz_wykres("11_wysokosc_kalibracja_wiek",
             wykres_severity_grupy(sev_wiek,
               "Wysokość szkody – kalibracja według wieku kierowcy",
               "Wiek kierowcy [lata]"))

sev_bm <- kalibracja_severity(szk_valid, grupy_cech(szk_valid, "BonusMalus"))
cat("\nSEVERITY: BonusMalus (walidacja):\n")
print(sev_bm)
pokaz_wykres("12_wysokosc_kalibracja_BM",
             wykres_severity_grupy(sev_bm,
               "Wysokość szkody – kalibracja według BonusMalus",
               "Wskaznik BonusMalus"))

# 7. CZTERY WARIANTY DWUMODELOWEJ SKLADKI CZYSTEJ ---------------------------
# Wazne jednostki:
#  pred_N / Exposure -> szkod na rok ochrony (czestosc lambda).
#  pred_S -> zl na dodatnia szkode (severity m).
#  lambda * m -> zl na rok ochrony (skladka czysta pi).
#  E * pi -> oczekiwany laczny koszt szkod dla konkretnej polisy.
#
# Iloczyn jest zgodny z typowa taryfikacja frequency-severity, o ile
# srednia szkoda przypadajaca na zdarzenie ma zalozona interpretacje przy X.
# Roznica miedzy rejestrem dodatnich kwot a ClaimNb moze pogorszyc kalibracje.
prognozy_polis <- function(d) {
  d$lambda_glm <- as.numeric(predict(mN_glm, newdata = d, type = "response")) /
    d$Exposure
  d$lambda_gam <- as.numeric(predict(mN_gam, newdata = d, type = "response")) /
    d$Exposure
  d$severity_glm <- as.numeric(predict(mS_glm, newdata = d, type = "response"))
  d$severity_gam <- as.numeric(predict(mS_gam, newdata = d, type = "response"))
  d$pp_glm_glm <- d$lambda_glm * d$severity_glm
  d$pp_glm_gam <- d$lambda_glm * d$severity_gam
  d$pp_gam_glm <- d$lambda_gam * d$severity_glm
  d$pp_gam_gam <- d$lambda_gam * d$severity_gam
  # Obserwowana skladka czysta na jednostke ekspozycji:
  # moze byc 0 dla polisy bez szkody, ale duza przy krotkiej ekspozycji.
  d$pp_obs <- d$LossPositive / d$Exposure
  d
}

valid_p <- prognozy_polis(pol_valid)
nazwy_pp <- c("pp_glm_glm", "pp_glm_gam", "pp_gam_glm", "pp_gam_gam")

# Ważony RMSE na poziomie polisy, waga = ekspozycja E.
# W duzych szkodach RMSE jest bardzo wrazliwy na obserwacje skrajne.
# Dlatego raportujemy tez wazony MAE i O/E portfela.
ocena_taryfy <- function(d, nazwa) {
  if (!(nazwa %in% names(d))) stop("Brakuje kolumny: ", nazwa)
  pred <- d[[nazwa]]
  if (any(!is.finite(pred)) || any(pred <= 0))
    stop("Niepoprawne prognozy skladki czystej: ", nazwa)
  E <- d$Exposure
  err <- d$pp_obs - pred
  wRMSE <- sqrt(sum(E * err^2) / sum(E))
  wMAE <- sum(E * abs(err)) / sum(E)
  obs <- sum(d$LossPositive)
  prog <- sum(E * pred)
  data.frame(Model = nazwa, wRMSE = wRMSE, wMAE = wMAE,
             OE = obs / prog, ObserwowanyKoszt = obs,
             PrognozowanyKoszt = prog)
}

tab_pp_valid <- do.call(rbind, lapply(nazwy_pp,
                                     function(x) ocena_taryfy(valid_p, x)))
rownames(tab_pp_valid) <- NULL
cat("\nWALIDACJA SKLADKI CZYSTEJ - cztery kombinacje:\n")
print(tab_pp_valid)

# wRMSE (ważone) - im mniejsze tym lepsze
# wMAE (ważone) - im mniejsze tym lepsze
# OE - im bliżej 1 tym lepiej

# DWA ROZNE WYBORY: (1) iloczyn oddzielnie najlepszych modeli skladowych;
# (2) model minimalizujacy bezposrednio wRMSE skladki czystej na walidacji.
# Te warianty nie musza byc identyczne. Glownym celem jest tu skladka czysta,
# dlatego jako finalny 'wybrany_pp' wybieramy wariant z minimum wRMSE.
wybrany_pp <- as.character(tab_pp_valid$Model[
  which.min(tab_pp_valid$wRMSE)])
cat("\nWybor przez modele skladowe:", wybrany_pp_skladowe, "\n")
cat("Wybor przez wynik skladki czystej (wRMSE):", wybrany_pp, "\n")

# Odczyt z nazwy wybranej pary, ktory model czestosci i ktory severity
# zostal w niej uzyty - przyda sie przy koncowym raporcie na zbiorze testowym.
czesci <- strsplit(wybrany_pp, "_", fixed = TRUE)[[1L]]
wybrany_n_pp <- czesci[2L]
wybrany_s_pp <- czesci[3L]
stopifnot(wybrany_n_pp %in% c("glm", "gam"),
          wybrany_s_pp %in% c("glm", "gam"))

# WYKRES 13 - porownanie wRMSE czterech taryf (tylko VALID).
wynik_wykres <- tab_pp_valid
wynik_wykres$Wybor <- ifelse(wynik_wykres$Model == wybrany_pp,
                              "Najmniejszy wRMSE", "Pozostale")
p <- ggplot(wynik_wykres,
            aes(x = reorder(Model, wRMSE), y = wRMSE, fill = Wybor)) +
  geom_col(width = 0.68) +
  coord_flip() +
  scale_fill_manual(values = c("Najmniejszy wRMSE" = kol_gam,
                               "Pozostale" = "#A7B8C8")) +
  scale_y_continuous(labels = etykiety_liczb,
                     expand = expansion(mult = c(0, 0.16))) +
  geom_text(aes(label = round(wRMSE, 1)), hjust = -0.1,
            size = 4.2, colour = kol_granat) +
  labs(title = "Która taryfa najdokładniej przewiduje składkę czystą?",
       subtitle = "Cztery kombinacje GLM/GAM | zbiór walidacyjny",
       x = "Wariant: częstość + wysokość", y = "Ważony RMSE [zł/rok]",
       caption = "Wybieramy na walidacji; na teście nie modyfikujemy już modelu.") +
  motyw_cw2() + theme(legend.position = "none")
pokaz_wykres("13_porownanie_taryf_wRMSE", p)

if (wykresy_rozszerzone) {
  # WYKRES 14 - jak mocno zmieniaja sie prognozy taryfy GLM+GLM i GAM+GAM?
  # Rozklad na skali log10, bo skladka czysta ma prawy ogon.
  d_rozk <- rbind(
    data.frame(PP = valid_p$pp_glm_glm, Model = "GLM+GLM"),
    data.frame(PP = valid_p$pp_gam_gam, Model = "GAM+GAM")
  )
  p <- ggplot(d_rozk, aes(x = PP, colour = Model, fill = Model)) +
    geom_density(alpha = 0.15, linewidth = 1.1) +
    scale_x_log10(labels = etykiety_liczb) +
    scale_colour_manual(values = c("GLM+GLM" = kol_glm, "GAM+GAM" = kol_gam)) +
    scale_fill_manual(values = c("GLM+GLM" = kol_glm, "GAM+GAM" = kol_gam)) +
    labs(title = "Rozkład prognoz składki czystej",
         subtitle = "Walidacja | porównanie dwóch skrajnych kombinacji",
         x = "Prognoza składki czystej [zł/rok; skala log10]",
         y = "Gęstość",
         caption = "Wykres pokazuje różnice w segmentacji ryzyka przez modele.") +
    motyw_cw2()
  pokaz_wykres("14_rozklady_skladki_GLMM_GAMM", p)

  # WYKRES 15 - kazda kropka to polisa; wzajemne polozenie prognoz.
  # Dla czytelnosci rysujemy losowe maksymalnie 4000 polis.
  set.seed(1234)
  r <- sample.int(nrow(valid_p), min(4000L, nrow(valid_p)))
  d_por <- data.frame(GLM = valid_p$pp_glm_glm[r],
                      GAM = valid_p$pp_gam_gam[r])
  p <- ggplot(d_por, aes(x = GLM, y = GAM)) +
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", colour = kol_szary, linewidth = 0.85) +
    geom_point(colour = kol_gam, alpha = 0.27, size = 1.5) +
    scale_x_log10(labels = etykiety_liczb) +
    scale_y_log10(labels = etykiety_liczb) +
    coord_equal() +
    labs(title = "Czy GAM i GLM nadają polisom podobne składki?",
         subtitle = "Walidacja | próbka do 4000 polis | linia przerywana: równe prognozy",
         x = "Składka GLM + GLM [zł/rok; log10]",
         y = "Składka GAM + GAM [zł/rok; log10]",
         caption = "Punkty poza przekątną oznaczają różną wycenę tego samego ryzyka.") +
    motyw_cw2()
  pokaz_wykres("15_GLM_GAM_porownanie_polis", p)
}

# 8. KALIBRACJA SKLADKI: DECYL, KWINTYL, TERCYL I CECHY ---------------------
# Kalibracja polega na porownaniu:
#   OBS = sum(LossPositive) / sum(Exposure),
#   PRED = sum(Exposure * prognoza_PP) / sum(Exposure).
# Stosunek O/E = sum(LossPositive) / sum(Exposure * prognoza_PP):
#   O/E > 1 -> model ZANIZA laczny koszt grupy;
#   O/E < 1 -> model ZAWYZA laczny koszt grupy.
# 1 to idealna kalibracja agregatowa (ale nie gwarancja dobrej segregacji).

# Grupowanie wedlug rankingow prognoz: podobna LICZBA polis w kazdej grupie,
# a NIE z koniecznosci podobna ekspozycja. Remisy rozstrzygamy kolejno.
# Jest to proste i powtarzalne, chociaz przy wielu remisach granice grup
# moga zalezec od porzadku wierszy.
grupy_ryzyka <- function(yhat, liczba_grup = 10L, prefiks = "D") {
  if (length(yhat) < liczba_grup || any(!is.finite(yhat)))
    stop("Za malo obserwacji lub brak prognoz do grupowania.")
  r <- rank(yhat, ties.method = "first")
  numer <- pmin(liczba_grup, ceiling(liczba_grup * r / length(r)))
  factor(paste0(prefiks, numer),
         levels = paste0(prefiks, seq_len(liczba_grup)), ordered = TRUE)
}

# Uniwersalna tabela kalibracji dla dowolnego grupowania.
# Kazda grupa moze miec inna liczbe polis i inna sumaryczna ekspozycje.
kalibracja_grup <- function(d, nazwa_pp, grupy, typ, zbior) {
  if (!(nazwa_pp %in% names(d))) stop("Brak wariantu taryfy: ", nazwa_pp)
  g <- factor(grupy)
  lv <- levels(g)
  yhat <- d[[nazwa_pp]]
  if (length(g) != nrow(d) || any(!is.finite(yhat)) || any(yhat <= 0))
    stop("Niepoprawne grupy lub prognozy kalibracyjne.")
  wynik <- lapply(lv, function(nazwa) {
    i <- which(as.character(g) == nazwa)
    if (!length(i)) return(NULL)
    E <- sum(d$Exposure[i])
    O <- sum(d$LossPositive[i])
    P <- sum(d$Exposure[i] * yhat[i])
    data.frame(Grupa = nazwa, Npolis = length(i), Exposure = E,
               ObserwowanyKoszt = O, PrognozowanyKoszt = P,
               ObservedPP = O / E, PredictedPP = P / E,
               OE = O / P, RoznicaPP = (O - P) / E)
  })
  out <- do.call(rbind, wynik)
  if (is.null(out)) stop("Nie utworzono zadnej grupy.")
  rownames(out) <- NULL
  out$Grupa <- factor(out$Grupa, levels = lv, ordered = TRUE)
  out$Typ <- typ
  out$Zbior <- zbior
  out$Wariant <- nazwa_pp
  # W malej grupie O/E jest silnie obciazone losowoscia szkod.
  # To prog dydaktyczny, nie formalny przedzial ufnosci.
  out$MalaEkspozycja <- out$Exposure < 100
  out
}
etykiety_liczby <- etykiety_liczb
# Wykres dwoch poziomow skladki (obs i pred) dla kolejnych grup.
# Ten sam szablon posluzy do porownania decyli, cech i przedzialow ekspozycji.
wykres_kalibracji <- function(tab, tytul, podtytul = NULL) {
  d <- rbind(
    data.frame(Grupa = tab$Grupa, PP = tab$ObservedPP,
               Seria = "Obserwowana"),
    data.frame(Grupa = tab$Grupa, PP = tab$PredictedPP,
               Seria = "Prognozowana")
  )
  d$Seria <- factor(d$Seria, levels = c("Obserwowana", "Prognozowana"))
  if (is.null(podtytul)) {
    podtytul <- paste("Zbiór", unique(tab$Zbior), "| wariant", unique(tab$Wariant))
  }
  ggplot(d, aes(x = Grupa, y = PP, colour = Seria, group = Seria)) +
    geom_line(linewidth = 1.15) + geom_point(size = 3.1) +
    scale_colour_manual(values = kolory_modeli) +
    scale_y_continuous(labels = etykiety_liczby,
                       expand = expansion(mult = c(0.03, 0.12))) +
    labs(title = tytul, subtitle = podtytul,
         x = "Grupa polis", y = "Składka czysta [zł na rok ochrony]",
         caption = "W grupie: obserwowana = suma kosztów / suma E; prognoza = suma E × PP / suma E.") +
    motyw_cw2() +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
}

# Wykres O/E. Pomaranczowe kolumny oznaczaja niedoszacowanie obserwowanego
# lacznego kosztu grupy. Linia na poziomie 1 to pelna zgodnosc agregatowa.
wykres_oe <- function(tab, tytul) {
  d <- tab
  d$Ocena <- ifelse(d$OE >= 1, "Niedoszacowanie", "Przeszacowanie")
  ggplot(d, aes(x = Grupa, y = OE, fill = Ocena)) +
    geom_hline(yintercept = 1, linetype = "dashed",
               linewidth = 0.85, colour = kol_granat) +
    geom_col(width = 0.67) +
    geom_point(aes(y = OE), shape = 21, colour = "white", size = 0.5) +
    scale_fill_manual(values = c("Niedoszacowanie" = kol_obserw,
                                 "Przeszacowanie" = kol_glm)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
    labs(title = tytul,
         subtitle = paste("Zbiór", unique(tab$Zbior), "| O/E = obserwowany koszt / prognozowany koszt"),
         x = "Grupa polis", y = "O/E (1 = pełna zgodność)",
         caption = "Niskie O/E w małej grupie nie dowodzi trwałego zawyżania taryfy.") +
    motyw_cw2() + theme(axis.text.x = element_text(angle = 20, hjust = 1))
}

# WALIDACJA: klasyczne decyle prognozowanej skladki.
cal_valid <- kalibracja_grup(
  valid_p, wybrany_pp, grupy_ryzyka(valid_p[[wybrany_pp]], 10L, "D"),
  typ = "Decyle ryzyka", zbior = "valid")
cat("\nKALIBRACJA - WALIDACJA - DECYL:\n")
print(cal_valid[, c("Grupa", "Npolis", "Exposure", "ObservedPP",
                     "PredictedPP", "OE")])
pokaz_wykres("16_walidacja_decyli",
             wykres_kalibracji(cal_valid,
                               "Kalibracja taryfy – decyle ryzyka (walidacja)"))
pokaz_wykres("17_walidacja_decyli_OE",
             wykres_oe(cal_valid,
                       "Gdzie taryfa zaniża lub zawyża koszt? Decyle ryzyka"))

# Kwintyle sa przydatne, gdy w decylach wystepuja duze wahania.
cal_valid_kwintyle <- kalibracja_grup(
  valid_p, wybrany_pp, grupy_ryzyka(valid_p[[wybrany_pp]], 5L, "K"),
  typ = "Kwintyle ryzyka", zbior = "valid")
cat("\nKALIBRACJA - WALIDACJA - KWINTYLE:\n")
print(cal_valid_kwintyle[, c("Grupa", "Exposure", "ObservedPP",
                              "PredictedPP", "OE")])
pokaz_wykres("18_walidacja_kwintyli",
             wykres_kalibracji(cal_valid_kwintyle,
                               "Kalibracja taryfy – kwintyle ryzyka (walidacja)"))

# Dodatkowo tercyle: malo grup, ale duzo ekspozycji przypadajacej na grupe.
cal_valid_tercyle <- kalibracja_grup(
  valid_p, wybrany_pp, grupy_ryzyka(valid_p[[wybrany_pp]], 3L, "T"),
  typ = "Tercyle ryzyka", zbior = "valid")
print(cal_valid_tercyle[, c("Grupa", "Exposure", "ObservedPP",
                            "PredictedPP", "OE")])

# Czestosc ma tez osobna kalibracje, wyrazona w szkodach na rok ochrony.
# Ponizej oceniamy model czestosci wchodzacy w sklad FINALNEJ taryfy.
kalibracja_czestosc <- function(d, nazwa_lambda, grupy) {
  g <- factor(grupy)
  lv <- levels(g)
  lambda <- d[[nazwa_lambda]]
  res <- lapply(lv, function(nazwa) {
    i <- which(as.character(g) == nazwa)
    if (!length(i)) return(NULL)
    E <- sum(d$Exposure[i])
    O <- sum(d$ClaimNb[i])
    P <- sum(d$Exposure[i] * lambda[i])
    data.frame(Grupa = nazwa, Exposure = E, Npolis = length(i),
               Obserwowana = O / E, Prognozowana = P / E, OE = O / P)
  })
  out <- do.call(rbind, res)
  out$Grupa <- factor(out$Grupa, levels = lv, ordered = TRUE)
  out
}

kol_lambda <- paste0("lambda_", wybrany_n_pp)
cal_freq_valid <- kalibracja_czestosc(
  valid_p, kol_lambda,
  grupy_ryzyka(valid_p[[kol_lambda]], 5L, "K"))
print(cal_freq_valid)

if (wykresy_rozszerzone) {
  d <- rbind(
    data.frame(Grupa = cal_freq_valid$Grupa,
               Czestosc = cal_freq_valid$Obserwowana, Seria = "Obserwowana"),
    data.frame(Grupa = cal_freq_valid$Grupa,
               Czestosc = cal_freq_valid$Prognozowana, Seria = "Prognozowana")
  )
  p <- ggplot(d, aes(x = Grupa, y = Czestosc, colour = Seria, group = Seria)) +
    geom_line(linewidth = 1.2) + geom_point(size = 3.1) +
    scale_colour_manual(values = kolory_modeli) +
    scale_y_continuous(labels = function(x)
      formatC(x, digits = 3, format = "f", decimal.mark = ",")) +
    labs(title = "Kalibracja częstości – kwintyle przewidywanego ryzyka",
         subtitle = paste("Walidacja | model częstości w taryfie:", wybrany_n_pp),
         x = "Kwintyl ryzyka częstości", y = "Szkody na 1 rok ochrony",
         caption = "Obserwowana częstość = liczba szkód / łączna ekspozycja.") +
    motyw_cw2()
  pokaz_wykres("19_walidacja_czestosci_kwintyle", p)
}

# 9. TEST - OCENA ZAMROZONEJ TARYFY ----------------------------------------
# Dopiero TERAZ korzystamy z testu. Nie wybieramy na nim zmiennych, k ani modeli.
test_p <- prognozy_polis(pol_test)
wynik_test <- ocena_taryfy(test_p, wybrany_pp)
# Raportujemy czestosc modelu faktycznie wykorzystanego w wybranej taryfie.
kol_lambda_test <- paste0("lambda_", wybrany_n_pp)
wynik_test$O_E_freq <- sum(test_p$ClaimNb) /
  sum(test_p$Exposure * test_p[[kol_lambda_test]])
cat("\nKONCOWA OCENA NA TEScie - bez ponownego wyboru:\n")
print(wynik_test)

cal_test <- kalibracja_grup(
  test_p, wybrany_pp, grupy_ryzyka(test_p[[wybrany_pp]], 10L, "D"),
  typ = "Decyle ryzyka", zbior = "test")
cal_test_kwintyle <- kalibracja_grup(
  test_p, wybrany_pp, grupy_ryzyka(test_p[[wybrany_pp]], 5L, "K"),
  typ = "Kwintyle ryzyka", zbior = "test")
cal_test_tercyle <- kalibracja_grup(
  test_p, wybrany_pp, grupy_ryzyka(test_p[[wybrany_pp]], 3L, "T"),
  typ = "Tercyle ryzyka", zbior = "test")
cat("\nTEST: kalibracja decylowa:\n")
print(cal_test[, c("Grupa", "Npolis", "Exposure", "ObservedPP",
                   "PredictedPP", "OE")])

pokaz_wykres("20_test_decyli",
             wykres_kalibracji(cal_test,
                               "TEST: obserwowana i prognozowana składka – decyle"))
pokaz_wykres("21_test_decyli_OE",
             wykres_oe(cal_test,
                       "TEST: O/E w kolejnych decylach ryzyka"))
pokaz_wykres("22_test_kwintyli",
             wykres_kalibracji(cal_test_kwintyle,
                               "TEST: kalibracja w kwintylach ryzyka"))

# Dodatkowa kalibracja po cechach taryfowych:
#   Area, DrivAge, VehAge, BonusMalus, Exposure.
# Grupy te NIE powstaja z prognozy, lecz z obserwowanej cechy.
# Pozwala to wykryc np. systematyczne niedoszacowanie w grupie mlodych
# kierowcow, nawet gdy kalibracja wedlug decyli wyglada dobrze.
cechy_kalibracji <- c("Area", "DrivAge", "VehAge", "BonusMalus", "Exposure")
cal_test_cechy <- list()
for (cecha in cechy_kalibracji) {
  cal <- kalibracja_grup(test_p, wybrany_pp,
                         grupy_cech(test_p, cecha),
                         typ = cecha, zbior = "test")
  cal_test_cechy[[cecha]] <- cal
  cat("\nTEST - kalibracja wg", cecha, ":\n")
  print(cal[, c("Grupa", "Npolis", "Exposure", "ObservedPP",
                "PredictedPP", "OE", "MalaEkspozycja")])
  pokaz_wykres(
    paste0("test_cecha_", cecha),
    wykres_kalibracji(cal,
      paste("TEST: kalibracja składki – grupy", cecha)))
  # Dla Area i wieku kierowcy pokazujemy rowniez osobny wykres O/E.
  if (wykresy_rozszerzone && cecha %in% c("Area", "DrivAge", "BonusMalus")) {
    pokaz_wykres(
      paste0("test_OE_", cecha),
      wykres_oe(cal, paste("TEST: O/E wedlug", cecha)))
  }
}

# Dodatkowy wykres uporzadkowania ryzyka (lift): czy rosnace prognozy
# daja wyzszy OBSERWOWANY koszt w kolejnych grupach? Gdy krzywa obserwowana
# jest mocno poszarpana, wynika to czesto z duzych pojedynczych szkod.
if (wykresy_rozszerzone) {
  p <- ggplot(cal_test, aes(x = PredictedPP, y = ObservedPP)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                linewidth = 0.85, colour = kol_szary) +
    geom_path(colour = kol_glm, linewidth = 1.15) +
    geom_point(aes(size = Exposure), colour = kol_obserw, alpha = 0.90) +
    geom_text(aes(label = Grupa), nudge_y = 0.03 * max(cal_test$ObservedPP),
              colour = kol_granat, size = 3.7, check_overlap = TRUE) +
    scale_x_continuous(labels = etykiety_liczb) +
    scale_y_continuous(labels = etykiety_liczb) +
    scale_size_continuous(range = c(3.5, 8)) +
    labs(title = "TEST: odchylenia decyli od pełnej kalibracji",
         subtitle = "Punkt = decyl; wielkość punktu = suma ekspozycji",
         x = "Składka prognozowana [zł/rok]",
         y = "Składka obserwowana [zł/rok]",
         caption = "Linia przerywana: prognoza = obserwacja (idealna kalibracja).") +
    motyw_cw2() + theme(legend.position = "right")
  pokaz_wykres("test_kalibracja_punkty_decyle", p)
}

# 10. ZAPIS RAPORTOW I WYKRESOW ---------------------------------------------
# Przechowujemy wszystkie dane podsumowujace w plikach CSV; mozna otworzyc je
# w R lub w arkuszu kalkulacyjnym i dolaczyc do sprawozdania w Moodle.
write.csv(tab_freq_valid,
          file.path(folder_wynikow, "walidacja_czestosc.csv"), row.names = FALSE)
write.csv(tab_sev_valid,
          file.path(folder_wynikow, "walidacja_wysokosc.csv"), row.names = FALSE)
write.csv(tab_pp_valid,
          file.path(folder_wynikow, "walidacja_cztery_taryfy.csv"), row.names = FALSE)
write.csv(cal_valid,
          file.path(folder_wynikow, "walidacja_decyle.csv"), row.names = FALSE)
write.csv(cal_valid_kwintyle,
          file.path(folder_wynikow, "walidacja_kwintyle.csv"), row.names = FALSE)
write.csv(cal_valid_tercyle,
          file.path(folder_wynikow, "walidacja_tercyle.csv"), row.names = FALSE)
write.csv(cal_freq_valid,
          file.path(folder_wynikow, "walidacja_czestosc_kwintyle.csv"), row.names = FALSE)
write.csv(sev_wiek,
          file.path(folder_wynikow, "walidacja_severity_wiek.csv"), row.names = FALSE)
write.csv(sev_bm,
          file.path(folder_wynikow, "walidacja_severity_BonusMalus.csv"), row.names = FALSE)
write.csv(wynik_test,
          file.path(folder_wynikow, "test_wyniki_taryfy.csv"), row.names = FALSE)
write.csv(cal_test,
          file.path(folder_wynikow, "test_decyle.csv"), row.names = FALSE)
write.csv(cal_test_kwintyle,
          file.path(folder_wynikow, "test_kwintyle.csv"), row.names = FALSE)
write.csv(cal_test_tercyle,
          file.path(folder_wynikow, "test_tercyle.csv"), row.names = FALSE)
for (cecha in names(cal_test_cechy)) {
  write.csv(cal_test_cechy[[cecha]],
            file.path(folder_wynikow, paste0("test_grupy_", cecha, ".csv")),
            row.names = FALSE)
}

# Zapis do wspolnego PDF jest DODATKOWY. Wczesniej kazdy wykres zostal juz
# wyswietlony przez print(p) i zapisany jako samodzielny PNG.
if (zapisuj_wykresy) {
  wspolny_pdf <- file.path(folder_wynikow, "CW2_GLM_GAM_wszystkie_wykresy.pdf")
  # PDF jest wektorowy: wykresy mozna powiekszac bez utraty ostrosci.
  if (capabilities("cairo")) {
    grDevices::cairo_pdf(wspolny_pdf, width = 12, height = 6.75,
                         onefile = TRUE)
  } else {
    # Bez Cairo mozliwa jest ograniczona obsluga polskich znakow w PDF;
    # na ekranie i w PNG opisy pozostaja poprawne przy UTF-8.
    grDevices::pdf(wspolny_pdf, width = 12, height = 6.75,
                   onefile = TRUE, paper = "special")
  }
  for (p in rejestr_rysunkow$lista) print(p)
  grDevices::dev.off()
  cat("\nZapisano wykresy w plikach PNG i w PDF:\n", wspolny_pdf, "\n")
}

# Informacja o wybranych modelach; przydatna w Moodle/sprawozdaniu.
podsumowanie <- c(
  paste("Seed:", 1234),
  paste("Liczba polis w probie:", nrow(pol)),
  paste("Train / valid / test:", nrow(pol_train), nrow(pol_valid), nrow(pol_test)),
  paste("Dodatnie szkody w train / valid / test:",
        nrow(szk_train), nrow(szk_valid), nrow(szk_test)),
  paste("Niespojnosc liczby dodatnich szkod i ClaimNb (polisy):", rozbieznosci),
  paste("Model czestosci wybrany wg dewiancji:", wybrany_n),
  paste("Model wysokosci wybrany wg dewiancji:", wybrany_s),
  paste("Taryfa z oddzielnie wybranych skladnikow:", wybrany_pp_skladowe),
  paste("Taryfa wybrana wg wRMSE skladki:", wybrany_pp),
  paste("Test wRMSE:", round(wynik_test$wRMSE, 3)),
  paste("Test wMAE:", round(wynik_test$wMAE, 3)),
  paste("Test O/E kosztu:", round(wynik_test$OE, 3)),
  paste("Liczba wygenerowanych wykresow:", length(rejestr_rysunkow$lista)),
  "Wyniki testowe NIE byly wykorzystywane przy wyborze taryfy."
)
writeLines(podsumowanie, file.path(folder_wynikow, "podsumowanie.txt"),
           useBytes = TRUE)
cat("\n", paste(podsumowanie, collapse = "\n"), "\n")

# 11. PYTANIA DO OMOWIENIA NA ZAJECIACH -------------------------------------
# 1) Dlaczego po zbudowaniu GAM nie analizujemy wylacznie istotnosci s(x)?
#    Jaka role odgrywaja EDF oraz blad walidacyjny?
# 2) Jak interpretowac pionowa odleglosc miedzy krzywa GLM i GAM na
#    wykresach warunkowych wieku kierowcy lub BonusMalus?
# 3) Dlaczego severity nie zawiera polis bez szkody? Czy pojedyncza duza
#    szkoda moze przesunac srednia w malo licznej grupie?
# 4) Czy polaczenie najlepszych osobno modeli czestosci i wysokosci musi
#    minimalizowac blad calej skladki czystej?
# 5) Czym rozni sie porownanie w decylach ryzyka od analizy wedlug Area?
#    Ktory sposob ujawnia systematyczne niedoszacowanie w konkretnym regionie?
# 6) Dlaczego wartosci O/E trzeba interpretowac razem z liczba polis,
#    ekspozycja i liczba zarejestrowanych szkod w grupie?
# 7) Jak zmienia sie wniosek o kalibracji przy uzyciu decyli vs kwintyli?
# 8) Czy poprawa wRMSE modelu GAM oznacza, ze GAM sprawiedliwiej wycenia
#    wszystkie grupy? Nie - sprawdz osobno kalibracje przekrojowa.
#
# ZALETY PODEJSCIA: interpretowalna taryfa, mozliwosc oceny skladowych,
#                  nieliniowe profile ryzyka, przejrzysta walidacja.
# OGRANICZENIA: rozklad Poissona i Gamma, brak czynnika inflacji szkod,
#               brak zaleznosci czasowej i odrebnego modelu ogona,
#               uproszczony sumaryczny koszt dodatnich szkod.
#
# OPCJONALNA DIAGNOSTYKA GAM - uruchamiana osobno, gdy czas pozwala:
mgcv::gam.check(mN_gam)  # Czy k nie jest za male? Jak wygladaja reszty?
mgcv::gam.check(mS_gam)
mgcv::concurvity(mN_gam, full = TRUE)  # Nieliniowe analogi wspolliniowosci.
mgcv::concurvity(mS_gam, full = TRUE)
#
# KONIEC SKRYPTU.
# =============================================================================
