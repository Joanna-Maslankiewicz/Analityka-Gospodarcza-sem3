# --- ZADANIE 1. Import, rozpoznanie danych i eksploracja ---------

plik <- "dane_samodzielnie.csv"                    # Nazwa pliku z obserwacjami polis.
if (!file.exists(plik)) {                     # Sprawdzamy, czy R widzi plik w katalogu.
  stop("Umiesc freMTPL2freq.csv w katalogu roboczym (getwd()).") # Przerywamy przy braku pliku.
}

raw <- read.csv2(plik, stringsAsFactors = FALSE, na.strings = c("", "NA")) # Czytamy sredniki i dziesietne przecinki.
names(raw)                                    # Wyswietlamy nazwy zmiennych.
dim(raw)                                      # Sprawdzamy liczbe wierszy i kolumn.
head(raw)                                     # Ogladamy pierwszych 6 obserwacji.
str(raw)                                      # Odczytujemy typy kolumn (numeric/character).

potrzebne <- c("veh_value", "exposure", "numclaims", "claimcst0", "veh_body", "veh_age", "gender", "area", "agecat") # Ciag dalszy spisu.
stopifnot(all(potrzebne %in% names(raw)))     # Przerywamy, gdy format danych nie pasuje do zadan.

summary(raw[c("veh_value", "exposure", "numclaims", "claimcst0", "veh_body", "veh_age", "gender", "area", "agecat")])
liczba_brakow <- colSums(is.na(raw))         # Zliczamy NA oddzielnie dla kazdej kolumny.
print(liczba_brakow)                          # Pokazujemy wyniki kontroli brakow.

tabela_szkod <- table(ifelse(raw$numclaims >= 2, "2+", as.character(raw$numclaims)), # Grupujemy duze liczby szkod.
                      useNA = "ifany")      # Nie ukrywamy brakujacych wartosci.
print(tabela_szkod)                           # Odczytujemy dominacje polis bez szkod.
barplot(tabela_szkod, main = "Liczba szkod na polise", # Rysujemy wykres rozkladu.
        xlab = "numclaims: 0, 1, 2+", ylab = "Liczba polis") # Ustalamy opisy osi.

# dodatkowo:
czestosc_calosc <- sum(raw$numclaims) / sum(raw$exposure) # Zdarzenia na poliso-rok.
cat("Czestosc portfela [szkody / poliso-rok]:", round(czestosc_calosc, 5), "\n") # Jednostka ma znaczenie.
cat("Udzial polis bez szkody:", round(mean(raw$numclaims == 0), 4), "\n") # Ta miara NIE jest czestoscia roczna.

# --- ZADANIE 2. Kontrola jakosci i bezpieczne czyszczenie  ------

zla_ekspozycja <- is.na(raw$exposure) | !is.finite(raw$exposure) | raw$exposure <= 0 # Ekspozycja musi byc dodatnia.
zla_liczba_szkod <- is.na(raw$numclaims) | !is.finite(raw$numclaims) | # Liczba szkod musi byc okreslona.
                    raw$numclaims < 0 | raw$numclaims != floor(raw$numclaims) # Odrzucamy liczby ujemne i ulamkowe.
#liczba_ID_powtorzonych <- sum(duplicated(raw$IDpol)) # Powtorzenia ID wymagaja wyjasnienia, NIE kasujemy ich w ciemno.
liczba_wierszy_powtorzonych <- sum(duplicated(raw)) # Identyczne profile nie musza oznaczac tej samej polisy.
ile_duzych_ekspozycji <- sum(raw$exposure > 1, na.rm = TRUE) # Ekspozycje powyzej roku jedynie SYGNALIZUJEMY.

raport_audytu <- data.frame(                          # Zbieramy obserwowane problemy w jednej tabeli.
  Problem = c("Braki danych (suma komorek)", "exposure <= 0 / niefinitywna", # Nazwy flag.
              "Nieprawidlowa numclaims", # Dalsze nazwy flag.
              "Wiersze identyczne", "exposure > 1"),   # Ostatnie nazwy flag.
  Liczba = c(sum(is.na(raw)), sum(zla_ekspozycja), sum(zla_liczba_szkod), # Licznosci pierwszych flag.
             liczba_wierszy_powtorzonych, # Licznosci powtorzen.
             ile_duzych_ekspozycji)                       # Liczba polis z ekspozycja ponad rok.
)
print(raport_audytu)                                  # Wyswietlamy raport przed czyszczeniem.

kompletne <- complete.cases(raw[potrzebne])           # Zaznaczamy rekordy bez brakow w kolumnach modelowych.
poprawne <- kompletne & !zla_ekspozycja & !zla_liczba_szkod & raw$veh_age >= 0     # Eliminujemy nielogiczne wartosci cech pojazdu.
poprawne[is.na(poprawne)] <- FALSE                    # Brak decyzji o poprawnosci traktujemy jako problem.

do_weryfikacji <- raw[!poprawne, , drop = FALSE]      # Zachowujemy problematyczne rekordy do audytu.
dane <- raw[poprawne, , drop = FALSE]                 # Budujemy zbior do dalszej pracy.
cat("Odrzucono:", nrow(do_weryfikacji), "wierszy; pozostalo:", nrow(dane), "\n") # Pokazujemy efekt.
write.csv2(do_weryfikacji, "C1_rekordy_do_weryfikacji.csv", row.names = FALSE) # Eksportujemy wiersze z flagami.

# UWAGA: exposure > 1 moze byc poprawne - nie obcinamy jej bez uzasadnienia.
### można to rozumiec że mamy dane z dwóch lat
# UWAGA: nie usuwamy powtorzonych pelnych wierszy bez identyfikacji polisy.
# UWAGA: IDpol nie jest predyktorem! To identyfikator rekordu, nie cecha ryzyka.

# Liczymy bazowa czestosc jako suma szkod / suma czasu ochrony, a nie
# jako srednia z indywidualnych numclaims / exposure.
czestosc_calosc <- sum(dane$numclaims) / sum(dane$exposure) # Zdarzenia na poliso-rok.
cat("Czestosc portfela [szkody / poliso-rok]:", round(czestosc_calosc, 5), "\n") # Jednostka ma znaczenie.
cat("Udzial polis bez szkody:", round(mean(dane$numclaims == 0), 4), "\n") # Ta miara NIE jest czestoscia roczna.

# --- ZADANIE 3. Transformacje i wskazniki segmentowe  ---------

dane$Area <- factor(dane$Area)                      # Area jest kategoria, nie wartoscia liczbowa.
dane$VehGas <- factor(dane$VehGas)                  # Rodzaj paliwa kodujemy jako czynnik.
dane$DrivAge_grupa <- cut(dane$DrivAge,            # Grupujemy wiek kierowcy wedlug progow.
                          breaks = c(17, 24, 34, 49, 64, Inf), # Granice grup, lewostronnie otwarte.
                          labels = c("18-24", "25-34", "35-49", "50-64", "65+")) # Nazwy grup.
dane$veh_age_grupa <- cut(dane$veh_age,              # Wiek pojazdu moze dzialac nieliniowo.
                         breaks = c(-Inf, 0, 2, 5, 10, Inf), # Zero i kolejne przedzialy.
                         labels = c("0", "1-2", "3-5", "6-10", "11+")) # Etykiety grup wieku.
dane$BM10 <- (dane$BonusMalus - 100) / 10         # Jedna jednostka BM10 oznacza +10 punktow bonus-malus.
dane$logDensity <- log1p(dane$Density)           # Log(1 + gestosc) obsluguje takze gestosc rowna 0.

tab_area <- aggregate(cbind(numclaims, exposure) ~ Area, # Najpierw sumujemy obie wielkosci.
                      data = dane, FUN = sum)     # Jedna obserwacja tabeli = jedna kategoria Area.
tab_area$Czestosc <- tab_area$numclaims / tab_area$exposure # Dzielimy sumy, uzyskujac stawke roczna.
print(tab_area[order(-tab_area$Czestosc), ])       # Sortujemy od najwyzszej czestosci.
barplot(tab_area$Czestosc, names.arg = tab_area$Area, # Wizualizujemy stawki dla obszarow.
        xlab = "Area", ylab = "Szkody / poliso-rok", # Jasno okreslamy jednostke pionowej osi.
        main = "Czestosc roczna wedlug Area")     # Tytul wykresu.

# --- ZADANIE 4. Podzial prob i uczenie modeli ---------------

set.seed(123)                                     # Ustalamy ziarno dla powtarzalnego podzialu.
kolejnosc <- sample.int(nrow(dane))              # Losujemy indeksy wierszy bez zwracania.
n <- nrow(dane)                                   # Liczba rekordow po czyszczeniu.
n_ucz <- floor(0.60 * n)                         # 60% danych przeznaczamy do uczenia.
n_wal <- floor(0.20 * n)                         # 20% danych przeznaczamy do walidacji.
id_ucz <- kolejnosc[seq_len(n_ucz)]              # Pierwsza grupa indeksow to uczenie.
id_wal <- kolejnosc[seq.int(n_ucz + 1, n_ucz + n_wal)] # Druga grupa to walidacja.
id_test <- kolejnosc[seq.int(n_ucz + n_wal + 1, n)] # Reszta tworzy nietkniety zbior testowy.
uczenie <- dane[id_ucz, , drop = FALSE]          # Wyodrebniamy dane uczace.
walidacja <- dane[id_wal, , drop = FALSE]        # Wyodrebniamy dane walidacyjne.
test <- dane[id_test, , drop = FALSE]            # Wyodrebniamy dane testowe.
cat("Podzial - uczenie, walidacja, test:", nrow(uczenie), nrow(walidacja), nrow(test), "\n") # Kontrola.

# Nie modelujemy numclaims/exposure jako zmiennej objasnianej: liczba szkod
# jest Y, a roznice ekspozycji uwzglednia OFFSET log(exposure).
# E[numclaims_i | X_i] = exposure_i * exp(beta0 + X_i * beta).

m0 <- glm(numclaims ~ 1 + offset(log(exposure)),   # M0: jedna stala stawka na poliso-rok.
          family = poisson(link = "log"), data = uczenie) # Poisson z logarytmicznym linkiem.
m1 <- glm(numclaims ~ Area + DrivAge_grupa + veh_age_grupa + # M1: podstawowe cechy taryfowe.
            VehGas + BM10 + offset(log(exposure)), # BM10 traktujemy jako zmienna liczbowa.
          family = poisson(link = "log"), data = uczenie) # Dopasowanie do probki uczacej.
m2 <- glm(numclaims ~ Area + DrivAge_grupa + veh_age_grupa + # M2: model wzbogacony.
            VehGas + BM10 + logDensity + VehPower + # Dodajemy gestosc zaludnienia i moc auta.
            offset(log(exposure)),               # Ekspozycja wciaz ma ustalony wspolczynnik 1.
          family = poisson(link = "log"), data = uczenie) # Uczymy trzeci model.

summary(m1)                                     # Wyswietlamy oszacowania, SE, z i wartosci p.
summary(m2)                                     # Ogladamy dodatkowe czynniki w M2.
exp(coef(m2))                                   # Przeliczamy beta na mnozniki stawki IRR.
cat("IRR za +10 pkt BonusMalus (M2):", round(exp(coef(m2)["BM10"]), 4), "\n") # Interpretacja BM10.

# UWAGA: IRR = exp(beta) jest relacja oczekiwanej CZESTOSCI rocznej
# przy innych zmiennych stalych; nie oznacza zaleznosci przyczynowej.
# UWAGA: exp(beta) dla Area dotyczy porownania z kategoria referencyjna.
cat("AreaB (M2):", round(exp(coef(m2)["AreaB"]), 4), "\n") # Interpretacja BM10.


# --- ZADANIE 5. Walidacja -> wybor -> jednorazowy test  ------

walidacja$mu0 <- predict(m0, newdata = walidacja, type = "response") # M0 przewiduje liczbe szkod na okres ochrony.
walidacja$mu1 <- predict(m1, newdata = walidacja, type = "response") # Prognozy z modelu M1.
walidacja$mu2 <- predict(m2, newdata = walidacja, type = "response") # Prognozy z modelu M2.
yw <- walidacja$numclaims                       # Wektor rzeczywistej liczby szkod w walidacji.

# Deviance Poissona w pojedynczym wierszu: 2*[y log(y/mu) - (y-mu)].
# Dla y=0 pierwsza czesc z definicji wynosi zero; pmax(y,1) zapewnia 0*log(1/mu)=0.
dev0 <- mean(2 * (yw * log(pmax(yw, 1) / pmax(walidacja$mu0, 1e-12)) - (yw - walidacja$mu0))) # M0.
dev1 <- mean(2 * (yw * log(pmax(yw, 1) / pmax(walidacja$mu1, 1e-12)) - (yw - walidacja$mu1))) # M1.
dev2 <- mean(2 * (yw * log(pmax(yw, 1) / pmax(walidacja$mu2, 1e-12)) - (yw - walidacja$mu2))) # M2.

wyniki_wal <- data.frame(                      # Budujemy wspolna tabele jakosci na walidacji.
  Model = c("M0", "M1", "M2"),               # Nazwy modeli.
  MAE = c(mean(abs(yw - walidacja$mu0)), mean(abs(yw - walidacja$mu1)), # Sredni blad bezwzgledny.
          mean(abs(yw - walidacja$mu2))),      # MAE modelu M2.
  RMSE = c(sqrt(mean((yw - walidacja$mu0)^2)), # Pierwiastek ze sredniego bledu kwadratowego M0.
           sqrt(mean((yw - walidacja$mu1)^2)), # RMSE modelu M1.
           sqrt(mean((yw - walidacja$mu2)^2))), # RMSE modelu M2.
  Srednia_dewiancja = c(dev0, dev1, dev2)     # Glowna miara selekcji: mniejsza = lepiej.
)
print(wyniki_wal)                               # Porownujemy wszystkie modele na TYCH SAMYCH wierszach.

najlepszy_napis <- wyniki_wal$Model[which.min(wyniki_wal$Srednia_dewiancja)] # Wybor bez korzystania z testu.
wybrany_model <- switch(najlepszy_napis, M0 = m0, M1 = m1, M2 = m2) # Pobieramy obiekt modelu.
cat("Wybor wg walidacji:", najlepszy_napis, "\n") # Informujemy o decyzji.

test$mu0 <- predict(m0, newdata = test, type = "response") # Punkt odniesienia na probce testowej.
test$mu <- predict(wybrany_model, newdata = test, type = "response") # Prognoza wybranego modelu na tescie.
yt <- test$numclaims                              # Faktyczna liczba szkod w zbiorze testowym.

test_mae <- mean(abs(yt - test$mu))             # MAE wybranego modelu na nietknietym tescie.
test_rmse <- sqrt(mean((yt - test$mu)^2))       # RMSE wybranego modelu na tescie.
test_dev <- mean(2 * (yt * log(pmax(yt, 1) / pmax(test$mu, 1e-12)) - # Iloraz wewnatrz log jest dodatni.
                       (yt - test$mu)))         # Srednia dewiancja Poissona wybranego modelu.
baseline_dev <- mean(2 * (yt * log(pmax(yt, 1) / pmax(test$mu0, 1e-12)) - # Tak samo dla M0.
                           (yt - test$mu0)))    # Wynik baseline na tescie.

cat("TEST - wybrany model:", najlepszy_napis, "\n") # Wskazujemy testowany model.
cat("TEST - MAE:", round(test_mae, 5), "RMSE:", round(test_rmse, 5), "\n") # Dwie miary bledu.
cat("TEST - dewiancja modelu:", round(test_dev, 5), "baseline:", round(baseline_dev, 5), "\n") # Porownanie.
cat("TEST - czestosc empiryczna:", round(sum(yt) / sum(test$exposure), 5), "\n") # Stawka roczna rzeczywista.
cat("TEST - czestosc prognozowana:", round(sum(test$mu) / sum(test$exposure), 5), "\n") # Stawka roczna przewidywana.

# --- ZADANIE 6. Diagnostyka modelu i kalibracja  ------------

phi <- sum(residuals(wybrany_model, type = "pearson")^2) / df.residual(wybrany_model) # Dyspersja Pearsona.
cat("Wspolczynnik dyspersji Pearsona:", round(phi, 4), "\n") # Wartosc wyraznie >1 sugeruje naddyspersje.

udzial_zer_obs <- mean(yt == 0)               # Faktyczny odsetek zer w zbiorze testowym.
udzial_zer_prog <- mean(exp(-test$mu))         # Dla Poissona P(Y=0 | mu)=exp(-mu).
cat("Odsetek zer: obserwowany", round(udzial_zer_obs, 4), # Porownujemy duza mase zer.
    "; prognozowany", round(udzial_zer_prog, 4), "\n") # Moze wskazywac niedopasowanie.

kalibracja_area <- aggregate(cbind(numclaims, exposure, mu) ~ Area, # Grupujemy obserwacje i prognozy po Area.
                            data = test, FUN = sum) # Sumujemy liczby szkod, ekspozycje i prognozy.
kalibracja_area$Obs <- kalibracja_area$numclaims / kalibracja_area$exposure # Stawka empiryczna.
kalibracja_area$Pred <- kalibracja_area$mu / kalibracja_area$exposure # Stawka przewidywana.
print(kalibracja_area)                          # Ocenia sie poziom i rozklad kalibracji.

matplot(seq_len(nrow(kalibracja_area)),         # Oś X zawiera kategorie Area.
        cbind(kalibracja_area$Obs, kalibracja_area$Pred), # Dwie serie: dane i model.
        type = "b", pch = c(16, 1), lty = c(1, 2), lwd = 2, # Rozrozniamy linie i pogrubiamy je.
        col = c("steelblue4", "darkorange3"), xaxt = "n", # Rozrozniamy serie kolorami i wylaczamy os X.
        xlab = "Area", ylab = "Szkody / poliso-rok", # Wspolna jednostka dla obu serii.
        main = "Kalibracja w segmentach Area: test") # Tytul wykresu.
axis(1, at = seq_len(nrow(kalibracja_area)), labels = kalibracja_area$Area) # Dodajemy etykiety Area.
legend("topright", legend = c("Obserwowane", "Prognozowane"), # Wyjasniamy znaczenie serii.
       pch = c(16, 1), lty = c(1, 2), lwd = 2, # Styl zgodny z wykresem.
       col = c("steelblue4", "darkorange3"), bty = "n") # Bez ramki.

set.seed(123)                                  # Ustalamy probe do czytelnego wykresu reszt.
ind_reszt <- sample.int(nrow(uczenie), min(5000, nrow(uczenie))) # Losujemy maksymalnie 5000 obserwacji.
plot(fitted(wybrany_model)[ind_reszt],        # Na osi X wartosci dopasowane mu.
     residuals(wybrany_model, type = "pearson")[ind_reszt], # Na osi Y reszty Pearsona.
     pch = 16, cex = 0.4, xlab = "Oczekiwana liczba szkod (mu)", # Male punkty sa czytelniejsze.
     ylab = "Reszta Pearsona", main = "Diagnostyka: reszty vs prognozy") # Opis wykresu.
abline(h = 0, lty = 2)                        # Linia referencyjna odpowiada reszcie zero.

write.csv2(wyniki_wal, "C1_wyniki_walidacji.csv", row.names = FALSE) # Archiwizujemy tabele porownania.
write.csv2(kalibracja_area, "C1_kalibracja_test_area.csv", row.names = FALSE) # Archiwizujemy kalibracje.
saveRDS(wybrany_model, "C1_model_poissona.rds") # Zachowujemy wybrany model do dalszych zajec.

# WNIOSKI DO DYSKUSJI:
# 1) Czy model M1 lub M2 przewiduje lepiej niz M0 na walidacji i tescie?
# 2) Czy MAE i dewiancja Poissona prowadza do takiego samego rankingu?
# 3) Czy dyspersja Pearsona jest zblizona do 1? Co oznacza, jezeli nie?
# 4) Czy obserwowana i przewidywana czestosc sa podobne w kazdej Area?
# 5) Czy podzial losowy wystarczylby, gdyby dane mialy kolejny rok obserwacji?
