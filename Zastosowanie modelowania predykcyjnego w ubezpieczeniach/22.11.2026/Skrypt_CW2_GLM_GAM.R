# ============================================================================
# ZASTOSOWANIE MODELOWANIA PREDYKCYJNEGO W UBEZPIECZENIACH
# CWICZENIA 2 (135 min): TARYFIKACJA Z WYKORZYSTANIEM GLM I GAM
# Wersja z rozwiazaniami oraz szczegolowymi komentarzami dla prowadzacego.
# Zrodlo danych: CASdatasets::freMTPL2freq oraz freMTPL2sev.
# Wymagane pakiety: CASdatasets, mgcv.
# Instalacja jednorazowa:
# install.packages(c("xts", "zoo", "mgcv"))
#install.packages("CASdatasets",
#repos = "https://dutangc.perso.math.cnrs.fr/RRepository/pub/",
#type = "source")
# CASdatasets jest za duzy dla CRAN - uzywamy repozytorium autorow.
# ============================================================================

# 0. PRZYGOTOWANIE ------------------------------------------------------------
# Zatrzymujemy wykonanie, jezeli brakuje pakietow. Zapobiega to niejasnym bledom.
for (pakiet in c("CASdatasets", "mgcv")) {
  if (!requireNamespace(pakiet, quietly = TRUE)) {
    stop(paste("Brak pakietu", pakiet, "- zainstaluj go przed zajeciami."))
  }
}
# Dolaczamy mgcv, aby funkcje GAM i diagnostyka byly dostepne w sesji.
library(mgcv)

# Ustalony seed zapewnia ten sam podzial danych przy kolejnym uruchomieniu.
set.seed(1234)
# Dane dostepne w pakiecie; nie wymagamy dodatkowych plikow CSV.
data("freMTPL2freq", package = "CASdatasets")
data("freMTPL2sev", package = "CASdatasets")
pol <- freMTPL2freq
szk <- freMTPL2sev

# Kontrola istnienia kolumn: rozne zestawy freMTPL maja inne nazwy.
potrzebne_pol <- c("IDpol", "ClaimNb", "Exposure", "DrivAge", "VehAge",
                   "BonusMalus", "Area")
potrzebne_szk <- c("IDpol", "ClaimAmount")
stopifnot(all(potrzebne_pol %in% names(pol)),
          all(potrzebne_szk %in% names(szk)))

# Wspolny typ ID zapobiega problemom przy dolaczaniu danych.
pol$IDpol <- as.character(pol$IDpol)
szk$IDpol <- as.character(szk$IDpol)

# 1. KONTROLA DANYCH I PODZIAL ------------------------------------------------
cat("Liczba polis przed kontrola:", nrow(pol), "\n")
cat("Liczba rekordow szkodowych przed kontrola:", nrow(szk), "\n")
cat("Braki danych w kluczowych polach polis:\n")
print(colSums(is.na(pol[, potrzebne_pol, drop = FALSE])))

# Zachowujemy poprawne obserwacje; w szczegolnosci Exposure musi byc dodatnie,
# aby mozna bylo uzyc log(Exposure) jako offsetu.
ok <- is.finite(pol$Exposure) & pol$Exposure > 0 &
      is.finite(pol$ClaimNb) & pol$ClaimNb >= 0 &
      is.finite(pol$DrivAge) & is.finite(pol$VehAge) &
      is.finite(pol$BonusMalus) & !is.na(pol$Area) &
      !is.na(pol$IDpol) & nzchar(pol$IDpol)
pol <- pol[ok, , drop = FALSE]
if (anyDuplicated(pol$IDpol)) stop("IDpol polis nie jest unikalne.")
pol$Area <- factor(pol$Area)
cat("Liczba polis po kontroli:", nrow(pol), "\n")
print(summary(pol$Exposure))

# Dla sprawnego wykonania podczas zajec losujemy maksymalnie 80000 polis.
# Ustaw Inf zamiast 80000L, jesli chcesz uzyc pelnej proby.
limit_polis <- 80000L
if (nrow(pol) > limit_polis) {
  pol <- pol[sample.int(nrow(pol), limit_polis), , drop = FALSE]
}

# Podzial jest wykonywany JEDYNIE na poziomie polisy.
# Wszystkie szkody przypisane do tej polisy dziedzicza ten sam zbior.
n <- nrow(pol)
los <- sample.int(n)
n_train <- floor(0.60 * n)
n_valid <- floor(0.20 * n)
pol$zbior <- NA_character_
pol$zbior[los[seq_len(n_train)]] <- "train"
pol$zbior[los[n_train + seq_len(n_valid)]] <- "valid"
pol$zbior[los[(n_train + n_valid + 1L):n]] <- "test"
stopifnot(!anyNA(pol$zbior))
print(table(pol$zbior))

# Przygotowanie pojedynczych DODATNICH rekordow szkodowych do Gamma GLM/GAM.
# Nie wrzucamy polis bez szkody do severity jako obserwacji o kwocie zero.
cat("Rekordy szkody o kwocie niedodatniej lub brakujacej:",
    sum(!is.finite(szk$ClaimAmount) | szk$ClaimAmount <= 0), "\n")
szk_pos <- szk[is.finite(szk$ClaimAmount) & szk$ClaimAmount > 0, , drop = FALSE]
szk_pos <- szk_pos[szk_pos$IDpol %in% pol$IDpol, , drop = FALSE]
idx <- match(szk_pos$IDpol, pol$IDpol)
for (v in c("DrivAge", "VehAge", "BonusMalus", "Area", "zbior")) {
  szk_pos[[v]] <- pol[[v]][idx]
}
stopifnot(!anyNA(szk_pos$zbior))
# Area zachowuje ten sam zbior poziomow w probach train/valid/test.
szk_pos$Area <- factor(szk_pos$Area, levels = levels(pol$Area))

# Cel porownania skladek: suma DODATNICH zaraportowanych kwot na polise.
# To jawne uproszczenie dydaktyczne: kwoty ujemne/zerowe sa tu pomijane.
# Nie nalezy tego utozsamiac bez kontroli z pelnym kosztem ksiegowym portfela.
sumy <- aggregate(ClaimAmount ~ IDpol, data = szk_pos, FUN = sum)
names(sumy)[2] <- "LossPositive"
pos <- match(pol$IDpol, sumy$IDpol)
pol$LossPositive <- sumy$LossPositive[pos]
pol$LossPositive[is.na(pol$LossPositive)] <- 0

# Diagnostyka zgodnosci dwoch plikow: liczba zarejestrowanych dodatnich kwot
# nie musi byc identyczna z ClaimNb w tabeli polis.
licz_pozytywne <- tabulate(match(szk_pos$IDpol, pol$IDpol), nbins = nrow(pol))
pol$NumPositiveRecords <- licz_pozytywne
n_roznic <- sum(pol$ClaimNb != pol$NumPositiveRecords)
cat("Polisy z roznica ClaimNb vs liczba dodatnich rekordow:",
    n_roznic, "z", nrow(pol), "\n")
if (n_roznic > 0) {
  cat("UWAGA: liczba szkod i rejestr dodatnich kwot nie sa w pelni spojne.\n")
}

# Wyodrebniamy zbiory, zachowujac poziomy faktora Area.
pol_train <- pol[pol$zbior == "train", , drop = FALSE]
pol_valid <- pol[pol$zbior == "valid", , drop = FALSE]
pol_test  <- pol[pol$zbior == "test",  , drop = FALSE]
szk_train <- szk_pos[szk_pos$zbior == "train", , drop = FALSE]
szk_valid <- szk_pos[szk_pos$zbior == "valid", , drop = FALSE]
szk_test  <- szk_pos[szk_pos$zbior == "test",  , drop = FALSE]
cat("Dodatnie szkody train/valid/test:", nrow(szk_train),
    nrow(szk_valid), nrow(szk_test), "\n")
if (nrow(szk_train) < 200) stop("Za malo szkod dodatnich do wiarygodnego severity.")

# 2. MODELE CZESTOSCI - GLM I GAM ---------------------------------------------
# ClaimNb to liczba szkod; log(Exposure) wchodzi jako offset ze wsp. 1.
formula_n_glm <- ClaimNb ~ DrivAge + VehAge + BonusMalus + Area +
  offset(log(Exposure))
# GAM zachowuje rozklad Poissona i link log, ale pozwala na nieliniowe efekty.
formula_n_gam <- ClaimNb ~ s(DrivAge, k = 9, bs = "cr") +
  s(VehAge, k = 7, bs = "cr") + s(BonusMalus, k = 9, bs = "cr") +
  Area + offset(log(Exposure))

mN_glm <- glm(formula_n_glm,
              family = poisson(link = "log"), data = pol_train)
mN_gam <- mgcv::gam(formula_n_gam,
                    family = poisson(link = "log"),
                    method = "REML", data = pol_train)
cat("\n==== GLM czestosc ====\n")
print(summary(mN_glm)$coefficients)
cat("\n==== GAM czestosc (EDF i funkcje gladkie) ====\n")
print(summary(mN_gam))

# Wskaznik dyspersji Pearsona jest sygnalem diagnostycznym,
# nie automatycznym dowodem na koniecznosc innej rodziny.
phi_glm <- sum(residuals(mN_glm, type = "pearson")^2) / df.residual(mN_glm)
phi_gam <- sum(residuals(mN_gam, type = "pearson")^2) / df.residual(mN_gam)
cat("Dyspersja Pearsona GLM / GAM:", round(phi_glm, 3),
    "/", round(phi_gam, 3), "\n")

# 3. MODELE WYSOKOSCI SZKODY - GLM I GAM ---------------------------------------
# Jeden rekord = jedna dodatnia szkoda, dlatego nie ma tu offsetu ekspozycji.
formula_s_glm <- ClaimAmount ~ DrivAge + VehAge + BonusMalus + Area
formula_s_gam <- ClaimAmount ~ s(DrivAge, k = 8, bs = "cr") +
  s(VehAge, k = 7, bs = "cr") + s(BonusMalus, k = 8, bs = "cr") + Area
mS_glm <- glm(formula_s_glm, family = Gamma(link = "log"), data = szk_train)
mS_gam <- mgcv::gam(formula_s_gam, family = Gamma(link = "log"),
                    method = "REML", data = szk_train)
cat("\n==== GLM wysokosc ====\n")
print(summary(mS_glm)$coefficients)
cat("\n==== GAM wysokosc ====\n")
print(summary(mS_gam))

# 4. WALIDACJA MODELI SKLADOWYCH ----------------------------------------------
# Mean Poisson deviance; dla y=0 wyraz y*log(y/mu) interpretujemy jako zero.
dev_pois <- function(y, mu) {
  if (length(y) != length(mu) || any(!is.finite(mu)) || any(mu <= 0))
    stop("Niepoprawna prognoza Poissona")
  skladnik <- numeric(length(y))
  dodatnie <- y > 0
  skladnik[dodatnie] <- y[dodatnie] * log(y[dodatnie] / mu[dodatnie])
  mean(2 * (skladnik - y + mu))
}
# Mean Gamma deviance; y i mu musza byc dodatnie.
dev_gamma <- function(y, mu) {
  if (length(y) != length(mu) || any(y <= 0) ||
      any(!is.finite(mu)) || any(mu <= 0))
    stop("Niepoprawna prognoza gamma")
  mean(2 * (y / mu - log(y / mu) - 1))
}

mu_n_glm_valid <- predict(mN_glm, newdata = pol_valid, type = "response")
mu_n_gam_valid <- predict(mN_gam, newdata = pol_valid, type = "response")
tab_freq_valid <- data.frame(
  Model = c("GLM Poisson", "GAM Poisson"),
  MeanDeviance = c(dev_pois(pol_valid$ClaimNb, mu_n_glm_valid),
                   dev_pois(pol_valid$ClaimNb, mu_n_gam_valid)),
  OE = c(sum(pol_valid$ClaimNb) / sum(mu_n_glm_valid),
         sum(pol_valid$ClaimNb) / sum(mu_n_gam_valid))
)
print(tab_freq_valid)

mu_s_glm_valid <- predict(mS_glm, newdata = szk_valid, type = "response")
mu_s_gam_valid <- predict(mS_gam, newdata = szk_valid, type = "response")
tab_sev_valid <- data.frame(
  Model = c("GLM Gamma", "GAM Gamma"),
  MeanDeviance = c(dev_gamma(szk_valid$ClaimAmount, mu_s_glm_valid),
                   dev_gamma(szk_valid$ClaimAmount, mu_s_gam_valid)),
  MAE = c(mean(abs(szk_valid$ClaimAmount - mu_s_glm_valid)),
          mean(abs(szk_valid$ClaimAmount - mu_s_gam_valid)))
)
print(tab_sev_valid)

# Wybieramy specyfikacje TYLKO na walidacji; test jest jeszcze nietkniety.
wybrany_n <- if (which.min(tab_freq_valid$MeanDeviance) == 1L) "glm" else "gam"
wybrany_s <- if (which.min(tab_sev_valid$MeanDeviance) == 1L) "glm" else "gam"
wybrany_pp <- paste("pp", wybrany_n, wybrany_s, sep = "_")
cat("Wybrany model czestosci:", wybrany_n,
    "; wysokosci:", wybrany_s, "\n")

# 5. KONSTRUKCJA SKLADKI CZYSTEJ ----------------------------------------------
# W jednym miejscu wykonujemy cztery kombinacje modeli na TYCH SAMYCH polisach.
# Oczekiwana liczba szkod z predykcji Poissona zawiera ekspozycje;
# dzielimy ja przez Exposure, aby otrzymac roczna czestosc.
prognozy_polis <- function(d) {
  d$lambda_glm <- predict(mN_glm, newdata = d, type = "response") / d$Exposure
  d$lambda_gam <- predict(mN_gam, newdata = d, type = "response") / d$Exposure
  # Severity dla kazdej polisy to oczekiwana kwota w razie wystapienia szkody.
  d$severity_glm <- predict(mS_glm, newdata = d, type = "response")
  d$severity_gam <- predict(mS_gam, newdata = d, type = "response")
  d$pp_glm_glm <- d$lambda_glm * d$severity_glm
  d$pp_glm_gam <- d$lambda_glm * d$severity_gam
  d$pp_gam_glm <- d$lambda_gam * d$severity_glm
  d$pp_gam_gam <- d$lambda_gam * d$severity_gam
  d$pp_obs <- d$LossPositive / d$Exposure
  d
}
valid_p <- prognozy_polis(pol_valid)

# Metryki na poziomie polisy i ekspozycji. O/E bliskie 1 jest pozadane.
# RMSE wazone ekspozycja mocno reaguje na duze szkody; interpretuj je ostroznie.
ocena_taryfy <- function(d, nazwa) {
  pred <- d[[nazwa]]
  e <- d$Exposure
  wRMSE <- sqrt(sum(e * (d$pp_obs - pred)^2) / sum(e))
  O_E <- sum(d$LossPositive) / sum(e * pred)
  data.frame(Model = nazwa, wRMSE = wRMSE, OE = O_E)
}
nazwy_pp <- c("pp_glm_glm", "pp_glm_gam", "pp_gam_glm", "pp_gam_gam")
tab_pp_valid <- do.call(rbind, lapply(nazwy_pp, function(z) ocena_taryfy(valid_p, z)))
print(tab_pp_valid)
cat("Wariant wybrany na podstawie modeli skladowych:", wybrany_pp, "\n")

# 6. KALIBRACJA W GRUPACH RYZYKA ----------------------------------------------
# Podzial na dziesiec grup o zblizonej liczbie polis, po prognozowanej skladce.
# Remisy rozstrzygamy stabilnie; przy masowych remisach rozpatrz takze grupy taryfowe.
kalibracja_decyle <- function(d, nazwa) {
  yhat <- d[[nazwa]]
  r <- rank(yhat, ties.method = "first")
  decyl <- pmin(10L, ceiling(10 * r / nrow(d)))
  eksp <- as.numeric(rowsum(d$Exposure, decyl, reorder = TRUE))
  koszt <- as.numeric(rowsum(d$LossPositive, decyl, reorder = TRUE))
  prognoza <- as.numeric(rowsum(d$Exposure * yhat, decyl, reorder = TRUE))
  out <- data.frame(Decyl = seq_along(eksp), Exposure = eksp,
                    ObservedPP = koszt / eksp,
                    PredictedPP = prognoza / eksp,
                    OE = koszt / pmax(prognoza, 1e-12))
  out
}
cal_valid <- kalibracja_decyle(valid_p, wybrany_pp)
print(cal_valid)

# Wykresy zapisujemy do pliku PDF - w celu odtworzenia na sali dydaktycznej.
pdf("CW2_GLM_GAM_wykresy.pdf", width = 10, height = 6)
plot(mN_gam, pages = 1, shade = TRUE, main = "Czestosc: GAM Poissona")
plot(mS_gam, pages = 1, shade = TRUE, main = "Wysokosc: GAM gamma")
plot(cal_valid$Decyl, cal_valid$ObservedPP, type = "b", pch = 16,
     xlab = "Decyl prognozowanego ryzyka (walidacja)",
     ylab = "Skladka czysta na jednostke ekspozycji",
     main = "Kalibracja taryfy na walidacji")
lines(cal_valid$Decyl, cal_valid$PredictedPP, type = "b", lty = 2, pch = 1)
legend("topleft", legend = c("Obserwowana", "Prognozowana"),
       lty = c(1, 2), pch = c(16, 1), bty = "n")
dev.off()

# Opcjonalna diagnostyka GAM: sprawdz adekwatnosc wymiaru bazy k.
# Funkcja gam.check() wyswietla informacje i wykresy; uruchom w konsoli.
# mgcv::gam.check(mN_gam)
# mgcv::gam.check(mS_gam)

# 7. KONCOWY TEST - BEZ PONOWNEGO WYBORU --------------------------------------
# Nie modyfikujemy juz modelu po zobaczeniu wynikow testowych.
test_p <- prognozy_polis(pol_test)
wynik_test <- ocena_taryfy(test_p, wybrany_pp)
pred_n_test <- if (wybrany_n == "glm") {
  predict(mN_glm, newdata = pol_test, type = "response")
} else {
  predict(mN_gam, newdata = pol_test, type = "response")
}
wynik_test$O_E_freq <- sum(pol_test$ClaimNb) / sum(pred_n_test)
print(wynik_test)
cal_test <- kalibracja_decyle(test_p, wybrany_pp)
print(cal_test)

# Eksport dla sprawozdania. Wartosci liczbowe pojawia sie po uruchomieniu R.
write.csv(tab_freq_valid, "CW2_walidacja_czestosc.csv", row.names = FALSE)
write.csv(tab_sev_valid,  "CW2_walidacja_wysokosc.csv", row.names = FALSE)
write.csv(tab_pp_valid,   "CW2_walidacja_taryfa.csv", row.names = FALSE)
write.csv(cal_test,       "CW2_test_decyle.csv", row.names = FALSE)
cat("Zakonczono. Test wykorzystano tylko do koncowej oceny.\n")

# PYTANIA DLA STUDENTA:
# 1) Czy EDF dla wszystkich funkcji gladkich jest wyraznie wieksze od 1?
# 2) Czy obnizenie dewiancji GAM jest dostateczne, by uzasadnic zlozonosc?
# 3) Czy O/E w najwyzszym decylu wskazuje na niedoszacowanie ryzyka?
# 4) Dlaczego skladka czysta nie obejmuje narzutow i marzy ubezpieczyciela?
