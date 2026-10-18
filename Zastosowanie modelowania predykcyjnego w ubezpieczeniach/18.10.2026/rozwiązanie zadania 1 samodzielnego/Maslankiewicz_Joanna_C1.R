### Import

plik = "dane_samodzielnie.csv"
if (!file.exists(plik)) {
  stop("Umiesc freMTPL2freq.csv w katalogu roboczym (getwd()).")
}

raw = read.csv2(plik, stringsAsFactors = FALSE, na.strings = c("", "NA"))
names(raw)
dim(raw)
head(raw)
str(raw)

### Zadanie A

potrzebne = c("veh_value", "exposure", "numclaims", "claimcst0", "veh_body", "veh_age", "gender", "area", "agecat")
stopifnot(all(potrzebne %in% names(raw)))

cat("Liczba rekordów przed czyszczeniem:", nrow(raw), "\n")
cat("Liczba braków NA (łącznie):", sum(is.na(raw)), "\n")

zla_ekspozycja = is.na(raw$exposure) | !is.finite(raw$exposure) | raw$exposure <= 0
zla_liczba_szkod = is.na(raw$numclaims) | !is.finite(raw$numclaims) | raw$numclaims < 0 | raw$numclaims != floor(raw$numclaims)

cat("Rekordy z exposure <= 0:", sum(zla_ekspozycja), "\n")
cat("Wśród nich: numclaims > 0:", sum(zla_ekspozycja & !is.na(raw$numclaims) & raw$numclaims > 0), "\n")

raport_audytu = data.frame(
  Problem = c("Braki NA (suma komórek)", "exposure <= 0 lub niefinitywna",
              "Niepoprawna numclaims", "Identyczne wiersze", "exposure > 1"),
  Liczba = c(sum(is.na(raw)), sum(zla_ekspozycja), sum(zla_liczba_szkod),
             sum(duplicated(raw)), sum(raw$exposure > 1, na.rm = TRUE))
)
print(raport_audytu)

### Zadanie B

kompletne = complete.cases(raw[potrzebne])
poprawne = kompletne & !zla_ekspozycja & !zla_liczba_szkod & raw$veh_age >= 0
poprawne[is.na(poprawne)] = FALSE

do_weryfikacji = raw[!poprawne, , drop = FALSE]
dane = raw[poprawne, , drop = FALSE]

dane$area = factor(dane$area, levels = c("A", "B", "C", "D", "E", "F"))
dane$agecat = factor(dane$agecat)
dane$veh_age = factor(dane$veh_age)
dane$gender = factor(dane$gender)

write.csv2(do_weryfikacji, "C1_rekordy_do_weryfikacji.csv", row.names = FALSE)

cat("Odrzucono:", nrow(do_weryfikacji), "wierszy; po czyszczeniu:", nrow(dane), "\n")
cat("Łączna liczba szkód:", sum(dane$numclaims), "\n")
cat("Suma ekspozycji [poliso-lata]:", sum(dane$exposure), "\n")
czestosc_calosc = sum(dane$numclaims) / sum(dane$exposure)
cat("Roczna częstość [szkody / poliso-rok]:", round(czestosc_calosc, 6), "\n")

### Zadanie C

tab_area = aggregate(cbind(numclaims, exposure) ~ area, data = dane, FUN = sum)
tab_area$Czestosc = tab_area$numclaims / tab_area$exposure
tab_area = tab_area[order(tab_area$area), ]
print(tab_area)
cat("Area o najwyższej częstości:", as.character(tab_area$area[which.max(tab_area$Czestosc)]), "\n")

png("C1_czestosc_wedlug_Area.png", width = 1000, height = 650, res = 120)
barplot(tab_area$Czestosc, names.arg = tab_area$area,
        xlab = "Area", ylab = "Szkody / poliso-rok",
        main = "Roczna częstość szkód według Area",
        ylim = c(0.0, 0.2))
dev.off()

### Zadanie D

set.seed(123)
n = nrow(dane)
kolejnosc = sample.int(n)
n_ucz = floor(0.70 * n)
id_ucz = kolejnosc[seq_len(n_ucz)]
id_kontrola = kolejnosc[seq.int(n_ucz + 1, n)]
uczenie = dane[id_ucz, , drop = FALSE]
kontrola = dane[id_kontrola, , drop = FALSE]
cat("Podział - uczenie, kontrola:", nrow(uczenie), nrow(kontrola), "\n")

#modele
m0 = glm(numclaims ~ offset(log(exposure)),
         family = poisson(link = "log"), data = uczenie)
m1 = glm(numclaims ~ area + agecat + veh_age + gender + veh_value + offset(log(exposure)),
         family = poisson(link = "log"), data = uczenie)

summary(m0)
summary(m1)

#prognozy dla obserwacji kontrolnych
kontrola$mu0 = predict(m0, newdata = kontrola, type = "response")
kontrola$mu1 = predict(m1, newdata = kontrola, type = "response")
y = kontrola$numclaims

mae0 = mean(abs(y - kontrola$mu0))
mae1 = mean(abs(y - kontrola$mu1))
dev0 = 2 * mean(y * log(pmax(y, 1) / pmax(kontrola$mu0, 1e-12)) - (y - kontrola$mu0))
dev1 = 2 * mean(y * log(pmax(y, 1) / pmax(kontrola$mu1, 1e-12)) - (y - kontrola$mu1))

wyniki = data.frame(
  Model = c("M0 - stała częstość", "M1 - z cechami taryfowymi"),
  MAE = c(mae0, mae1),
  Srednia_dewiancja_Poissona = c(dev0, dev1)
)
print(wyniki)
cat("Model lepszy według średniej dewiancji:", wyniki$Model[which.min(wyniki$Srednia_dewiancja_Poissona)], "\n")
