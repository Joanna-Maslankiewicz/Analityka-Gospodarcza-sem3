############################################################
#   Drugie zajęcia z metod aktuarialnych — modelowanie liczby szkód
#   Cel spotkania:
#     1) Zrozumieć, kiedy Poisson jest za mało elastyczny (nadrozproszenie, nadmiar zer)
#     2) Dopasować i porównać modele jednowymiarowe: Poisson, NegBin, ZIP/ZAP, ZANB
#     3) Czytać diagnostykę: AIC/BIC, gofstat, overlay, rootogram, p(0)
#   Uwaga: brak Exposure → traktujemy ekspozycję jako 1 (kontrakt-rocznie)
############################################################

## =======================
## 0) Pakiety i dane
## =======================
# Potrzebujemy: dopasowań (fitdistrplus, gamlss.dist), transformacji/wykresów (dplyr, ggplot2 itd.)
req <- c("dplyr","ggplot2","forcats","tibble","scales","patchwork","fitdistrplus","gamlss.dist", "vcd")
new <- setdiff(req, rownames(installed.packages()))
if(length(new)) install.packages(new, repos = "https://cloud.r-project.org")
invisible(lapply(req, library, character.only = TRUE))

# Dane lokalne (jak poprzednio)
dane <- read.table("dane_fr_liczba", header = TRUE, dec=".", stringsAsFactors = FALSE)

# Porządkowanie typów + sanity check (braki, wartości ujemne)
dane <- dane %>%
  mutate(
    ClaimNb  = as.integer(ClaimNb),
    VehClass = factor(VehClass)
  ) %>%
  filter(!is.na(ClaimNb) & ClaimNb >= 0)

l.szkod <- dane$ClaimNb
summary(l.szkod)

## =======================
## 1) Empiryka: ECDF + histogram dyskretny
## =======================
# Spojrzenie „na oko” — czy dużo zer? jak długi ogon?
par(mfrow = c(1,2))
plot(ecdf(l.szkod), main = "ECDF liczby szkód", xlab = "k", ylab = "F(k)")
br <- -0.5:(max(l.szkod)+0.5)
hist(l.szkod, breaks = br, main = "Histogram (dyskretna skala)", xlab = "k = ClaimNb")
par(mfrow = c(1,1))

## =======================
## 2) Szybki test nadrozproszenia (regresja y_k = a*k + b)
## =======================
# Idea: y_k = k * n_k / n_{k-1}; jeśli a>0 → nadrozproszenie → rozważ NB / mieszanki
h        <- hist(l.szkod, breaks = br, plot = FALSE)
n_k      <- h$counts[-1]                       # n_k
n_km1    <- h$counts[-length(h$counts)]        # n_{k-1}
k_mid    <- h$mids[-1]                         # k
y_k      <- k_mid * (n_k / pmax(n_km1, 1e-8))  # zabezpieczenie przed dzieleniem przez 0
reg_yk   <- lm(y_k ~ k_mid)
summary(reg_yk)                                 # a (wsp. przy k_mid): dodatni? → sygnał NB lub modeli zmodyfikowanych

## =======================
## 3a) Dopasowanie rozkładów klasy ab0
## =======================
# Poisson i NegBin (fitdistrplus), warianty zero-modified/inflated (gamlss.dist)
fit_Pois <- fitdist(l.szkod, "pois", method = "mle")
fit_NB   <- fitdist(l.szkod, "nbinom", method = "mle", discrete = TRUE)
summary(fit_Pois) # Poisson: λ = średnia szkód na polisę/rok.
summary(fit_NB) # NB: μ = średnia; size=r → im mniejsze r, tym większa Var (większe rozproszenie).

## =======================
## 3b) Dopasowanie rozkładów klasy ab1
## =======================
# Zero-Inflated Poisson (ZIP): mieszanka punktu 0 i Poissona
fit_ZIP  <- fitdist(l.szkod, "ZIP",
                    start = list(mu = mean(l.szkod), sigma = mean(l.szkod==0)),
                    method = "mle", discrete = TRUE, lower = c(0,0), upper = c(Inf,1))
summary(fit_ZIP) # „sigma” ≈ udział części nadwyżkowej (interpretuj jako p(0) „dodatkowe”).

# Zero-Adjusted Poisson (ZAP): zero ma własne p; dodatnie ~ Poisson przesunięty
fit_ZAP  <- fitdist(l.szkod, "ZAP",
                    start = list(mu = mean(l.szkod[l.szkod>0]), sigma = mean(l.szkod==0)),
                    method = "mle", discrete = TRUE, lower = c(0,0), upper = c(Inf,1))
summary(fit_ZAP) # „sigma” ≈ udział części zerowej (interpretuj jako p(0) „strukturalne”),  parametr μ dotyczy rozkładu dodatnich wartości (bez zera).

# Zero-Altered NegBin (ZANBI): zero ma własne p; dodatnie ~ NB (sigma=1/size)
fit_ZANB <- fitdist(l.szkod, "ZANBI",
                    start = list(mu = mean(l.szkod[l.szkod>0]), sigma = 0.05, nu = mean(l.szkod==0)),
                    method = "mle", discrete = TRUE, lower = c(0,0,0), upper = c(Inf,Inf,1))
summary(fit_ZANB) #  nu ≈ p(0)

## =======================
## 4) Diagnostyka graficzna (fitdistrplus)
## =======================
# Czy rozkład „łapie” zero i ogon? (QQ/PP/CDF/dens). Niższe odchylenia = lepiej.
plot(fit_Pois); title("Poisson")
plot(fit_NB);   title("NegBin")
plot(fit_ZIP);  title("ZIP")
plot(fit_ZAP);  title("ZAP")
plot(fit_ZANB); title("ZANB")

## =======================
## 5) Porównanie jakości dopasowania (AIC/BIC + gofstat)
## =======================
# Niższe AIC/BIC lepiej; gofstat: testy dla rozkładów dyskretnych (KS/CvM/χ²)
models <- list(Poisson=fit_Pois, NegBin=fit_NB, ZIP=fit_ZIP, ZAP=fit_ZAP, ZANB=fit_ZANB)

tab_ic <- purrr::imap_dfr(models, ~tibble::tibble(
  model = .y,
  AIC   = AIC(.x),
  BIC   = BIC(.x)
)) %>%
  dplyr::arrange(BIC)

tab_ic


## =======================
## 6) Overlay i rootogram: obs vs exp (intuicyjna ocena dopasowania)
## =======================
# Oczekiwane liczebności: exp_k = N * P(K=k | model)
expected_counts <- function(x, kmax, pmf_fun, ...) {
  k <- 0:kmax
  N <- length(x)
  tibble(
    k   = k,
    obs = as.numeric(table(factor(x, levels = k))),
    exp = N * pmf_fun(k, ...)
  )
}
kmax <- max(l.szkod)

df_Pois <- expected_counts(l.szkod, kmax, dpois,   lambda = fit_Pois$estimate["lambda"])
df_NB   <- expected_counts(l.szkod, kmax, dnbinom, size   = fit_NB$estimate["size"], mu = fit_NB$estimate["mu"])
df_ZIP  <- expected_counts(l.szkod, kmax, dZIP,    mu     = fit_ZIP$estimate["mu"],  sigma = fit_ZIP$estimate["sigma"])
df_ZAP  <- expected_counts(l.szkod, kmax, dZAP,    mu     = fit_ZAP$estimate["mu"],  sigma = fit_ZAP$estimate["sigma"])
df_ZANB <- expected_counts(l.szkod, kmax, dZANBI,  mu     = fit_ZANB$estimate["mu"], sigma = fit_ZANB$estimate["sigma"], nu = fit_ZANB$estimate["nu"])

plot_overlay <- function(df, ttl){
  ggplot(df, aes(k, obs)) +
    geom_col(fill = "grey85", width = 0.9) +             # słupki = obserwacje
    geom_point(aes(y = exp), color = "red", size = 2) +  # czerwone = oczekiwane
    geom_line(aes(y = exp),  color = "red", linewidth = 0.6) +
    labs(title = ttl, x = "k (liczba szkód)", y = "Liczność") +
    theme_minimal()
}
(plot_overlay(df_Pois, "Poisson") |
    plot_overlay(df_NB,   "NegBin")  |
    plot_overlay(df_ZIP,  "ZIP")) /
  (plot_overlay(df_ZAP,  "ZAP")     |
     plot_overlay(df_ZANB, "ZANB")   )
SS
# Rootogram (sqrt-liczebności) — lepiej eksponuje ogon i zero-inflację
plot_root <- function(df, ttl){
  ggplot(df, aes(k, sqrt(obs))) +
    geom_col(fill = "grey85", width = 0.9) +
    geom_point(aes(y = sqrt(exp)), color = "blue", size = 2) +
    geom_line(aes(y = sqrt(exp)),  color = "blue", linewidth = 0.6) +
    labs(title = paste("Rootogram:", ttl), x = "k", y = "sqrt(Liczność)") +
    theme_minimal()
}
(plot_root(df_Pois, "Poisson") | plot_root(df_NB, "NegBin") | plot_root(df_ZIP, "ZIP")) /
  (plot_root(df_ZAP, "ZAP") | plot_root(df_ZANB, "ZANB"))

## =======================
## 7) „Nadmiar zer” — szybka diagnoza
## =======================
############################################################
# p(0) dla różnych modeli (zarówno „zwykłych”, jak i mieszanych)
# DATA  = udział zer w danych
# POIS  = dpois(0, λ̂)
# NB    = dnbinom(0, size=r̂, mu=μ̂)
# ZIP   = dZIP(0,  mu=μ̂, sigma=π̂)           # mieszanka: π + (1-π)*Pois(0; μ)
# ZAP   = dZAP(0,  mu=μ̂, sigma=ν̂)           # hurdle: p(0)=ν (zero ma własną masę)
# ZANB  = dZANBI(0, mu=μ̂, sigma=1/r̂, nu=ν̂) # hurdle NB: p(0)=ν
############################################################

p0_obs  <- mean(l.szkod == 0)
p0_pois <- dpois(0, lambda = fit_Pois$estimate["lambda"])
p0_nb   <- dnbinom(0, size   = fit_NB$estimate["size"], mu = fit_NB$estimate["mu"])

p0_zip  <- dZIP( 0, mu = fit_ZIP$estimate["mu"],  sigma = fit_ZIP$estimate["sigma"])
p0_zap  <- dZAP( 0, mu = fit_ZAP$estimate["mu"],  sigma = fit_ZAP$estimate["sigma"])
p0_zanb <- dZANBI(0, mu = fit_ZANB$estimate["mu"], sigma = fit_ZANB$estimate["sigma"],
                  nu = fit_ZANB$estimate["nu"])

tibble::tibble(
  Model = c("DATA","Poisson","NegBin","ZIP","ZAP","ZANB"),
  `p(0)` = c(p0_obs, p0_pois, p0_nb, p0_zip, p0_zap, p0_zanb)
)

## =======================
## 8) Mini-segmentacja: „Cheapest” vs „Most expensive”
## =======================
# Sprawdza, czy dopasowanie/parametry różnią się w grupach portfela
levels_order <- c("Cheapest","Cheaper","Cheap","Medium low","Medium","Medium high","Expensive","More expensive","Most expensive")
dane <- dane %>% mutate(VehClass = factor(VehClass, levels = intersect(levels_order, levels(VehClass))))

seg_summary <- dane %>%
  filter(VehClass %in% c("Cheapest","Most expensive")) %>%
  group_by(VehClass) %>%
  summarise(n = n(), mean_claims = mean(ClaimNb), p_zero = mean(ClaimNb==0), .groups="drop")
seg_summary

dane %>%
  filter(VehClass %in% c("Cheapest","Most expensive")) %>%
  ggplot(aes(x = ClaimNb, colour = VehClass)) +
  stat_ecdf(linewidth=1) +
  labs(title="Dystrybuanta empiryczna: tanie vs najdroższe auta",
       x="Liczba szkód", y="F(x)")

# (opcjonalnie) dopasuj Poissona/NB w obu klasach i porównaj AIC
fit_Cheap_P <- fitdist(dane$ClaimNb[dane$VehClass=="Cheapest"], "pois",   method="mle")
fit_Cheap_N <- fitdist(dane$ClaimNb[dane$VehClass=="Cheapest"], "nbinom", method="mle", discrete=TRUE)
fit_MExp_P  <- fitdist(dane$ClaimNb[dane$VehClass=="Most expensive"], "pois",   method="mle")
fit_MExp_N  <- fitdist(dane$ClaimNb[dane$VehClass=="Most expensive"], "nbinom", method="mle", discrete=TRUE)

# Rootogram Poisson – segment Cheapest
gf_pois_ch <- goodfit(dane$ClaimNb[dane$VehClass=="Cheapest"], type="poisson")

# Rootogram Poisson – segment Most expensive
gf_pois_me <- goodfit(dane$ClaimNb[dane$VehClass=="Most expensive"], type="poisson")

rootogram(gf_pois_ch, main="Rootogram – Cheapest (Poisson)")
rootogram(gf_pois_me, main="Rootogram – Most expensive (Poisson)")

## =======================
## 9) Co wybieramy i dlaczego? (mini-podsumowanie)
## =======================
# - Sprawdź jednocześnie: (1) AIC/BIC, (2) overlay/rootogram, (3) p(0) vs oczekiwane.
# - Jeśli Var >> Mean i/lub dużo zer: Poisson przegrywa → NB lub warianty zero-infl/adj.
# - To była analiza bez determinant. W praktyce kolejny krok to GLM (Poisson/NB/ZIP)
#   z predyktorami: DrivAge_band, VehClass, Area, VehUsage, itp. -> kolejne zajęcia

