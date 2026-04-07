library(quantmod)
library(ggplot2)
library(gridExtra)
library(moments)

getSymbols("EEM", src = "yahoo", from = "2021-01-01", auto.assign = TRUE) # pobiera dane z yahoo o cenach EEM od 01/01/2021
getSymbols("QQQ", src = "yahoo", from = "2021-01-01", auto.assign = TRUE)

eem_price <- Ad(EEM) # pobiera adjusted price z EEM
qqq_price <- Ad(QQQ)

common_dates <- merge(eem_price, qqq_price, join = "inner")
colnames(common_dates) <- c("EEM", "QQQ")

# Logarytmiczne stopy zwrotu (dzienne)
return <- na.omit(diff(log(common_dates)))
colnames(return) <- c("EEM", "QQQ")

return_eem <- as.numeric(return[, "EEM"])
return_qqq <- as.numeric(return[, "QQQ"])

cat("\n--- Podstawowe statystyki stop zwrotu ---\n")
cat(sprintf("EEM: srednia=%.5f, odch.std=%.5f, skosnosc=%.3f, kurtoza=%.3f\n",
            mean(return_eem), sd(return_eem), skewness(return_eem), kurtosis(return_eem)))
cat(sprintf("QQQ: srednia=%.5f, odch.std=%.5f, skosnosc=%.3f, kurtoza=%.3f\n",
            mean(return_qqq), sd(return_qqq), skewness(return_qqq), kurtosis(return_qqq)))

# testy permutacyjne 

B <- 10000
set.seed(123)

# ---- 4a. Test istotnosci KOWARIANCJI ----
# H0: cov(r_eem, r_qqq) = 0

obs_cov <- cov(return_eem, return_qqq) #kowariancja obserwowana

perm_cov <- replicate(B, {
  cov(sample(return_eem), return_qqq)   # permutujemy jedna serie
})

p_cov <- mean(abs(perm_cov) >= abs(obs_cov)) # liczymy jaki procent permutacji dał kowariancję co najmniej tak ekstremalną jak obserwowana 
if (p_cov < 0.05) {
  cat("=> Kowariancja istotnie rozna od 0 (poziom 5%)\n")
} else {
  cat("=> Brak podstaw do odrzucenia H0 (kowariancja = 0)\n")
}

# ---- 4b. Test rownosci WARIANCJI ----
# H0: var(r_eem) = var(r_qqq)

obs_var_diff <- abs(var(return_eem) - var(return_qqq)) # obserwowana różnica wariancji 

combined_return <- c(return_eem, return_qqq) # łączymy wszystkie dane
n1 <- length(return_eem)
n2 <- length(return_qqq)

perm_var <- replicate(B, {
  perm <- sample(combined_return) #  dzielimy na dwie losowe grupy
  abs(var(perm[1:n1]) - var(perm[(n1+1):(n1+n2)])) # liczymy różnicę warancji dla tych dwóch grup
})

p_var <- mean(perm_var >= obs_var_diff) # srawdzamy jaki procent permutacji dał różnicę wariancji co najmniej tak dużą jak obserwowana
cat(sprintf("Test permutacyjny wariancji: p-wartosc = %.4f\n", p_var))
if (p_var < 0.05) {
  cat("=> Wariancje istotnie rozne (poziom 5%)\n")
} else {
  cat("=> Brak podstaw do odrzucenia H0 (wariancje rowne)\n")
}

# ---- 4c. Test rownosci WARTOSCI OCZEKIWANYCH ----
# H0: E[r_eem] = E[r_qqq]

obs_mean_diff <- abs(mean(return_eem) - mean(return_qqq)) # obserwowana różnica średnich 

perm_mean <- replicate(B, {
  perm <- sample(combined_return)     # bardzo podobnie jak w wariancji dzielimy na dwie losowe grupy
  abs(mean(perm[1:n1]) - mean(perm[(n1+1):(n1+n2)])) # liczymy różnicę średnich tych wylosowanych grup
})

p_mean <- mean(perm_mean >= obs_mean_diff)# i znowu p-value
cat(sprintf("Test permutacyjny wartosci oczekiwanych: p-wartosc = %.4f\n", p_mean))
if (p_mean < 0.05) {
  cat("=> Wartosci oczekiwane istotnie rozne (poziom 5%)\n")
} else {
  cat("=> Brak podstaw do odrzucenia H0 (wartosci oczekiwane rowne)\n")
}


# --- 5. PORTFEL: KUPNO 100 AKCJI, SPRZEDAZ PO 100 DNIACH -----------------

prices_eem <- as.numeric(common_dates[, "EEM"]) # pobieramy ceny dla EEM i QQQ
prices_qqq <- as.numeric(common_dates[, "QQQ"]) 
N <- length(prices_eem)
horizon <- 100  # dni gieldowych
quantity <- 100      # liczba akcji kazdego ETF

n_trades <- N - horizon # liczba możliwych dni zakupu (najpóźniej 100 dni przed końcem danych)

profit <- numeric(n_trades)
for (i in 1:n_trades) {
  buy_eem  <- prices_eem[i]
  sell_eem <- prices_eem[i + horizon] # zapisujemy ceny kupna i sprzedaży po 100 dniach dla oby ETF
  buy_qqq  <- prices_qqq[i]
  sell_qqq <- prices_qqq[i + horizon]
  profit[i] <- quantity * (sell_eem - buy_eem) + quantity * (sell_qqq - buy_qqq) # obliczamy zysk jako iloczyn różnicy cen i ilości zakupionych aktywów
}

cat(sprintf("\n--- Statystyki zysku portfela (100 akcji, horyzont %d dni) ---\n", horizon))
cat(sprintf("Liczba transakcji: %d\n", n_trades))
cat(sprintf("Sredni zysk:     %10.2f USD\n", mean(profit)))
cat(sprintf("Mediana zysku:   %10.2f USD\n", median(profit)))
cat(sprintf("Odch. std:       %10.2f USD\n", sd(profit)))
cat(sprintf("Min zysk:        %10.2f USD\n", min(profit)))
cat(sprintf("Max zysk:        %10.2f USD\n", max(profit)))
cat(sprintf("Skosnosc:        %10.4f\n", skewness(profit)))
cat(sprintf("Kurtoza:         %10.4f\n", kurtosis(profit)))

# --- 6. VALUE AT RISK (VaR) ------------------------------------------------
# VaR na poziomie 5% to taka wartość, że tylko w 5% przypadków strata będzie większa (czyli liczenie kwantyli dolnych)

alpha <- c(0.05, 0.01, 0.001) 
var_empirical <- quantile(profit, probs = alpha) # liczymy kwantyle z naszego wektora profit
                                                 # sortujemy wszystkie zyski od najmniejszego do największego
                                                 # i bierzemy wartość na pozycji 5%, 1% i 0.1%

# VaR przy zalozeniu normalnosci
mu_profit  <- mean(profit) # Zakładamy że zyski mają rozkład normalny z tą samą średnią i wariancją co nasze dane
sig_profit <- sd(profit)
var_normal <- qnorm(alpha, mean = mu_profit, sd = sig_profit)

cat("\n--- Value at Risk (strata = ujemny zysk) ---\n")
var_table <- data.frame(
  Poziom         = paste0(alpha * 100, "%"),
  VaR_empiryczny = round(var_empirical, 2),
  VaR_normalny   = round(var_normal, 2)
)

print(var_table, row.names = FALSE)

cat("\nInterpretacja VaR:\n")
cat(sprintf("  VaR 5%%  empiryczny: z prawdop. 5%% stracimy wiecej niz %.2f USD\n",
            abs(var_empirical[1])))
cat(sprintf("  VaR 1%%  empiryczny: z prawdop. 1%% stracimy wiecej niz %.2f USD\n",
            abs(var_empirical[2])))
cat(sprintf("  VaR 0.1%% empiryczny: z prawdop. 0.1%% stracimy wiecej niz %.2f USD\n",
            abs(var_empirical[3])))


# --- 7. WYKRESY ------------------------------------------------------------

dates_common <- index(common_dates)

# ---- 7a. Ceny ETF (znormalizowane do 100) ----

# Dzielimy każdą cenę przez cenę z pierwszego dnia i mnożymy przez 100, żeby obie serie zaczynały od wartości 100

df_price <- data.frame(
  Date  = dates_common,
  EEM   = 100 * prices_eem / prices_eem[1],
  QQQ   = 100 * prices_qqq / prices_qqq[1]
)

p1 <- ggplot(df_price, aes(x = Date)) +
  geom_line(aes(y = EEM, color = "EEM"), linewidth = 0.6) +
  geom_line(aes(y = QQQ, color = "QQQ"), linewidth = 0.6) +
  scale_color_manual(values = c("EEM" = "red", "QQQ" = "blue")) + 
  labs(title = "Znormalizowane ceny ETF",
       x = "Data", y = "Wartosc (baza=100)", color ="ETF") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

# ---- 7b. Histogramy stop zwrotu + krzywa normalna ----
make_return_hist <- function(r, name, color) {
  df <- data.frame(r = r)
  ggplot(df, aes(x = r)) +
    geom_histogram(aes(y = after_stat(density)),
                   fill = color,color = "white") +
    stat_function(fun = dnorm,
                  args = list(mean = mean(r), sd = sd(r)),
                  color = "black", linewidth = 1.3) +
    labs(title = paste("Rozklad stop zwrotu:", name),
         x = "Stopa zwrotu", y = "Gestosc") +
    theme_minimal(base_size = 11)
}

p2 <- make_return_hist(return_eem, "EEM", "red")
p3 <- make_return_hist(return_qqq, "QQQ", "blue")
p2
p3

# ---- 7c. Histogram zysku z portfela + porownanie z normalnym ----
df_profit <- data.frame(profit = profit)

p4 <- ggplot(df_profit, aes(x = profit)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill = "lightgreen", color = "white") +
  stat_function(fun = dnorm,
                args = list(mean = mu_profit, sd = sig_profit),
                color = "black", linewidth = 1.2, linetype = "dashed") +
  geom_vline(xintercept = var_empirical,
             color = c("orange", "red", "purple"), linewidth = 0.9) +
  geom_vline(xintercept = var_normal,
             color = c("orange", "red", "purple"), linetype = "dotted", linewidth = 0.9) +
  annotate("text", x = var_empirical[1], y = Inf,
           label = "VaR 5%", angle = 90, vjust = -0.3, hjust = 1, size = 3) +
  annotate("text", x = var_empirical[2], y = Inf,
           label = "VaR 1%", angle = 90, vjust = -0.3, hjust = 1.1, size = 3) +
  annotate("text", x = var_empirical[3], y = Inf,
           label = "VaR 0.1%", angle = 90, vjust = -0.3, hjust = 1.1, size = 3) +
  labs(title = "Zysk portfela: empiryczny vs rozklad normalny",
       subtitle = "Linie ciagle = VaR empiryczny | Linie kropkowane = VaR normalny",
       x = "Zysk (USD)", y = "Gestosc") +
  theme_minimal(base_size = 11)
p4


# ---- 7d. Zysk portfela w czasie ----
df_profit_ts <- data.frame(
  Date   = dates_common[1:n_trades],
  Profit = profit
)

p5 <- ggplot(df_profit_ts, aes(x = Date, y = Profit)) +
  geom_line(color = "lightgreen", linewidth = 0.6, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_hline(yintercept = var_empirical[1], color = "orange",
             linetype = "dashed", linewidth = 0.7) +
  geom_hline(yintercept = var_empirical[2], color = "red",
             linetype = "dashed", linewidth = 0.7) +
  geom_hline(yintercept = var_empirical[3], color = "purple",
             linetype = "dashed", linewidth = 0.7) +
  labs(title = "Zysk portfela w czasie (horyzont 100 dni)",
       x = "Data zakupu", y = "Zysk (USD)") +
  theme_minimal(base_size = 11)
p5