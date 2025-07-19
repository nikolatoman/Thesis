# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# RSCRIPT PRO APLIKACNI CAST PRACE - HISTORICKA SIMULACE, VARIANCNI-KOVARIANCNI METODA, TABULKY S VYSLEDKY
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------------------------
# nacteni pckgs
# ------------------------------------------------------------------------------------------------------------------
library(xts)
library(extRemes)
library(xtable)

# ------------------------------------------------------------------------------------------------------------------
# METODA HISTORICKE SIMULACE - KONZERVATIVNI PORTFOLIO
# ------------------------------------------------------------------------------------------------------------------
alpha <- c(.05,.025,.01,.005)
VaR_hist_rpc <- -quantile(rpc$`Daily r_p cons.`, probs = 1-alpha)
ES_hist_rpc <- -sapply(VaR_hist_rpc, function(VaR) {  mean(rpc[rpc> -VaR])})
# intervalove odhady - bootstrap:
set.seed(123)
B <- 1000
VaR_boot_hist <- data.frame(numeric(B),numeric(B),numeric(B), numeric(B))
ES_boot_hist <- data.frame(numeric(B),numeric(B),numeric(B), numeric(B))
for (i in 1:B) {
  resample <- sample(rpc, replace = TRUE)
  for (j in 1:length(alpha)) {
    VaR_boot_hist[i,j] <- -quantile(resample, probs = 1-alpha[j])
    ES_boot_hist[i,j] <- -mean(resample[resample > -VaR_boot_hist[i,j]])
  }
}
# 95% interval spolehlivosti (bootstrap):
ci_var_hist_c <- data.frame(numeric(2), numeric(2), numeric(2))
ci_es_hist_c <- data.frame(numeric(2), numeric(2), numeric(2))
for (j in 1:length(alpha)) {
  ci_var_hist_c[j,] <- quantile(VaR_boot_hist[,j], probs = c(0.025,.5, 0.975))
  ci_es_hist_c[j,] <- quantile(ES_boot_hist[,j], probs = c(0.025,.5, 0.975))
}
ci_var_hist_c
ci_es_hist_c

# ------------------------------------------------------------------------------------------------------------------
# METODA HISTORICKE SIMULACE - DYNAMICKE PORTFOLIO
# ------------------------------------------------------------------------------------------------------------------
alpha <- c(.05,.025,.01,.005)
VaR_hist_rpd <- -quantile(rpd$`Daily r_p dyn.`, probs = 1-alpha)
ES_hist_rpd <- -sapply(VaR_hist_rpd, function(VaR) {  mean(rpd[rpd> -VaR])})
# intervalove odhady - bootstrap:
set.seed(123)
B <- 1000
VaR_boot_hist <- data.frame(numeric(B),numeric(B),numeric(B), numeric(B))
ES_boot_hist <- data.frame(numeric(B),numeric(B),numeric(B), numeric(B))
for (i in 1:B) {
  resample <- sample(rpd, replace = TRUE)
  for (j in 1:length(alpha)) {
    VaR_boot_hist[i,j] <- -quantile(resample, probs = 1-alpha[j])
    ES_boot_hist[i,j] <- -mean(resample[resample > -VaR_boot_hist[i,j]])
  }
}
# 95% interval spolehlivosti (bootstrap):
ci_var_hist_d <- data.frame(numeric(2), numeric(2), numeric(2))
ci_es_hist_d <- data.frame(numeric(2), numeric(2), numeric(2))
for (j in 1:length(alpha)) {
  ci_var_hist_d[j,] <- quantile(VaR_boot_hist[,j], probs = c(0.025,.5, 0.975))
  ci_es_hist_d[j,] <- quantile(ES_boot_hist[,j], probs = c(0.025,.5, 0.975))
}
ci_var_hist_d
ci_es_hist_d

# ------------------------------------------------------------------------------------------------------------------
# VARIANCNI KOVARIANCNI METODA - KONZERVATIVNI PORTFOLIO
# ------------------------------------------------------------------------------------------------------------------
# vypocet je proveden na rozdeleni ztrat (ne vynosu!), ale vysledek uz je uveden zpet jako zaporne cislo (tzn. max. mozna ztrata z rozdeleni vynosu)
alpha <- c(.05,.025, .01,.005)
dfc_train = - dfc[1:which(rownames(dfc) =="2021-12-31"), ]
mu_loss <- t(vahyc)%*%colMeans(-dfc_train)
sigma_loss <- sqrt(t(vahyc) %*% cov(-dfc_train) %*% vahyc)
vahyc

VaR_param_c <- sapply(1:length(alpha), function(i) -(mu_loss + sigma_loss*qnorm(1-alpha[i])))
ES_param_c <- sapply(1:length(alpha), function(i) -(mu_loss + (sigma_loss/alpha[i]) * dnorm(qnorm(1-alpha[i]))))

# intervalove odhady - bootstrap:
set.seed(123)
B <- 1000
VaR_boot_par <- data.frame(numeric(B),numeric(B),numeric(B), numeric(B))
ES_boot_par <- data.frame(numeric(B),numeric(B),numeric(B), numeric(B))
for (i in 1:B) {
  resample <- sample(dfc_train, replace = TRUE)
  mu_p <- t(vahyc)%*%colMeans(-resample)
  sigma_p <- sqrt(t(vahyc) %*% cov(-resample) %*% vahyc)
  for (j in 1:length(alpha)) {
    VaR_boot_par[i,j] <- -(mu_p + sigma_p*qnorm(1-alpha[j]))
    ES_boot_par[i,j] <- -(mu_p + (sigma_p/alpha[j]) * dnorm(qnorm(1-alpha[j])))
  }
}
# 95% (bootstrap) interval spolehlivosti:
ci_var_param_c <- data.frame(numeric(2), numeric(2), numeric(2))
ci_es_param_c <- data.frame(numeric(2), numeric(2), numeric(2))
for (j in 1:length(alpha)) {
  ci_var_param_c[j,] <- quantile(VaR_boot_par[,j], probs = c(0.025,.5, 0.975))
  ci_es_param_c[j,] <- quantile(ES_boot_par[,j], probs = c(0.025,.5, 0.975))
}
ci_var_param_c
ci_es_param_c

# ------------------------------------------------------------------------------------------------------------------
# VARIANCNI KOVARIANCNI METODA - DYNAMICKE PORTFOLIO
# ------------------------------------------------------------------------------------------------------------------
# vypocet je proveden na rozdeleni ztrat (ne vynosu!), ale vysledek uz je uveden zpet jako zaporne cislo (tzn. max. mozna ztrata z rozdeleni vynosu)
alpha <- c(.05,.025, .01,.005)
dfd_train <- -dfd[1:which(rownames(dfd) =="2021-12-31"), ]
mu_lossD <- t(vahyd)%*%colMeans(-dfd_train) # prumerny vynos v dynamickem portfoliu je vice nez 3x vetsi nez u konzervativniho
sigma_lossD <- sqrt(t(vahyd) %*% cov(-dfd_train) %*% vahyd)
vahyd

VaR_param_d <- sapply(1:length(alpha), function(i) -(mu_lossD + sigma_lossD*qnorm(1-alpha[i]))) #i hodnoty v riziku je ale vyssi nez u konzervativniho (vic nez 2x)
ES_param_d <- sapply(1:length(alpha), function(i) -(mu_lossD + (sigma_lossD/alpha[i]) * dnorm(qnorm(1-alpha[i]))))

# intervalove odhady - bootstrap:
set.seed(123)
B <- 1000
VaR_boot_par <- data.frame(numeric(B),numeric(B),numeric(B), numeric(B))
ES_boot_par <- data.frame(numeric(B),numeric(B),numeric(B), numeric(B))
for (i in 1:B) {
  resample <- sample(dfd_train, replace = TRUE)
  mu_p <- t(vahyd)%*%colMeans(-resample)
  sigma_p <- sqrt(t(vahyd) %*% cov(-resample) %*% vahyd)
  for (j in 1:length(alpha)) {
    VaR_boot_par[i,j] <- -(mu_p + sigma_p*qnorm(1-alpha[j]))
    ES_boot_par[i,j] <- -(mu_p + (sigma_p/alpha[j]) * dnorm(qnorm(1-alpha[j])))
  }
}
# 95% (bootstrap) interval spolehlivosti:
ci_var_param_d <- data.frame(numeric(2), numeric(2), numeric(2))
ci_es_param_d <- data.frame(numeric(2), numeric(2), numeric(2))
for (j in 1:length(alpha)) {
  ci_var_param_d[j,] <- quantile(VaR_boot_par[,j], probs = c(0.025,.5, 0.975))
  ci_es_param_d[j,] <- quantile(ES_boot_par[,j], probs = c(0.025,.5, 0.975))
}
ci_var_param_d
ci_es_param_d


# ------------------------------------------------------------------------------------------------------------------
# SROVNANI VYSLEDNYCH CI PRO VaR A ES - KONZERVATIVNI PORTFOLIO
# ------------------------------------------------------------------------------------------------------------------
df_var_c <- data.frame(rbind(
ci_var_hist_c,
ci_var_param_c,
ci_var_pot_c,
ci_var_potD_c,
ci_var_pot_c2,
ci_var_potD_c2,
ci_var_bmmQ,
ci_var_bmmM,
ci_var_bmmW_c))
colnames(df_var_c) <- c("ci_lower", "mean", "ci_upper")
row_names <- c(
  "var_hist_95", "var_hist_975", "var_hist_99", "var_hist_995",
  "var_param_95", "var_param_975", "var_param_99", "var_param_995",
  "var_pot_95", "var_pot_975", "var_pot_99", "var_pot_995",
  "var_potD_95", "var_potD_975", "var_potD_99", "var_potD_995",
  "var_pot2_95", "var_pot2_975", "var_pot2_99", "var_pot2_995",
  "var_potD2_95", "var_potD2_975", "var_potD2_99", "var_potD2_995",
  "var_bmmQ_95", "var_bmmQ_975", "var_bmmQ_99", "var_bmmQ_995",
  "var_bmmM_95", "var_bmmM_975", "var_bmmM_99", "var_bmmM_995",
  "var_bmmW_95", "var_bmmW_975", "var_bmmW_99", "var_bmmW_995"
)
rownames(df_var_c) <- row_names

df_es_c <- data.frame(rbind(
  ci_es_hist_c,
  ci_es_param_c,
  ci_es_pot_c,
  ci_es_potD_c,
  ci_es_pot_c2,
  ci_es_potD_c2,
  ci_es_bmmQ,
  ci_es_bmmM,
  ci_es_bmmW_c))
colnames(df_es_c) <- c("ci_lower", "mean", "ci_upper")
row_names <- c(
  "es_hist_95", "es_hist_975", "es_hist_99", "es_hist_995",
  "es_param_95", "es_param_975", "es_param_99", "es_param_995",
  "es_pot_95", "es_pot_975", "es_pot_99", "es_pot_995",
  "es_potD_95", "es_potD_975", "es_potD_99", "es_potD_995",
  "es_pot2_95", "es_pot2_975", "es_pot2_99", "es_pot2_995",
  "es_potD2_95", "es_potD2_975", "es_potD2_99", "es_potD2_995",
  "es_bmmQ_95", "es_bmmQ_975", "es_bmmQ_99", "es_bmmQ_995",
  "es_bmmM_95", "es_bmmM_975", "es_bmmM_99", "es_bmmM_995",
  "es_bmmW_95", "es_bmmW_975", "es_bmmW_99", "es_bmmW_995"
)
rownames(df_es_c) <- row_names

df_c <- rbind(df_var_c, df_es_c)
df_c
write.csv(df_c, file = paste0(save_path, "/var_es_cons_final.csv"), row.names = TRUE)

# ------------------------------------------------------------------------------------------------------------------
# SROVNANI VYSLEDNYCH CI PRO VaR A ES - DYNAMICKE PORTFOLIO
# ------------------------------------------------------------------------------------------------------------------
df_var_d <- data.frame(rbind(
  ci_var_hist_d,
  ci_var_param_d,
  ci_var_pot_d,
  ci_var_potD_d,
  ci_var_pot_d2,
  ci_var_potD_d2,
  ci_var_bmmQ_d,
  ci_var_bmmM_d,
  ci_var_bmmW_d))
colnames(df_var_d) <- c("ci_lower", "mean", "ci_upper")
row_names <- c(
  "var_hist_95", "var_hist_975", "var_hist_99", "var_hist_995",
  "var_param_95", "var_param_975", "var_param_99", "var_param_995",
  "var_pot_95", "var_pot_975", "var_pot_99", "var_pot_995",
  "var_potD_95", "var_potD_975", "var_potD_99", "var_potD_995",
  "var_pot2_95", "var_pot2_975", "var_pot2_99", "var_pot2_995",
  "var_potD2_95", "var_potD2_975", "var_potD2_99", "var_potD2_995",
  "var_bmmQ_95", "var_bmmQ_975", "var_bmmQ_99", "var_bmmQ_995",
  "var_bmmM_95", "var_bmmM_975", "var_bmmM_99", "var_bmmM_995",
  "var_bmmW_95", "var_bmmW_975", "var_bmmW_99", "var_bmmW_995"
)
rownames(df_var_d) <- row_names

df_es_d <- data.frame(rbind(
  ci_es_hist_d,
  ci_es_param_d,
  ci_es_pot_d,
  ci_es_potD_d,
  ci_es_pot_d2,
  ci_es_potD_d2,
  ci_es_bmmQ_d,
  ci_es_bmmM_d,
  ci_es_bmmW_d))
colnames(df_es_d) <- c("ci_lower", "mean", "ci_upper")
row_names <- c(
  "es_hist_95", "es_hist_975", "es_hist_99", "es_hist_995",
  "es_param_95", "es_param_975", "es_param_99", "es_param_995",
  "es_pot_95", "es_pot_975", "es_pot_99", "es_pot_995",
  "es_potD_95", "es_potD_975", "es_potD_99", "es_potD_995",
  "es_pot2_95", "es_pot2_975", "es_pot2_99", "es_pot2_995",
  "es_potD2_95", "es_potD2_975", "es_potD2_99", "es_potD2_995",
  "es_bmmQ_95", "es_bmmQ_975", "es_bmmQ_99", "es_bmmQ_995",
  "es_bmmM_95", "es_bmmM_975", "es_bmmM_99", "es_bmmM_995",
  "es_bmmW_95", "es_bmmW_975", "es_bmmW_99", "es_bmmW_995"
)
rownames(df_es_d) <- row_names

df_d <- rbind(df_var_d, df_es_d)
df_d
write.csv(df_d, file = paste0(save_path, "/var_es_dyn_final.csv"), row.names = TRUE)




xtable(df_c[1:36,], caption = "Odhady VaR konzervativního portfolia", label = "var_c_results",digits=c(0,5,5,5))
xtable(df_c[37:72,], caption = "Odhady ES konzervativního portfolia", label = "es_c_results",digits=c(0,5,5,5))
xtable(df_d[1:36,], caption = "Odhady VaR dynamického portfolia", label = "var_d_results",digits=c(0,5,5,5))
xtable(df_d[37:72,], caption = "Odhady ES dynamického portfolia", label = "es_d_results",digits=c(0,5,5,5))
#BIS pozaduje ES na hladine prsti 97,5%, viz basel MAR33


df_c <- read.csv(paste0(save_path, "/var_es_cons_final.csv"))
df_c_var <- df_c[1:36,]
df_c_es <- df_c[37:72,]

df_d <- read.csv(paste0(save_path, "/var_es_dyn_final.csv"))
df_d_var <- df_d[1:36,]
df_d_es <- df_d[37:72,]


# ------------------------------------------------------------------------------------------------------------------
# KONEC SCRIPTU PRO HS, VAR-KOVAR. A SROVNAVACI TABULKY
# ------------------------------------------------------------------------------------------------------------------
