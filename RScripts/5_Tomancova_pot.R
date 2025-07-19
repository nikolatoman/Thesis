# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# RSCRIPT PRO APLIKACNI CAST PRACE - METODA SPICEK NAD PRAHEM
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------------------------
# nacteni pckgs a dat
# ------------------------------------------------------------------------------------------------------------------
library(xts)
library(extRemes)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(xtable)

rpc <- - rpcon_train
rpc_test <- - rpcon_test
rpd <- - rpdyn_train
rpd_test <- - rpdyn_test

# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# POT PRO KONZERVATIVNI PORTFOLIO
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# volba prahu u - viz samostatny RScript Tomancova_thresholds
# ------------------------------------------------------------------------------------------------------------------
#vysledne volby pro konzervativni portflio:
u1c <- .65
u2c <- .85

# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# MODELY PRO u1c
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
threshold_rpc <- u1c
pot_rpc <- rpc[rpc > threshold_rpc]

# ------------------------------------------------------------------------------------------------------------------
# u1c DECLUSTERING - volba vhodného r
# ------------------------------------------------------------------------------------------------------------------
rpc_zoo <- zoo(rpc$`Daily r_p cons.`, order.by = index(rpc))

mingap_rpc <- 7 # minimální odstup přesahů přes práh je 1 týden -> zbyde 59 ze 107
# Funkce na identifikaci clusterů
clust_rpc <- cumsum(c(1, diff(index(pot_rpc))) > mingap_rpc)
clust_rpc

# Výběr maxima z každého clusteru v datech
pot_rpc_declust <- cbind(index(pot_rpc), data.frame(pot_rpc) , clust_rpc)
pot_rpc_declust <- pot_rpc_declust %>%
  group_by(clust_rpc) %>%                     # Skupinování podle clust_rpc
  mutate(is_group_max = if_else(Daily.r_p.cons. == max(Daily.r_p.cons.), 1, 0)) %>%  # Výběr maxima z každé skupiny
  ungroup() 
pot_rpc_declust <- xts(pot_rpc_declust, order.by = pot_rpc_declust$`index(pot_rpc)`)
pot_rpc_data <- pot_rpc_declust$`Daily.r_p.cons.`[pot_rpc_declust$is_group_max==1]
length(pot_rpc_data)
zavisla_ind <- index(pot_rpc_declust$Daily.r_p.cons.[pot_rpc_declust$is_group_max==0])
rpc_decl <- rpc[!(index(rpc)%in% zavisla_ind)]
length(rpc_decl)

# ------------------------------------------------------------------------------------------------------------------
# u1c MODELOVÁNÍ GPD
# ------------------------------------------------------------------------------------------------------------------
fit_rpc_pot <- fevd(rpc, threshold = threshold_rpc, type = "GP")
summary(fit_rpc_pot)
ci(fit_rpc_pot, type = "parameter")
exc_rpc <- pot_rpc[pot_rpc > threshold_rpc] - threshold_rpc
scale <- fit_rpc_pot$results$par[1]
shape <- fit_rpc_pot$results$par[2]
#ulozeni parametru pro ucely backtestingu
scale_pot1_c <- fit_rpc_pot$results$par[1]
shape_pot1_c <- fit_rpc_pot$results$par[2]
rate_pot1_c <- length(pot_rpc)/length(rpc)

fit_rpc_pot_declust <- fevd(as.numeric(pot_rpc_data), threshold = threshold_rpc, type = "GP")
summary(fit_rpc_pot_declust)
ci(fit_rpc_pot_declust, type = "parameter")
summary(fevd(as.numeric(pot_rpc_data), type="GP", threshold = threshold_rpc, method = "Lmoments")) #kontrola robustnosti
exc_rpcD <- as.numeric(pot_rpc_data[pot_rpc_data > threshold_rpc]) - threshold_rpc
scaleD <- fit_rpc_pot_declust$results$par[1]
shapeD <- fit_rpc_pot_declust$results$par[2]
#ulozeni parametru pro ucely backtestingu
scale_potD1_c <- fit_rpc_pot_declust$results$par[1]
shape_potD1_c <- fit_rpc_pot_declust$results$par[2]
rate_potD1_c <- length(pot_rpc_data)/length(rpc_decl)

#udaje do tabulky pro u1c:
is <- round(ci(fit_rpc_pot, type = "parameter")[1:2,1:3],5) 
is_char <- c(paste0("(", is[,1], ";", is[,3], ")"))
df_par_est_c <- data.frame(matrix(nrow=2, ncol=4))
df_par_est_c <- cbind(is,is_char)
is <- round(ci(fit_rpc_pot_declust, type = "parameter")[1:2,1:3],5) 
is_char <- c(paste0("(", is[,1], ";", is[,3], ")"))
df_par_est_c <- rbind(df_par_est_c, cbind(is,is_char))
# ------------------------------------------------------------------------------------------------------------------
# u1c DIAGNOSTIKA MODELŮ
# ------------------------------------------------------------------------------------------------------------------
# A) PRO PUVODNI MODEL

#ECDF a nafitovana teoreticka CDF
empirical_cdf <- ecdf(as.vector(exc_rpc$`Daily r_p cons.`))
x_vals <- seq(-5, 10, length.out = 1000)  # Body na ose x pro vygenerovani teoreticke cdf
theoretical_cdf <- pevd(x_vals, scale = scale, shape = shape, type="GP")  # Teoretická CDF
plot(empirical_cdf, verticals = TRUE, do.points = FALSE, col=2, xlim=c(-5,10), xlab = "y", 
     ylab = expression(F[u](y)), main = "Konzervativní portfolio")
lines(x_vals, theoretical_cdf, type="l")

#PPPLOT:
empirical_cdf <- ecdf(as.vector(exc_rpc))
empirical_probs <- empirical_cdf(as.vector(exc_rpc))
theoretical_probs <- pevd(as.vector(exc_rpc), scale = scale, shape = shape, type="GP")  # Teoretická CDF
plot_data <- data.frame(empirical = empirical_probs,  theoretical = theoretical_probs)
ppplot <- ggplot(plot_data, aes(x = theoretical, y = empirical)) +
  geom_point(color = "black") +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "P-P Plot - Konzervativní p.", x = "Teoretické pravděpodobnosti",y = "Empirické pravděpodobnosti") +
  theme_bw()

#QQPLOT:
qqplot <- ggplot(exc_rpc, aes(sample = as.vector(exc_rpc$`Daily r_p cons.`))) +
  stat_qq(distribution = extRemes::qevd, dparams = list(scale = scale, shape = shape, loc = 0, type = "GP")) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Teoretické kvantily", y = "Empirické kvantily", title = "Q-Q Plot") +
  xlim(0,5.5) + ylim(0,5.5) +
  theme_bw()

# Histogram s hustotou odhadnuteho rozdeleni
hist <- ggplot(exc_rpc, aes(x = as.vector(exc_rpc$`Daily r_p cons.`))) +
  geom_histogram(mapping = aes(y = after_stat(density)), binwidth = .4, col = "gray30", fill = "dimgrey", alpha = .2,) +
  stat_function(fun = extRemes::devd, args = list(scale = scale, shape = shape, type = "GP"), col = "black", size = .7) +
  labs(x = "Exces", y = "Hustota", title = "Histogram") +
  theme_bw()

# Return Level Plot
return_periods <- c(1.1, 2, 5, 10, 20, 50, 100, 500)  # Doby návratu
return_levels <- return.level(fit_rpc_pot, return.period = return_periods)
n_y <- 365 # pocet dnu v roce v teto casove rade 
ci <- ci(fit_rpc_pot, type = "return.level", return.period = return_periods) # Intervaly spolehlivosti pro návratové hladiny
plot_data <- data.frame(return_period = return_periods, return_level = return_levels, ci_lower = ci[, 1], ci_upper = ci[, 3])
empirical_levels <- sort(as.vector(pot_rpc), decreasing = TRUE)
empirical_periods <- (((length(rpc)) + 1) / index(empirical_levels))/365
empirical_data <- data.frame(period = empirical_periods, level = empirical_levels)
rlplot <- ggplot() +
  geom_line(data = plot_data, aes(x = return_period, y = return_level), color = "black", size = 1) +  # Teoretická křivka
  geom_point(data = empirical_data, aes(x = period, y = level), color = "black", size = 2) +  # Empirické body
  geom_ribbon(data = plot_data, aes(x= return_period, ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "darkgray") +  # Intervaly spolehlivosti
  scale_x_log10() +  # Logaritmická osa x
  labs(title = "Return Level Plot", x = "Návratová perioda (log)",y = "Návratová úroveň") +
  theme_bw()

grid.arrange(
  ppplot, qqplot, rlplot, hist,
  nrow = 2, ncol = 2
)

# B) PRO DECLUST MODEL

#ECDF a nafitovana teoreticka CDF
empirical_cdf <- ecdf(as.vector(exc_rpcD))
x_vals <- seq(-5, 10, length.out = 1000)  # Body na ose x pro vygenerovani teoreticke cdf
theoretical_cdf <- pevd(x_vals, scale = scaleD, shape = shapeD, type="GP")  # Teoretická CDF
plot(empirical_cdf, verticals = TRUE, do.points = FALSE, col=2, xlim=c(-5,10), xlab = "y", 
     ylab = expression(F[u](y)), main = "Konzervativní portfolio")
lines(x_vals, theoretical_cdf, type="l")

#PPPLOT:
empirical_cdf <- ecdf(as.vector(exc_rpcD))
empirical_probs <- empirical_cdf(as.vector(exc_rpcD))
theoretical_probs <- pevd(as.vector(exc_rpcD), scale = scaleD, shape = shapeD, type="GP")  # Teoretická CDF
plot_data <- data.frame(empirical = empirical_probs,theoretical = theoretical_probs)
ppplot <- ggplot(plot_data, aes(x = theoretical, y = empirical)) +
  geom_point(color = "black") +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "P-P Plot - Konzervativní p.", x = "Teoretické pravděpodobnosti",y = "Empirické pravděpodobnosti") +
  theme_bw()

#QQPLOT:
qqplot <- ggplot(as.data.frame(exc_rpcD), aes(sample = as.numeric(exc_rpcD))) +
  stat_qq(distribution = extRemes::qevd, dparams = list(scale = scaleD, shape = shapeD, loc = 0, type = "GP")) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Teoretické kvantily", y = "Empirické kvantily", title = "Q-Q Plot") + xlim(0,5.5) + ylim(0,5.5) +
  theme_bw()

# Histogram s hustotou odhadnuteho rozdeleni
hist <- ggplot(as.data.frame(exc_rpcD), aes(x = as.vector(exc_rpcD))) +
  geom_histogram(mapping = aes(y = after_stat(density)), binwidth = .4, col = "gray30", fill = "dimgrey", alpha = .2,) +
  stat_function(fun = extRemes::devd, args = list(scale = scaleD, shape = shapeD, type = "GP"), col = "black", size = .7) +
  labs(x = "Exces", y = "Hustota", title = "Histogram") +
  theme_bw()

# Return Level Plot 
return_periods <- c(1.1, 2, 5, 10, 20, 50, 100, 500)  # Doby návratu
return_levels <- return.level(fit_rpc_pot_declust, return.period = return_periods)
n_y <- round(length(rpc_decl)/7) # pocet dnu v roce v teto casove rade 
ci <- ci(fit_rpc_pot_declust, type = "return.level", return.period = return_periods) # Intervaly spolehlivosti pro návratové hladiny
plot_data <- data.frame(return_period = return_periods, return_level = return_levels, ci_lower = ci[, 1], ci_upper = ci[, 3])
empirical_levels <- sort(as.numeric(pot_rpc_data), decreasing = TRUE)
empirical_periods <- (((length(rpc_decl)) + 1) / index(empirical_levels))/n_y
empirical_data <- data.frame(period = empirical_periods, level = empirical_levels)
rlplot <- ggplot() +
  geom_line(data = plot_data, aes(x = return_period, y = return_level), color = "black", size = 1) +  # Teoretická křivka
  geom_point(data = empirical_data, aes(x = period, y = level), color = "black", size = 2) +  # Empirické body
  geom_ribbon(data = plot_data, aes(x= return_period, ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "darkgray") +  # Intervaly spolehlivosti
  scale_x_log10() +  # Logaritmická osa x
  labs(title = "Return Level Plot", x = "Návratová perioda (log)",y = "Návratová úroveň") +
  theme_bw()

grid.arrange(
  ppplot, qqplot, rlplot, hist,
  nrow = 2, ncol = 2
)

# ------------------------------------------------------------------------------------------------------------------
# u1c ODHAD VaR a ES
# ------------------------------------------------------------------------------------------------------------------
# uvedeno jiz opet jako zaporna hodnota, tzn vynosy = kladne, ztraty = zaporne
alpha <- c(.05,.025,.01,.005)
n <- length(rpc)
n_u <- length(exc_rpc)
VaR_pot_rpc <- -(threshold_rpc + (scale / shape) * ((((n / n_u) * (alpha))^(-shape)) - 1))
ES_pot_rpc <- -sapply(VaR_pot_rpc, function(VaR) {(-VaR/(1-shape))+((scale-shape*threshold_rpc)/(1-shape))})

# intervalove odhady - bootstrap:
set.seed(123)
B <- 1000
VaR_boot_pot <- data.frame(numeric(B),numeric(B),numeric(B), numeric(B))
ES_boot_pot <- data.frame(numeric(B),numeric(B),numeric(B), numeric(B))
# POZOR, bootstrap je vypocetne narocny, trva to dele!
for (i in 1:B) {
  resample <- sample(exc_rpc, replace = TRUE)
  fit_boot <- fevd(resample, threshold = 0, type = "GP")
  params_boot <- fit_boot$results$par
  for (j in 1:length(alpha)) {
    VaR_boot_pot[i,j] <- -(threshold_rpc + (params_boot["scale"] / params_boot["shape"]) * 
                        ((((n / n_u) * (alpha[j]))^(-params_boot["shape"])) - 1))
    ES_boot_pot[i,j] <- -((-VaR_boot_pot[i,j]/(1- params_boot["shape"]))+((params_boot["scale"]-
                        params_boot["shape"]*threshold_rpc)/(1- params_boot["shape"])))
  }
}
# 95% interval spolehlivosti:
ci_var_pot_c <- data.frame(numeric(2), numeric(2), numeric(2))
ci_es_pot_c <- data.frame(numeric(2), numeric(2), numeric(2))
for (j in 1:length(alpha)) {
  ci_var_pot_c[j,] <- quantile(VaR_boot_pot[,j], probs = c(0.025,.5, 0.975))
  ci_es_pot_c[j,] <- quantile(ES_boot_pot[,j], probs = c(0.025,.5, 0.975))
}
ci_var_pot_c
ci_es_pot_c

#totez pro declusterizovany model:
nD <- length(rpc_decl)
n_uD <- length(exc_rpcD)
VaR_pot_rpc_D <- -(threshold_rpc + (scaleD / shapeD) * ((((nD / n_uD) * (alpha))^(-shapeD)) - 1))
ES_pot_rpc_D <- -sapply(VaR_pot_rpc_D, function(VaR) {(-VaR/(1-shapeD))+((scaleD-shapeD*threshold_rpc)/(1-shapeD))})
# intervalove odhady - bootstrap:
VaR_boot_potD <- data.frame(numeric(B),numeric(B),numeric(B), numeric(B))
ES_boot_potD <- data.frame(numeric(B),numeric(B),numeric(B), numeric(B))
# POZOR, bootstrap je vypocetne narocny, trva to dele!
for (i in 1:B) {
  resample <- sample(exc_rpcD, replace = TRUE)
  fit_boot <- fevd(resample, threshold = 0, type = "GP")
  params_boot <- fit_boot$results$par
  for (j in 1:length(alpha)) {
    VaR_boot_potD[i,j] <- -(threshold_rpc + (params_boot["scale"] / params_boot["shape"]) * 
                             ((((nD / n_uD) * (alpha[j]))^(-params_boot["shape"])) - 1))
    ES_boot_potD[i,j] <- -((-VaR_boot_potD[i,j]/(1- params_boot["shape"]))+((params_boot["scale"]-
                                                                             params_boot["shape"]*threshold_rpc)/(1- params_boot["shape"])))
  }
}
# 95% interval spolehlivosti:
ci_var_potD_c<- data.frame(numeric(2), numeric(2), numeric(2))
ci_es_potD_c <- data.frame(numeric(2), numeric(2), numeric(2))
for (j in 1:length(alpha)) {
  ci_var_potD_c[j,] <- quantile(VaR_boot_potD[,j], probs = c(0.025,.5, 0.975))
  ci_es_potD_c[j,] <- quantile(ES_boot_potD[,j], probs = c(0.025,.5, 0.975))
}
ci_var_potD_c
ci_es_potD_c

# ------------------------------------------------------------------------------------------------------------------
# u1c BACKTESTING - TESTOVACI DATA
# ------------------------------------------------------------------------------------------------------------------
# # viz samostatny soubor Tomancova_backtesting.R

# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# MODELY PRO u2c
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------

threshold_rpc <- u2c
pot_rpc <- rpc[rpc > threshold_rpc]

# ------------------------------------------------------------------------------------------------------------------
# u2c DECLUSTERING - volba vhodného r
# ------------------------------------------------------------------------------------------------------------------
rpc_zoo <- zoo(rpc$`Daily r_p cons.`, order.by = index(rpc))

mingap_rpc <- 7 # minimální odstup přesahů přes práh je 1 týden -> zbyde 35 z 56
# Funkce na identifikaci clusterů
clust_rpc <- cumsum(c(1, diff(index(pot_rpc))) > mingap_rpc)
clust_rpc

# Výběr maxima z každého clusteru v datech
pot_rpc_declust <- cbind(index(pot_rpc), data.frame(pot_rpc) , clust_rpc)
pot_rpc_declust <- pot_rpc_declust %>%
  group_by(clust_rpc) %>%                     # Skupinování podle clust_rpc
  mutate(is_group_max = if_else(Daily.r_p.cons. == max(Daily.r_p.cons.), 1, 0)) %>%  # Výběr maxima z každé skupiny
  ungroup() 
pot_rpc_declust <- xts(pot_rpc_declust, order.by = pot_rpc_declust$`index(pot_rpc)`)
pot_rpc_data <- pot_rpc_declust$`Daily.r_p.cons.`[pot_rpc_declust$is_group_max==1]
length(pot_rpc_data)
zavisla_ind <- index(pot_rpc_declust$Daily.r_p.cons.[pot_rpc_declust$is_group_max==0])
rpc_decl <- rpc[!(index(rpc)%in% zavisla_ind)]
length(rpc_decl)


# ------------------------------------------------------------------------------------------------------------------
# u2c MODELOVÁNÍ GPD
# ------------------------------------------------------------------------------------------------------------------
fit_rpc_pot <- fevd(rpc, threshold = threshold_rpc, type = "GP")
summary(fit_rpc_pot)
ci(fit_rpc_pot, type = "parameter")
summary(fevd(rpc, type="GP", threshold = threshold_rpc, method = "Lmoments")) #kontrola robustnosti
exc_rpc <- pot_rpc[pot_rpc > threshold_rpc] - threshold_rpc
scale <- fit_rpc_pot$results$par[1]
shape <- fit_rpc_pot$results$par[2]
#ulozeni parametru pro ucely backtestingu
scale_pot2_c <- fit_rpc_pot$results$par[1]
shape_pot2_c <- fit_rpc_pot$results$par[2]
rate_pot2_c <- length(pot_rpc)/length(rpc)

fit_rpc_pot_declust <- fevd(as.numeric(pot_rpc_data), threshold = threshold_rpc, type = "GP")
summary(fit_rpc_pot_declust)
ci(fit_rpc_pot_declust, type = "parameter")
summary(fevd(as.numeric(pot_rpc_data), type="GP", threshold = threshold_rpc, method = "Lmoments")) #kontrola robustnosti
exc_rpcD <- as.numeric(pot_rpc_data[pot_rpc_data > threshold_rpc]) - threshold_rpc
scaleD <- fit_rpc_pot_declust$results$par[1]
shapeD <- fit_rpc_pot_declust$results$par[2]
#ulozeni parametru pro ucely backtestingu
scale_potD2_c <- fit_rpc_pot_declust$results$par[1]
shape_potD2_c <- fit_rpc_pot_declust$results$par[2]
rate_potD2_c <- length(pot_rpc_data)/length(rpc_decl)


#udaje do tabulky:
is <- round(ci(fit_rpc_pot, type = "parameter")[1:2,1:3],5) 
is_char <- c(paste0("(", is[,1], ";", is[,3], ")"))
df_par_est_c <- rbind(df_par_est_c, cbind(is,is_char))
is <- round(ci(fit_rpc_pot_declust, type = "parameter")[1:2,1:3],5) 
is_char <- c(paste0("(", is[,1], ";", is[,3], ")"))
df_par_est_c <- rbind(df_par_est_c, cbind(is,is_char))
colnames(df_par_est_c) <- c( "lower", "Bodový odhad", "upper", "Intervalový odhad")
rownames(df_par_est_c) <- rep(c("sigma", "xi"), 4)
prah <- c("$u_1 = 0,65$", "$u_1 = 0,65$", "$u_1 = 0,65$", "$u_1 = 0,65$", "$u_2 = 0,85$", "$u_2 = 0,85$", "$u_2 = 0,85$", "$u_2 = 0,85$")
library(xtable)
xtable(data.frame('Práh' = prah[c(1,3,5,7,2,4,6,8)], 'Model' = cbind(rep(c("GP", "Declust. GP"),4)), df_par_est_c[c(1,3,5,7,2,4,6,8),c(2,4)]))
# ------------------------------------------------------------------------------------------------------------------
# u2c DIAGNOSTIKA MODELŮ
# ------------------------------------------------------------------------------------------------------------------
# A) PRO PUVODNI MODEL

#ECDF a nafitovana teoreticka CDF
empirical_cdf <- ecdf(as.vector(exc_rpc$`Daily r_p cons.`))
x_vals <- seq(-5, 10, length.out = 1000)  # Body na ose x pro vygenerovani teoreticke cdf
theoretical_cdf <- pevd(x_vals, scale = scale, shape = shape, type="GP")  # Teoretická CDF
plot(empirical_cdf, verticals = TRUE, do.points = FALSE, col=2, xlim=c(-5,10), xlab = "y", 
     ylab = expression(F[u](y)), main = "Konzervativní portfolio")
lines(x_vals, theoretical_cdf, type="l")

#PPPLOT:
empirical_cdf <- ecdf(as.vector(exc_rpc))
empirical_probs <- empirical_cdf(as.vector(exc_rpc))
theoretical_probs <- pevd(as.vector(exc_rpc), scale = scale, shape = shape, type="GP")  # Teoretická CDF
plot_data <- data.frame(empirical = empirical_probs,  theoretical = theoretical_probs)
ppplot <- ggplot(plot_data, aes(x = theoretical, y = empirical)) +
  geom_point(color = "black") +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "P-P Plot - Konzervativní p.", x = "Teoretické pravděpodobnosti",y = "Empirické pravděpodobnosti") +
  theme_bw()

#QQPLOT:
qqplot <- ggplot(exc_rpc, aes(sample = as.vector(exc_rpc$`Daily r_p cons.`))) +
  stat_qq(distribution = extRemes::qevd, dparams = list(scale = scale, shape = shape, loc = 0, type = "GP")) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Teoretické kvantily", y = "Empirické kvantily", title = "Q-Q Plot") +
  xlim(0,6) + ylim(0,6) +
  theme_bw()

# Histogram s hustotou odhadnuteho rozdeleni
hist <- ggplot(exc_rpc, aes(x = as.vector(exc_rpc$`Daily r_p cons.`))) +
  geom_histogram(mapping = aes(y = after_stat(density)), binwidth = .4, col = "gray30", fill = "dimgrey", alpha = .2,) +
  stat_function(fun = extRemes::devd, args = list(scale = scale, shape = shape, type = "GP"), col = "black", size = .7) +
  labs(x = "Exces", y = "Hustota", title = "Histogram") +
  theme_bw()

# Return Level Plot
return_periods <- c(1.1, 2, 5, 10, 20, 50, 100, 500)  # Doby návratu
return_levels <- return.level(fit_rpc_pot, return.period = return_periods)
n_y <- 365 # pocet dnu v roce v teto casove rade 
ci <- ci(fit_rpc_pot, type = "return.level", return.period = return_periods) # Intervaly spolehlivosti pro návratové hladiny
plot_data <- data.frame(return_period = return_periods, return_level = return_levels, ci_lower = ci[, 1], ci_upper = ci[, 3])
empirical_levels <- sort(as.vector(pot_rpc), decreasing = TRUE)
empirical_periods <- (((length(rpc)) + 1) / index(empirical_levels))/365
empirical_data <- data.frame(period = empirical_periods, level = empirical_levels)
rlplot <- ggplot() +
  geom_line(data = plot_data, aes(x = return_period, y = return_level), color = "black", size = 1) +  # Teoretická křivka
  geom_point(data = empirical_data, aes(x = period, y = level), color = "black", size = 2) +  # Empirické body
  geom_ribbon(data = plot_data, aes(x= return_period, ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "darkgray") +  # Intervaly spolehlivosti
  scale_x_log10() +  # Logaritmická osa x
  labs(title = "Return Level Plot", x = "Návratová perioda (log)",y = "Návratová úroveň") +
  theme_bw()

grid.arrange(
  ppplot, qqplot, rlplot, hist,
  nrow = 2, ncol = 2
)

# B) PRO DECLUST MODEL

#ECDF a nafitovana teoreticka CDF
empirical_cdf <- ecdf(as.vector(exc_rpcD))
x_vals <- seq(-5, 10, length.out = 1000)  # Body na ose x pro vygenerovani teoreticke cdf
theoretical_cdf <- pevd(x_vals, scale = scaleD, shape = shapeD, type="GP")  # Teoretická CDF
plot(empirical_cdf, verticals = TRUE, do.points = FALSE, col=2, xlim=c(-5,10), xlab = "y", 
     ylab = expression(F[u](y)), main = "Konzervativní portfolio")
lines(x_vals, theoretical_cdf, type="l")

#PPPLOT:
empirical_cdf <- ecdf(as.vector(exc_rpcD))
empirical_probs <- empirical_cdf(as.vector(exc_rpcD))
theoretical_probs <- pevd(as.vector(exc_rpcD), scale = scaleD, shape = shapeD, type="GP")  # Teoretická CDF
plot_data <- data.frame(empirical = empirical_probs,theoretical = theoretical_probs)
ppplot <- ggplot(plot_data, aes(x = theoretical, y = empirical)) +
  geom_point(color = "black") +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "P-P Plot - Konzervativní p.", x = "Teoretické pravděpodobnosti",y = "Empirické pravděpodobnosti") +
  theme_bw()

#QQPLOT:
qqplot <- ggplot(as.data.frame(exc_rpcD), aes(sample = as.numeric(exc_rpcD))) +
  stat_qq(distribution = extRemes::qevd, dparams = list(scale = scaleD, shape = shapeD, loc = 0, type = "GP")) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Teoretické kvantily", y = "Empirické kvantily", title = "Q-Q Plot") + xlim(0,5.5) + ylim(0,5.5) +
  theme_bw()

# Histogram s hustotou odhadnuteho rozdeleni
hist <- ggplot(as.data.frame(exc_rpcD), aes(x = as.vector(exc_rpcD))) +
  geom_histogram(mapping = aes(y = after_stat(density)), binwidth = .4, col = "gray30", fill = "dimgrey", alpha = .2,) +
  stat_function(fun = extRemes::devd, args = list(scale = scaleD, shape = shapeD, type = "GP"), col = "black", size = .7) +
  labs(x = "Exces", y = "Hustota", title = "Histogram") +
  theme_bw()

# Return Level Plot 
return_periods <- c(1.1, 2, 5, 10, 20, 50, 100, 500)  # Doby návratu
return_levels <- return.level(fit_rpc_pot_declust, return.period = return_periods)
n_y <- round(length(rpc_decl) / 7) # pocet dnu v roce v teto casove rade 
ci <- ci(fit_rpc_pot_declust, type = "return.level", return.period = return_periods) # Intervaly spolehlivosti pro návratové hladiny
plot_data <- data.frame(return_period = return_periods, return_level = return_levels, ci_lower = ci[, 1], ci_upper = ci[, 3])
empirical_levels <- sort(as.numeric(pot_rpc_data), decreasing = TRUE)
empirical_periods <- (((length(rpc_decl)) + 1) / index(empirical_levels))/n_y
empirical_data <- data.frame(period = empirical_periods, level = empirical_levels)
rlplot <- ggplot() +
  geom_line(data = plot_data, aes(x = return_period, y = return_level), color = "black", size = 1) +  # Teoretická křivka
  geom_point(data = empirical_data, aes(x = period, y = level), color = "black", size = 2) +  # Empirické body
  geom_ribbon(data = plot_data, aes(x= return_period, ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "darkgray") +  # Intervaly spolehlivosti
  scale_x_log10() +  # Logaritmická osa x
  labs(title = "Return Level Plot", x = "Návratová perioda (log)",y = "Návratová úroveň") +
  theme_bw()

grid.arrange(
  ppplot, qqplot, rlplot, hist,
  nrow = 2, ncol = 2
)

# ------------------------------------------------------------------------------------------------------------------
# u2c ODHAD VaR a ES
# ------------------------------------------------------------------------------------------------------------------
# uvedeno jiz opet jako zaporna hodnota, tzn vynosy = kladne, ztraty = zaporne
alpha <- c(.05,.025,.01,.005)
n <- length(rpc)
n_u <- length(exc_rpc)
VaR_pot_rpc2 <- -(threshold_rpc + (scale / shape) * ((((n / n_u) * (alpha))^(-shape)) - 1))
ES_pot_rpc2 <- -sapply(VaR_pot_rpc, function(VaR) {(-VaR/(1-shape))+((scale-shape*threshold_rpc)/(1-shape))})

# intervalove odhady - bootstrap:
set.seed(123)
B <- 1000
VaR_boot_pot <- data.frame(numeric(B),numeric(B),numeric(B), numeric(B))
ES_boot_pot <- data.frame(numeric(B),numeric(B),numeric(B), numeric(B))
# POZOR, bootstrap je vypocetne narocny, trva to dele!
for (i in 1:B) {
  resample <- sample(exc_rpc, replace = TRUE)
  fit_boot <- fevd(resample, threshold = 0, type = "GP")
  params_boot <- fit_boot$results$par
  for (j in 1:length(alpha)) {
    VaR_boot_pot[i,j] <- -(threshold_rpc + (params_boot["scale"] / params_boot["shape"]) * 
                             ((((n / n_u) * (alpha[j]))^(-params_boot["shape"])) - 1))
    ES_boot_pot[i,j] <- -((-VaR_boot_pot[i,j]/(1- params_boot["shape"]))+((params_boot["scale"]-
                                                                             params_boot["shape"]*threshold_rpc)/(1- params_boot["shape"])))
  }
}
# 95% interval spolehlivosti:
ci_var_pot_c2 <- data.frame(numeric(2), numeric(2), numeric(2))
ci_es_pot_c2 <- data.frame(numeric(2), numeric(2), numeric(2))
for (j in 1:length(alpha)) {
  ci_var_pot_c2[j,] <- quantile(VaR_boot_pot[,j], probs = c(0.025,.5, 0.975))
  ci_es_pot_c2[j,] <- quantile(ES_boot_pot[,j], probs = c(0.025,.5, 0.975))
}
ci_var_pot_c2
ci_es_pot_c2

#totez pro declusterizovany model:
nD <- length(rpc_decl)
n_uD <- length(exc_rpcD)
VaR_pot_rpc_D2 <- -(threshold_rpc + (scaleD / shapeD) * ((((nD / n_uD) * (alpha))^(-shapeD)) - 1))
ES_pot_rpc_D2 <- -sapply(VaR_pot_rpc_D, function(VaR) {(-VaR/(1-shapeD))+((scaleD-shapeD*threshold_rpc)/(1-shapeD))})
# intervalove odhady - bootstrap:
VaR_boot_potD <- data.frame(numeric(B),numeric(B),numeric(B), numeric(B))
ES_boot_potD <- data.frame(numeric(B),numeric(B),numeric(B), numeric(B))
# POZOR, bootstrap je vypocetne narocny, trva to dele!
for (i in 1:B) {
  resample <- sample(exc_rpcD, replace = TRUE)
  fit_boot <- fevd(resample, threshold = 0, type = "GP")
  params_boot <- fit_boot$results$par
  for (j in 1:length(alpha)) {
    VaR_boot_potD[i,j] <- -(threshold_rpc + (params_boot["scale"] / params_boot["shape"]) * 
                              ((((nD / n_uD) * (alpha[j]))^(-params_boot["shape"])) - 1))
    ES_boot_potD[i,j] <- -((-VaR_boot_potD[i,j]/(1- params_boot["shape"]))+((params_boot["scale"]-
                                                                               params_boot["shape"]*threshold_rpc)/(1- params_boot["shape"])))
  }
}
# 95% interval spolehlivosti:
ci_var_potD_c2<- data.frame(numeric(2), numeric(2), numeric(2))
ci_es_potD_c2 <- data.frame(numeric(2), numeric(2), numeric(2))
for (j in 1:length(alpha)) {
  ci_var_potD_c2[j,] <- quantile(VaR_boot_potD[,j], probs = c(0.025,.5, 0.975))
  ci_es_potD_c2[j,] <- quantile(ES_boot_potD[,j], probs = c(0.025,.5, 0.975))
}
ci_var_potD_c2
ci_es_potD_c2

# ------------------------------------------------------------------------------------------------------------------
# u2c BACKTESTING - TESTOVACI DATA
# ------------------------------------------------------------------------------------------------------------------
# # viz samostatny soubor Tomancova_backtesting.R




# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# POT PRO DYNAMICKE PORTFOLIO
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# volba prahu u - viz samostatny RScript Tomancova_thresholds
# ------------------------------------------------------------------------------------------------------------------
#vysledne volby pro konzervativni portflio:
u1d <- 2.35
u2d <- 3.05

# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# MODELY PRO u1d
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
threshold_rpd <- u1d
pot_rpd <- rpd[rpd > threshold_rpd]

# ------------------------------------------------------------------------------------------------------------------
# u1d DECLUSTERING - volba vhodného r
# ------------------------------------------------------------------------------------------------------------------
rpd_zoo <- zoo(rpd$`Daily r_p dynam.`, order.by = index(rpd))

mingap_rpd <- 7 # minimální odstup přesahů přes práh je 1 týden -> zbyde 44 ze 70
# Funkce na identifikaci clusterů
clust_rpd <- cumsum(c(1, diff(index(pot_rpd))) > mingap_rpd)
clust_rpd

# Výběr maxima z každého clusteru v datech
pot_rpd_declust <- cbind(index(pot_rpd), data.frame(pot_rpd) , clust_rpd)
pot_rpd_declust <- pot_rpd_declust %>%
  group_by(clust_rpd) %>%                     # Skupinování podle clust_rpd
  mutate(is_group_max = if_else(Daily.r_p.dynam. == max(Daily.r_p.dynam.), 1, 0)) %>%  # Výběr maxima z každé skupiny
  ungroup() 
pot_rpd_declust <- xts(pot_rpd_declust, order.by = pot_rpd_declust$`index(pot_rpd)`)
pot_rpd_data <- pot_rpd_declust$`Daily.r_p.dynam.`[pot_rpd_declust$is_group_max==1]
length(pot_rpd_data)
zavisla_ind <- index(pot_rpd_declust$Daily.r_p.dynam.[pot_rpd_declust$is_group_max==0])
rpd_decl <- rpd[!(index(rpd)%in% zavisla_ind)]
length(rpd_decl)


# ------------------------------------------------------------------------------------------------------------------
# u1d MODELOVÁNÍ GPD
# ------------------------------------------------------------------------------------------------------------------
fit_rpd_pot <- fevd(rpd, threshold = threshold_rpd, type = "GP")
summary(fit_rpd_pot)
ci(fit_rpd_pot, type = "parameter")
exc_rpd <- pot_rpd[pot_rpd > threshold_rpd] - threshold_rpd
scale <- fit_rpd_pot$results$par[1]
shape <- fit_rpd_pot$results$par[2]
#ulozeni parametru pro ucely backtestingu
scale_pot1_d <- fit_rpc_pot$results$par[1]
shape_pot1_d <- fit_rpc_pot$results$par[2]
rate_pot1_d <- length(pot_rpd)/length(rpd)

fit_rpd_pot_declust <- fevd(as.numeric(pot_rpd_data), threshold = threshold_rpd, type = "GP")
summary(fit_rpd_pot_declust)
ci(fit_rpd_pot_declust, type = "parameter")
exc_rpdD <- as.numeric(pot_rpd_data$Daily.r_p.dynam.) - threshold_rpd
scaleD <- fit_rpd_pot_declust$results$par[1]
shapeD <- fit_rpd_pot_declust$results$par[2]
#ulozeni parametru pro ucely backtestingu
scale_potD1_d <- fit_rpc_pot_declust$results$par[1]
shape_potD1_d <- fit_rpc_pot_declust$results$par[2]
rate_potD1_d <- length(pot_rpd_data)/length(rpd_decl)

#udaje do tabulky pro u1d:
is <- round(ci(fit_rpd_pot, type = "parameter")[1:2,1:3],5) 
is_char <- c(paste0("(", is[,1], ";", is[,3], ")"))
df_par_est_d <- data.frame(matrix(nrow=2, ncol=4))
df_par_est_d <- cbind(is,is_char)
is <- round(ci(fit_rpd_pot_declust, type = "parameter")[1:2,1:3],5) 
is_char <- c(paste0("(", is[,1], ";", is[,3], ")"))
df_par_est_d <- rbind(df_par_est_d, cbind(is,is_char))

# ------------------------------------------------------------------------------------------------------------------
# u1d DIAGNOSTIKA MODELŮ
# ------------------------------------------------------------------------------------------------------------------
# A) PRO PUVODNI MODEL

#ECDF a nafitovana teoreticka CDF
empirical_cdf <- ecdf(as.vector(exc_rpd$`Daily r_p dynam.`))
x_vals <- seq(-5, 20, length.out = 1000)  # Body na ose x pro vygenerovani teoreticke cdf
theoretical_cdf <- pevd(x_vals, scale = scale, shape = shape, type="GP")  # Teoretická CDF
plot(empirical_cdf, verticals = TRUE, do.points = FALSE, col=2, xlim=c(-5,20), xlab = "y", 
     ylab = expression(F[u](y)), main = "Dynamické portfolio")
lines(x_vals, theoretical_cdf, type="l")

#PPPLOT:
empirical_cdf <- ecdf(as.vector(exc_rpd))
empirical_probs <- empirical_cdf(as.vector(exc_rpd))
theoretical_probs <- pevd(as.vector(exc_rpd), scale = scale, shape = shape, type="GP")  # Teoretická CDF
plot_data <- data.frame(empirical = empirical_probs,  theoretical = theoretical_probs)
ppplot <- ggplot(plot_data, aes(x = theoretical, y = empirical)) +
  geom_point(color = "black") +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "P-P Plot - Dynamické p.", x = "Teoretické pravděpodobnosti",y = "Empirické pravděpodobnosti") +
  theme_bw()

#QQPLOT:
qqplot <- ggplot(exc_rpd, aes(sample = as.vector(exc_rpd$`Daily r_p dynam.`))) +
  stat_qq(distribution = extRemes::qevd, dparams = list(scale = scale, shape = shape, loc = 0, type = "GP")) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Teoretické kvantily", y = "Empirické kvantily", title = "Q-Q Plot") + xlim(c(0,17)) + ylim(c(0,17)) +
  theme_bw()

# Histogram s hustotou odhadnuteho rozdeleni
hist <- ggplot(exc_rpd, aes(x = as.vector(exc_rpd$`Daily r_p dynam.`))) +
  geom_histogram(mapping = aes(y = after_stat(density)), binwidth = 1, col = "gray30", fill = "dimgrey", alpha = .2,) +
  stat_function(fun = extRemes::devd, args = list(scale = scale, shape = shape, type = "GP"), col = "black", size = .7) +
  labs(x = "Exces", y = "Hustota", title = "Histogram") +
  theme_bw()

# Return Level Plot
return_periods <- c(1.1, 2, 5, 10, 20, 50, 100, 500)  # Doby návratu
return_levels <- return.level(fit_rpd_pot, return.period = return_periods)
n_y <- 365 # pocet dnu v roce v teto casove rade 
ci <- ci(fit_rpd_pot, type = "return.level", return.period = return_periods) # Intervaly spolehlivosti pro návratové hladiny
plot_data <- data.frame(return_period = return_periods, return_level = return_levels, ci_lower = ci[, 1], ci_upper = ci[, 3])
empirical_levels <- sort(as.vector(pot_rpd), decreasing = TRUE)
empirical_periods <- (((length(rpd)) + 1) / index(empirical_levels))/365
empirical_periods
empirical_data <- data.frame(period = empirical_periods, level = empirical_levels)
rlplot <- ggplot() +
  geom_line(data = plot_data, aes(x = return_period, y = return_level), color = "black", size = 1) +  # Teoretická křivka
  geom_point(data = empirical_data, aes(x = period, y = level), color = "black", size = 2) +  # Empirické body
  geom_ribbon(data = plot_data, aes(x= return_period, ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "darkgray") +  # Intervaly spolehlivosti
  scale_x_log10() +  # Logaritmická osa x
  labs(title = "Return Level Plot", x = "Návratová perioda (log)",y = "Návratová úroveň") +
  theme_bw()

grid.arrange(
  ppplot, qqplot, rlplot, hist,
  nrow = 2, ncol = 2
)

# B) PRO DECLUST MODEL

#ECDF a nafitovana teoreticka CDF
empirical_cdf <- ecdf(as.vector(exc_rpdD))
x_vals <- seq(-5, 20, length.out = 1000)  # Body na ose x pro vygenerovani teoreticke cdf
theoretical_cdf <- pevd(x_vals, scale = scaleD, shape = shapeD, type="GP")  # Teoretická CDF
plot(empirical_cdf, verticals = TRUE, do.points = FALSE, col=2, xlim=c(-5,20), xlab = "y", 
     ylab = expression(F[u](y)), main = "Dynamické portfolio")
lines(x_vals, theoretical_cdf, type="l")

#PPPLOT:
empirical_cdf <- ecdf(as.vector(exc_rpdD))
empirical_probs <- empirical_cdf(as.vector(exc_rpdD))
theoretical_probs <- pevd(as.vector(exc_rpdD), scale = scaleD, shape = shapeD, type="GP")  # Teoretická CDF
plot_data <- data.frame(empirical = empirical_probs,theoretical = theoretical_probs)
ppplot <- ggplot(plot_data, aes(x = theoretical, y = empirical)) +
  geom_point(color = "black") +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "P-P Plot - Dynamické p.", x = "Teoretické pravděpodobnosti",y = "Empirické pravděpodobnosti") +
  theme_bw()

#QQPLOT:
qqplot <- ggplot(as.data.frame(exc_rpdD), aes(sample = as.numeric(exc_rpdD))) +
  stat_qq(distribution = extRemes::qevd, dparams = list(scale = scaleD, shape = shapeD, loc = 0, type = "GP")) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Teoretické kvantily", y = "Empirické kvantily", title = "Q-Q Plot") + xlim(0,17) + ylim(0,17) +
  theme_bw()

# Histogram s hustotou odhadnuteho rozdeleni
hist<- ggplot(as.data.frame(exc_rpdD), aes(x = as.vector(exc_rpdD))) +
  geom_histogram(mapping = aes(y = after_stat(density)), binwidth = 1, col = "gray30", fill = "dimgrey", alpha = .2,) +
  stat_function(fun = extRemes::devd, args = list(scale = scaleD, shape = shapeD, type = "GP"), col = "black", size = .7) +
  labs(x = "Exces", y = "Hustota", title = "Histogram") +
  theme_bw()

# Return Level Plot 
return_periods <- c(1.1, 2, 5, 10, 20, 50, 100, 500)  # Doby návratu
return_levels <- return.level(fit_rpd_pot_declust, return.period = return_periods)
n_y <- round(length(rpd_decl)/7) # pocet dnu v roce v teto casove rade 
ci <- ci(fit_rpd_pot_declust, type = "return.level", return.period = return_periods) # Intervaly spolehlivosti pro návratové hladiny
plot_data <- data.frame(return_period = return_periods, return_level = return_levels, ci_lower = ci[, 1], ci_upper = ci[, 3])
empirical_levels <- sort(as.numeric(pot_rpd_data), decreasing = TRUE)
empirical_periods <- (((length(rpd_decl)) + 1) / index(empirical_levels))/n_y
empirical_data <- data.frame(period = empirical_periods, level = empirical_levels)
rlplot <- ggplot() +
  geom_line(data = plot_data, aes(x = return_period, y = return_level), color = "black", size = 1) +  # Teoretická křivka
  geom_point(data = empirical_data, aes(x = period, y = level), color = "black", size = 2) +  # Empirické body
  geom_ribbon(data = plot_data, aes(x= return_period, ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "darkgray") +  # Intervaly spolehlivosti
  scale_x_log10() +  # Logaritmická osa x
  labs(title = "Return Level Plot", x = "Návratová perioda (log)",y = "Návratová úroveň") +
  theme_bw()

grid.arrange(
  ppplot, qqplot, rlplot, hist,
  nrow = 2, ncol = 2
)

# ------------------------------------------------------------------------------------------------------------------
# u1d ODHAD VaR a ES
# ------------------------------------------------------------------------------------------------------------------
# uvedeno jiz opet jako zaporna hodnota, tzn vynosy = kladne, ztraty = zaporne
alpha <- c(.05,.025,.01,.005)
n <- length(rpd)
n_u <- length(pot_rpd_data)
VaR_pot_rpd <- -(threshold_rpd + (scale / shape) * ((((n / n_u) * (alpha))^(-shape)) - 1))
ES_pot_rpd <- -sapply(VaR_pot_rpd, function(VaR) {(-VaR/(1-shape))+((scale-shape*threshold_rpd)/(1-shape))})

# intervalove odhady - bootstrap:
set.seed(123)
B <- 1000
VaR_boot_pot <- data.frame(numeric(B),numeric(B),numeric(B), numeric(B))
ES_boot_pot <- data.frame(numeric(B),numeric(B),numeric(B), numeric(B))
# POZOR, bootstrap je vypocetne narocny, trva to dele!
for (i in 1:B) {
  resample <- sample(exc_rpd, replace = TRUE)
  fit_boot <- fevd(resample, threshold = 0, type = "GP")
  params_boot <- fit_boot$results$par
  for (j in 1:length(alpha)) {
    VaR_boot_pot[i,j] <- -(threshold_rpd + (params_boot["scale"] / params_boot["shape"]) * 
                             ((((n / n_u) * (alpha[j]))^(-params_boot["shape"])) - 1))
    ES_boot_pot[i,j] <- -((-VaR_boot_pot[i,j]/(1- params_boot["shape"]))+((params_boot["scale"]-
                                                                             params_boot["shape"]*threshold_rpd)/(1- params_boot["shape"])))
  }
}
# 95% interval spolehlivosti:
ci_var_pot_d <- data.frame(numeric(2), numeric(2), numeric(2))
ci_es_pot_d <- data.frame(numeric(2), numeric(2), numeric(2))
for (j in 1:length(alpha)) {
  ci_var_pot_d[j,] <- quantile(VaR_boot_pot[,j], probs = c(0.025,.5, 0.975))
  ci_es_pot_d[j,] <- quantile(ES_boot_pot[,j], probs = c(0.025,.5, 0.975))
}
ci_var_pot_d
ci_es_pot_d

#totez pro declusterizovany model:
# intervalove odhady - bootstrap:
VaR_boot_potD <- data.frame(numeric(B),numeric(B),numeric(B), numeric(B))
ES_boot_potD <- data.frame(numeric(B),numeric(B),numeric(B), numeric(B))
n <- length(rpd_decl)
n_u <- length(exc_rpdD)
# POZOR, bootstrap je vypocetne narocny, trva to dele!
for (i in 1:B) {
  resample <- sample(exc_rpdD, replace = TRUE)
  fit_boot <- fevd(resample, threshold = 0, type = "GP")
  params_boot <- fit_boot$results$par
  for (j in 1:length(alpha)) {
    VaR_boot_potD[i,j] <- -(threshold_rpd + (params_boot["scale"] / params_boot["shape"]) * 
                              ((((n / n_u) * (alpha[j]))^(-params_boot["shape"])) - 1))
    ES_boot_potD[i,j] <- -((-VaR_boot_potD[i,j]/(1- params_boot["shape"]))+((params_boot["scale"]-
                                                                               params_boot["shape"]*threshold_rpd)/(1- params_boot["shape"])))
  }
}
# 95% interval spolehlivosti:
ci_var_potD_d<- data.frame(numeric(2), numeric(2), numeric(2))
ci_es_potD_d <- data.frame(numeric(2), numeric(2), numeric(2))
for (j in 1:length(alpha)) {
  ci_var_potD_d[j,] <- quantile(VaR_boot_potD[,j], probs = c(0.025,.5, 0.975))
  ci_es_potD_d[j,] <- quantile(ES_boot_potD[,j], probs = c(0.025,.5, 0.975))
}
ci_var_potD_d
ci_es_potD_d

# ------------------------------------------------------------------------------------------------------------------
# u1d BACKTESTING - TESTOVACI DATA
# ------------------------------------------------------------------------------------------------------------------
# # viz samostatny soubor Tomancova_backtesting.R


# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# MODELY PRO u2d
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
threshold_rpd <- u2d
pot_rpd <- rpd[rpd > threshold_rpd]

# ------------------------------------------------------------------------------------------------------------------
# u2d DECLUSTERING - volba vhodného r
# ------------------------------------------------------------------------------------------------------------------
rpd_zoo <- zoo(rpd$`Daily r_p dynam.`, order.by = index(rpd))

mingap_rpd <- 7 # minimální odstup přesahů přes práh je 1 týden -> zbyde 44 ze 70
# Funkce na identifikaci clusterů
clust_rpd <- cumsum(c(1, diff(index(pot_rpd))) > mingap_rpd)
clust_rpd

# Výběr maxima z každého clusteru v datech
pot_rpd_declust <- cbind(index(pot_rpd), data.frame(pot_rpd) , clust_rpd)
pot_rpd_declust <- pot_rpd_declust %>%
  group_by(clust_rpd) %>%                     # Skupinování podle clust_rpd
  mutate(is_group_max = if_else(Daily.r_p.dynam. == max(Daily.r_p.dynam.), 1, 0)) %>%  # Výběr maxima z každé skupiny
  ungroup() 
pot_rpd_declust <- xts(pot_rpd_declust, order.by = pot_rpd_declust$`index(pot_rpd)`)
pot_rpd_data <- pot_rpd_declust$`Daily.r_p.dynam.`[pot_rpd_declust$is_group_max==1]
length(pot_rpd_data)
zavisla_ind <- index(pot_rpd_declust$Daily.r_p.dynam.[pot_rpd_declust$is_group_max==0])
rpd_decl <- rpd[!(index(rpd)%in% zavisla_ind)]
length(rpd_decl)


# ------------------------------------------------------------------------------------------------------------------
# u2d MODELOVÁNÍ GPD
# ------------------------------------------------------------------------------------------------------------------
fit_rpd_pot <- fevd(rpd, threshold = threshold_rpd, type = "GP")
summary(fit_rpd_pot)
ci(fit_rpd_pot, type = "parameter")
summary(fevd(rpd, threshold = threshold_rpd, type =  "GP", method = "Lmoments")) # kontrola robustnosti odhadu
exc_rpd <- pot_rpd[pot_rpd > threshold_rpd] - threshold_rpd
scale <- fit_rpd_pot$results$par[1]
shape <- fit_rpd_pot$results$par[2]
#ulozeni parametru pro ucely backtestingu
scale_pot2_d <- fit_rpc_pot$results$par[1]
shape_pot2_d <- fit_rpc_pot$results$par[2]
rate_pot2_d <- length(pot_rpd)/length(rpd)

fit_rpd_pot_declust <- fevd(as.numeric(pot_rpd_data), threshold = threshold_rpd, type = "GP")
summary(fit_rpd_pot_declust)
ci(fit_rpd_pot_declust, type = "parameter")
summary(fevd(as.numeric(pot_rpd_data), threshold = threshold_rpd, type =  "GP", method = "Lmoments")) # kontrola robustnosti odhadu
exc_rpdD <- as.numeric(pot_rpd_data$Daily.r_p.dynam.) - threshold_rpd
scaleD <- fit_rpd_pot_declust$results$par[1]
shapeD <- fit_rpd_pot_declust$results$par[2]
#ulozeni parametru pro ucely backtestingu
scale_potD2_d <- fit_rpc_pot_declust$results$par[1]
shape_potD2_d <- fit_rpc_pot_declust$results$par[2]
rate_potD2_d <- length(pot_rpd_data)/length(rpd_decl)

#udaje do tabulky pro u2d:
is <- round(ci(fit_rpd_pot, type = "parameter")[1:2,1:3],5) 
is_char <- c(paste0("(", is[,1], ";", is[,3], ")"))
df_par_est_d <- rbind(df_par_est_d, cbind(is,is_char))
is <- round(ci(fit_rpd_pot_declust, type = "parameter")[1:2,1:3],5) 
is_char <- c(paste0("(", is[,1], ";", is[,3], ")"))
df_par_est_d <- rbind(df_par_est_d, cbind(is,is_char))
colnames(df_par_est_d) <- c( "lower", "Bodový odhad", "upper", "Intervalový odhad")
rownames(df_par_est_d) <- rep(c("sigma", "xi"), 4)
prah <- c("$u_1 = 2,35$", "$u_1 = 2,35$", "$u_1 = 2,35$", "$u_1 = 2,35$", "$u_2 = 3,05$", "$u_2 = 3,05$", "$u_2 = 3,05$", "$u_2 = 3,05$")
xtable(data.frame('Práh' = prah, 'Model' = cbind(rep(c("GP", "Declust. GP"),4)), df_par_est_d[c(1,3,5,7,2,4,6,8),c(2,4)]))
# ------------------------------------------------------------------------------------------------------------------
# u2d DIAGNOSTIKA MODELŮ
# ------------------------------------------------------------------------------------------------------------------
# A) PRO PUVODNI MODEL

#ECDF a nafitovana teoreticka CDF
empirical_cdf <- ecdf(as.vector(exc_rpd$`Daily r_p dynam.`))
x_vals <- seq(-5, 20, length.out = 1000)  # Body na ose x pro vygenerovani teoreticke cdf
theoretical_cdf <- pevd(x_vals, scale = scale, shape = shape, type="GP")  # Teoretická CDF
plot(empirical_cdf, verticals = TRUE, do.points = FALSE, col=2, xlim=c(-5,20), xlab = "y", 
     ylab = expression(F[u](y)), main = "Dynamické portfolio")
lines(x_vals, theoretical_cdf, type="l")

#PPPLOT:
empirical_cdf <- ecdf(as.vector(exc_rpd))
empirical_probs <- empirical_cdf(as.vector(exc_rpd))
theoretical_probs <- pevd(as.vector(exc_rpd), scale = scale, shape = shape, type="GP")  # Teoretická CDF
plot_data <- data.frame(empirical = empirical_probs,  theoretical = theoretical_probs)
ppplot <- ggplot(plot_data, aes(x = theoretical, y = empirical)) +
  geom_point(color = "black") +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "P-P Plot - Dynamické p.", x = "Teoretické pravděpodobnosti",y = "Empirické pravděpodobnosti") +
  theme_bw()

#QQPLOT:
qqplot <- ggplot(exc_rpd, aes(sample = as.vector(exc_rpd$`Daily r_p dynam.`))) +
  stat_qq(distribution = extRemes::qevd, dparams = list(scale = scale, shape = shape, loc = 0, type = "GP")) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Teoretické kvantily", y = "Empirické kvantily", title = "Q-Q Plot") + xlim(c(0,17)) + ylim(c(0,17)) +
  theme_bw()

# Histogram s hustotou odhadnuteho rozdeleni
hist <- ggplot(exc_rpd, aes(x = as.vector(exc_rpd$`Daily r_p dynam.`))) +
  geom_histogram(mapping = aes(y = after_stat(density)), binwidth = 1, col = "gray30", fill = "dimgrey", alpha = .2,) +
  stat_function(fun = extRemes::devd, args = list(scale = scale, shape = shape, type = "GP"), col = "black", size = .7) +
  labs(x = "Exces", y = "Hustota", title = "Histogram") +
  theme_bw()

# Return Level Plot
return_periods <- c(1.1, 2, 5, 10, 20, 50, 100, 500)  # Doby návratu
return_levels <- return.level(fit_rpd_pot, return.period = return_periods)
n_y <- 365 # pocet dnu v roce v teto casove rade 
ci <- ci(fit_rpd_pot, type = "return.level", return.period = return_periods) # Intervaly spolehlivosti pro návratové hladiny
plot_data <- data.frame(return_period = return_periods, return_level = return_levels, ci_lower = ci[, 1], ci_upper = ci[, 3])
empirical_levels <- sort(as.vector(pot_rpd), decreasing = TRUE)
empirical_periods <- (((length(rpd)) + 1) / index(empirical_levels))/365
empirical_periods
empirical_data <- data.frame(period = empirical_periods, level = empirical_levels)
rlplot <- ggplot() +
  geom_line(data = plot_data, aes(x = return_period, y = return_level), color = "black", size = 1) +  # Teoretická křivka
  geom_point(data = empirical_data, aes(x = period, y = level), color = "black", size = 2) +  # Empirické body
  geom_ribbon(data = plot_data, aes(x= return_period, ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "darkgray") +  # Intervaly spolehlivosti
  scale_x_log10() +  # Logaritmická osa x
  labs(title = "Return Level Plot", x = "Návratová perioda (log)",y = "Návratová úroveň") +
  theme_bw()

grid.arrange(
  ppplot, qqplot, rlplot, hist,
  nrow = 2, ncol = 2
)

# B) PRO DECLUST MODEL

#ECDF a nafitovana teoreticka CDF
empirical_cdf <- ecdf(as.vector(exc_rpdD))
x_vals <- seq(-5, 20, length.out = 1000)  # Body na ose x pro vygenerovani teoreticke cdf
theoretical_cdf <- pevd(x_vals, scale = scaleD, shape = shapeD, type="GP")  # Teoretická CDF
plot(empirical_cdf, verticals = TRUE, do.points = FALSE, col=2, xlim=c(-5,20), xlab = "y", 
     ylab = expression(F[u](y)), main = "Dynamické portfolio")
lines(x_vals, theoretical_cdf, type="l")

#PPPLOT:
empirical_cdf <- ecdf(as.vector(exc_rpdD))
empirical_probs <- empirical_cdf(as.vector(exc_rpdD))
theoretical_probs <- pevd(as.vector(exc_rpdD), scale = scaleD, shape = shapeD, type="GP")  # Teoretická CDF
plot_data <- data.frame(empirical = empirical_probs,theoretical = theoretical_probs)
ppplot <- ggplot(plot_data, aes(x = theoretical, y = empirical)) +
  geom_point(color = "black") +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "P-P Plot - Dynamické p.", x = "Teoretické pravděpodobnosti",y = "Empirické pravděpodobnosti") +
  theme_bw()

#QQPLOT:
qqplot <- ggplot(as.data.frame(exc_rpdD), aes(sample = as.numeric(exc_rpdD))) +
  stat_qq(distribution = extRemes::qevd, dparams = list(scale = scaleD, shape = shapeD, loc = 0, type = "GP")) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Teoretické kvantily", y = "Empirické kvantily", title = "Q-Q Plot") + xlim(0,17) + ylim(0,17) +
  theme_bw()

# Histogram s hustotou odhadnuteho rozdeleni
hist<- ggplot(as.data.frame(exc_rpdD), aes(x = as.vector(exc_rpdD))) +
  geom_histogram(mapping = aes(y = after_stat(density)), binwidth = 1, col = "gray30", fill = "dimgrey", alpha = .2,) +
  stat_function(fun = extRemes::devd, args = list(scale = scaleD, shape = shapeD, type = "GP"), col = "black", size = .7) +
  labs(x = "Exces", y = "Hustota", title = "Histogram") +
  theme_bw()

# Return Level Plot 
return_periods <- c(1.1, 2, 5, 10, 20, 50, 100, 500)  # Doby návratu
return_levels <- return.level(fit_rpd_pot_declust, return.period = return_periods)
n_y <- round(length(rpd_decl)/7) # pocet dnu v roce v teto casove rade 
ci <- ci(fit_rpd_pot_declust, type = "return.level", return.period = return_periods) # Intervaly spolehlivosti pro návratové hladiny
plot_data <- data.frame(return_period = return_periods, return_level = return_levels, ci_lower = ci[, 1], ci_upper = ci[, 3])
empirical_levels <- sort(as.numeric(pot_rpd_data), decreasing = TRUE)
empirical_periods <- (((length(rpd_decl)) + 1) / index(empirical_levels))/n_y
empirical_data <- data.frame(period = empirical_periods, level = empirical_levels)
rlplot <- ggplot() +
  geom_line(data = plot_data, aes(x = return_period, y = return_level), color = "black", size = 1) +  # Teoretická křivka
  geom_point(data = empirical_data, aes(x = period, y = level), color = "black", size = 2) +  # Empirické body
  geom_ribbon(data = plot_data, aes(x= return_period, ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "darkgray") +  # Intervaly spolehlivosti
  scale_x_log10() +  # Logaritmická osa x
  labs(title = "Return Level Plot", x = "Návratová perioda (log)",y = "Návratová úroveň") +
  theme_bw()

grid.arrange(
  ppplot, qqplot, rlplot, hist,
  nrow = 2, ncol = 2
)

# ------------------------------------------------------------------------------------------------------------------
# u2d ODHAD VaR a ES
# ------------------------------------------------------------------------------------------------------------------
# uvedeno jiz opet jako zaporna hodnota, tzn vynosy = kladne, ztraty = zaporne
alpha <- c(.05,.025,.01,.005)
n <- length(rpd)
n_u <- length(pot_rpd_data)
VaR_pot_rpd2 <- -(threshold_rpd + (scale / shape) * ((((n / n_u) * (alpha))^(-shape)) - 1))
ES_pot_rpd2 <- -sapply(VaR_pot_rpd, function(VaR) {(-VaR/(1-shape))+((scale-shape*threshold_rpd)/(1-shape))})

# intervalove odhady - bootstrap:
set.seed(123)
B <- 1000
VaR_boot_pot <- data.frame(numeric(B),numeric(B),numeric(B), numeric(B))
ES_boot_pot <- data.frame(numeric(B),numeric(B),numeric(B), numeric(B))
# POZOR, bootstrap je vypocetne narocny, trva to dele!
for (i in 1:B) {
  resample <- sample(exc_rpd, replace = TRUE)
  fit_boot <- fevd(resample, threshold = 0, type = "GP")
  params_boot <- fit_boot$results$par
  for (j in 1:length(alpha)) {
    VaR_boot_pot[i,j] <- -(threshold_rpd + (params_boot["scale"] / params_boot["shape"]) * 
                             ((((n / n_u) * (alpha[j]))^(-params_boot["shape"])) - 1))
    ES_boot_pot[i,j] <- -((-VaR_boot_pot[i,j]/(1- params_boot["shape"]))+((params_boot["scale"]-
                                                                             params_boot["shape"]*threshold_rpd)/(1- params_boot["shape"])))
  }
}
# 95% interval spolehlivosti:
ci_var_pot_d2 <- data.frame(numeric(2), numeric(2), numeric(2))
ci_es_pot_d2 <- data.frame(numeric(2), numeric(2), numeric(2))
for (j in 1:length(alpha)) {
  ci_var_pot_d2[j,] <- quantile(VaR_boot_pot[,j], probs = c(0.025,.5, 0.975))
  ci_es_pot_d2[j,] <- quantile(ES_boot_pot[,j], probs = c(0.025,.5, 0.975))
}
ci_var_pot_d2
ci_es_pot_d2

#totez pro declusterizovany model:
# intervalove odhady - bootstrap:
VaR_boot_potD <- data.frame(numeric(B),numeric(B),numeric(B), numeric(B))
ES_boot_potD <- data.frame(numeric(B),numeric(B),numeric(B), numeric(B))
n <- length(rpd_decl)
n_u <- length(exc_rpdD)
# POZOR, bootstrap je vypocetne narocny, trva to dele!
for (i in 1:B) {
  resample <- sample(exc_rpdD, replace = TRUE)
  fit_boot <- fevd(resample, threshold = 0, type = "GP")
  params_boot <- fit_boot$results$par
  for (j in 1:length(alpha)) {
    VaR_boot_potD[i,j] <- -(threshold_rpd + (params_boot["scale"] / params_boot["shape"]) * 
                              ((((n / n_u) * (alpha[j]))^(-params_boot["shape"])) - 1))
    ES_boot_potD[i,j] <- -((-VaR_boot_potD[i,j]/(1- params_boot["shape"]))+((params_boot["scale"]-
                                                                               params_boot["shape"]*threshold_rpd)/(1- params_boot["shape"])))
  }
}
# 95% interval spolehlivosti:
ci_var_potD_d2<- data.frame(numeric(2), numeric(2), numeric(2))
ci_es_potD_d2 <- data.frame(numeric(2), numeric(2), numeric(2))
for (j in 1:length(alpha)) {
  ci_var_potD_d2[j,] <- quantile(VaR_boot_potD[,j], probs = c(0.025,.5, 0.975))
  ci_es_potD_d2[j,] <- quantile(ES_boot_potD[,j], probs = c(0.025,.5, 0.975))
}
ci_var_potD_d2
ci_es_potD_d2

# ------------------------------------------------------------------------------------------------------------------
# BACKTESTING - TESTOVACI DATA
# ------------------------------------------------------------------------------------------------------------------
# # viz samostatny soubor Tomancova_backtesting.R


# ------------------------------------------------------------------------------------------------------------------
# KONEC SCRIPTU PRO METODU SPICEK NAD PRAHEM
# ------------------------------------------------------------------------------------------------------------------
