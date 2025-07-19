# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# RSCRIPT PRO APLIKACNI CAST PRACE - METODA BLOKOVYCH MAXIM
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
# BMM PRO KONZERVATIVNI PORTFOLIO
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# BLOK = 1 KVARTAL
# ------------------------------------------------------------------------------------------------------------------
n <- 365
m_n <- apply.quarterly(rpc,max)
m_n_index <- which(index(rpc) %in% index(rpc[rpc %in% m_n])) 

fit_con_gev <- fevd(m_n, type = "GEV")
summary(fit_con_gev)
summary(fevd(m_n, type = "GEV", method = "Lmoments")) # Pro overeni dostatcne robustnosti MLE odhadu jeste kontrola s odhady ziskanymi metodou Lmoments
ci(fit_con_gev, type = "parameter") # IS s kvantilem st. norm. rozd.
ci(fit_con_gev, type = "parameter", method = "boot") # IS metodou bootstrap
loc <- fit_con_gev$results$par[1]
scale <- fit_con_gev$results$par[2]
shape <- fit_con_gev$results$par[3]
#ulozeni parametru pro backtesting:
loc_bmmQ_c <- loc
scale_bmmQ_c <- scale
shape_bmmQ_c <- shape


#udaje do tabulky:
is <- round(ci(fit_con_gev, type = "parameter")[1:3,1:3],5) # IS v pripade Lmoments je ziskan bootstrapem, viz RDocumentation
is_char <- c(paste0("(", is[,1], ";", is[,3], ")"))
df_par_est_c <- data.frame(matrix(nrow=3, ncol=4))
df_par_est_c <- cbind(is,is_char)
# ------------------------------------------------------------------------------------------------------------------
# DIAGNOSTIKA (BLOK = 1 KVARTAL)
# ------------------------------------------------------------------------------------------------------------------
#ECDF a nafitovana teoreticka CDF
empirical_cdf <- ecdf(as.vector(m_n)) #ECDF
x_vals <- seq(-5,25, length.out = 10000)  # Body na ose x
theoretical_cdf <- pevd(x_vals, loc = loc, scale = scale, shape = shape, type="GEV")  # Teoretická CDF
plot(empirical_cdf, verticals = TRUE, do.points = FALSE, col=2, xlim=c(-5,20), main = "Konzervativní portfolio")
lines(x_vals, theoretical_cdf, type="l")

#PPPLOT:
empirical_cdf <- ecdf(as.vector(m_n))
empirical_probs <- empirical_cdf(as.vector(m_n))
theoretical_probs <- pevd(as.vector(m_n), loc = loc, scale = scale, shape = shape, type="GEV")  # Teoretická CDF
plot_data <- data.frame(empirical = empirical_probs,theoretical = theoretical_probs)
ppplot <- ggplot(plot_data, aes(x = theoretical, y = empirical)) +
  geom_point(color = "black") +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Teoretické pravděpodobnosti", y = "Empirické pravděpodobnosti", title = "P-P Plot - Konzervativní p." ) +
  theme_bw()

#QQPLOT:
qqplot <- ggplot(m_n, aes(sample = as.vector(m_n))) +
  stat_qq(distribution = extRemes::qevd, dparams = list(loc = loc, scale = scale, shape = shape, type = "GEV")) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Teoretické kvantily", y = "Empirické kvantily", title = "Q-Q Plot") +
  xlim(0,6) + ylim(0,6) +
  theme_bw()

# Histogram s hustotou odhadnuteho rozdeleni
hist <- ggplot(m_n, aes(x = as.vector(m_n))) +
  geom_histogram(mapping = aes(y = after_stat(density)),binwidth = .4,col = "gray30", fill = "dimgrey", alpha = .2,) +
  stat_function(fun = extRemes::devd, args = list(loc = loc, scale = scale, shape = shape, type = "GEV"), col = "black", size = .7) +
  labs(x = "x", y = "Hustota", title = "Histogram") +
  theme_bw()

# Return Level Plot
return_periods <- c(1.1, 2, 5, 10, 20, 50, 100, 500)  # Doby návratu
return_levels <- return.level(fit_con_gev, return.period = return_periods) 
ci <- ci(fit_con_gev, type = "return.level", return.period = return_periods) # Intervaly spolehlivosti pro návratové hladiny
plot_data <- data.frame(return_period = return_periods,  return_level = return_levels,  ci_lower = ci[, 1],  ci_upper = ci[, 3])
empirical_levels <- sort(as.vector(m_n), decreasing = TRUE)
empirical_periods <- (((length(rpc)) + 1) / index(empirical_levels))*4/n
empirical_data <- data.frame(period = empirical_periods, level = empirical_levels)
rlplot <- ggplot(data = plot_data, aes(x = return_period, y = return_level)) +
  geom_line(color = "black", size = 1) +  # Teoretická křivka
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
# ODHAD VaR a ES 
# ------------------------------------------------------------------------------------------------------------------
# uvedeno jiz opet jako zaporna hodnota, tzn vynosy = kladne, ztraty = zaporne
alpha <- c(.05, .025, .01, .005)
m <- n/4 # velikost jednoho bloku
var_gev_con_q <- -((loc -(scale/shape)*(1-((-m*log(1-alpha))^(-shape)))))

# Definice funkce pro numerickou integraci
integrand <- function(L) {
  L * (1 + shape * ((L-loc)/scale))^(-1/shape - 1) * exp(- (1/m) * (1 + shape * ((L-loc)/scale))^(-1/shape))
}
# Numerická integrace pro ES
compute_ES <- function(a, var_y) {
  integral_result <- integrate(integrand, lower = -var_y, upper = Inf)
  -(1 / (a * m * scale)) * integral_result$value
}
es_gev_con_q <- sapply(1:length(alpha), function(i) compute_ES(alpha[i], var_gev_con_q[i]))
var_gev_con_q
es_gev_con_q

# intervalove odhady - bootstrap:
set.seed(123)
B <- 1000
VaR_boot_bmmQ <- data.frame(numeric(B),numeric(B), numeric(B), numeric(B))
ES_boot_bmmQ <- data.frame(numeric(B),numeric(B), numeric(B), numeric(B))
bmm_bootQ <- data.frame(numeric(B),numeric(B), numeric(B), numeric(B))
# POZOR, bootstrap je vypocetne narocny, trva to dele!
for (i in 1:B) {
  resample <- sample(m_n, replace = TRUE)
  fit_boot <- fevd(resample, type = "GEV")
  params_boot <- fit_boot$results$par
  for (j in 1:length(alpha)) {
    VaR_boot_bmmQ[i,j] <- -((params_boot["location"] -(params_boot["scale"]/params_boot["shape"])*(1-((-m*log(1-alpha[j]))^(-params_boot["shape"])))))
    integrand <- function(L) {
      L * (1 + params_boot["shape"] * ((L-params_boot["location"])/params_boot["scale"]))^(-1/params_boot["shape"] - 1) *
        exp(- (1/m) * (1 + params_boot["shape"] * ((L-params_boot["location"])/params_boot["scale"]))^(-1/params_boot["shape"]))
    }
    # Pokus o integraci
    integral_result <- try(integrate(integrand, lower = -VaR_boot_bmmQ[i,j], upper = Inf), silent = TRUE)
    
    # Kontrola, zda integrace proběhla úspěšně
    if (inherits(integral_result, "try-error") || is.nan(integral_result$value)) {
      ES_boot_bmmQ[i,j] <- NaN
    } else {
      ES_boot_bmmQ[i,j] <- -(1 / (alpha[j] * m * params_boot["scale"])) * integral_result$value
    }
  }
  for (k in 1:3) {
    bmm_bootQ[i,k] <- params_boot[k]
  }
}
# 95% interval spolehlivosti:
ci_var_bmmQ<- data.frame(numeric(2), numeric(2), numeric(2))
ci_es_bmmQ <- data.frame(numeric(2), numeric(2), numeric(2))
for (j in 1:length(alpha)) {
  ci_var_bmmQ[j,] <- quantile(VaR_boot_bmmQ[,j], probs = c(0.025,.5, 0.975), na.rm = TRUE)
  ci_es_bmmQ[j,] <- quantile(ES_boot_bmmQ[,j], probs = c(0.025,.5, 0.975), na.rm = TRUE)
}
ci_var_bmmQ
ci_es_bmmQ

# bias odhadnuty empiricky z dat pomoci bootstrapu:
biasQ <- c(abs(mean(bmm_bootQ[[1]])-loc), abs(mean(bmm_bootQ[[2]])-scale), abs(mean(bmm_bootQ[[3]])-shape))
biasQ

# ------------------------------------------------------------------------------------------------------------------
# BLOK = 1 MESIC
# ------------------------------------------------------------------------------------------------------------------
m_n <- apply.monthly(rpc,max)
m_n_index <- which(index(rpc) %in% index(rpc[rpc %in% m_n])) 

fit_con_gev <- fevd(m_n, type = "GEV")
summary(fit_con_gev)
summary(fevd(m_n, type = "GEV", method = "Lmoments")) # Pro overeni dostatcne robustnosti MLE odhadu jeste kontrola s odhady ziskanymi metodou Lmoments
ci(fit_con_gev, type = "parameter") # IS s kvantilem st. norm. rozd.
ci(fit_con_gev, type = "parameter", method = "boot") # IS metodou bootstrap
loc <- fit_con_gev$results$par[1]
scale <- fit_con_gev$results$par[2]
shape <- fit_con_gev$results$par[3]
#ulozeni parametru pro backtesting:
loc_bmmM_c <- loc
scale_bmmM_c <- scale
shape_bmmM_c <- shape

#udaje do tabulky:
is <- round(ci(fit_con_gev, type = "parameter")[1:3,1:3],5) # IS v pripade MLE pracuje s approx. normalitou
is_char <- c(paste0("(", is[,1], ";", is[,3], ")"))
df_par_est_c <- rbind(df_par_est_c, cbind(is,is_char))

# ------------------------------------------------------------------------------------------------------------------
# DIAGNOSTIKA (BLOK = 1 MESIC)
# ------------------------------------------------------------------------------------------------------------------
#ECDF a nafitovana teoreticka CDF
empirical_cdf <- ecdf(as.vector(m_n)) #ECDF
x_vals <- seq(-5,25, length.out = 10000)  # Body na ose x
theoretical_cdf <- pevd(x_vals, loc = loc, scale = scale, shape = shape, type="GEV")  # Teoretická CDF
plot(empirical_cdf, verticals = TRUE, do.points = FALSE, col=2, xlim=c(-5,20), main = "Konzervativní portfolio")
lines(x_vals, theoretical_cdf, type="l")

#PPPLOT:
empirical_cdf <- ecdf(as.vector(m_n))
empirical_probs <- empirical_cdf(as.vector(m_n))
theoretical_probs <- pevd(as.vector(m_n), loc = loc, scale = scale, shape = shape, type="GEV")  # Teoretická CDF
plot_data <- data.frame(empirical = empirical_probs,theoretical = theoretical_probs)
ppplot <- ggplot(plot_data, aes(x = theoretical, y = empirical)) +
  geom_point(color = "black") +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "P-P Plot - Konzervativní p.", x = "Teoretické pravděpobnosti", y = "Empirické pravděpodobnosti") +
  theme_bw()

#QQPLOT:
qqplot <- ggplot(m_n, aes(sample = as.vector(m_n))) +
  stat_qq(distribution = extRemes::qevd,dparams = list(loc = loc,scale = scale,shape = shape,type = "GEV")) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Teoretické kvantily", y = "Empirické kvantily", title = "Q-Q Plot") +
  xlim(0,6) +  ylim(0,6) +
  theme_bw()

# Histogram s hustotou odhadnuteho rozdeleni
hist <- ggplot(m_n, aes(x = as.vector(m_n))) +
  geom_histogram(mapping = aes(y = after_stat(density)),binwidth = .3,col = "gray30",fill = "dimgrey",alpha = .2,) +
  stat_function(fun = extRemes::devd, args = list(loc = loc, scale = scale, shape = shape, type = "GEV"), col = "black", size = .7) +
  labs(x = "x", y = "Hustota", title = "Histogram") +
  theme_bw()

# Return Level Plot
return_periods <- c(1.1, 2, 5, 10, 20, 50, 100, 500)  # Doby návratu
return_levels <- return.level(fit_con_gev, return.period = return_periods) 
ci <- ci(fit_con_gev, type = "return.level", return.period = return_periods) # Intervaly spolehlivosti pro návratové hladiny
plot_data <- data.frame(return_period = return_periods,  return_level = return_levels,  ci_lower = ci[, 1],  ci_upper = ci[, 3])
empirical_levels <- sort(as.vector(m_n), decreasing = TRUE)
empirical_periods <- (((length(rpc)) + 1) / index(empirical_levels))*12/n
empirical_data <- data.frame(period = empirical_periods, level = empirical_levels)
rlplot <- ggplot(data = plot_data, aes(x = return_period, y = return_level)) +
  geom_line(color = "black", size = 1) +  # Teoretická křivka
  geom_point(data = empirical_data, aes(x = period, y = level), color = "black", size = 2) +  # Empirické body
  geom_ribbon(data = plot_data, aes(x= return_period, ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "darkgray") +  # Intervaly spolehlivosti
  scale_x_log10() +  # Logaritmická osa x
  labs(title = "Return Level Plot", x = "Návratová perioda (log)", y = "Návratová úroveň") +
  theme_bw()

grid.arrange(
  ppplot, qqplot, rlplot, hist,
  nrow = 2, ncol = 2
)
# ------------------------------------------------------------------------------------------------------------------
# ODHAD VaR a ES
# ------------------------------------------------------------------------------------------------------------------
# uvedeno jiz opet jako zaporna hodnota, tzn vynosy = kladne, ztraty = zaporne
alpha <- c(.05,.025,.01,.005)
m <- n/12 # velikost jednoho bloku
var_gev_con_m <- -((loc -(scale/shape)*(1-((-m*log(1-alpha))^(-shape)))))
es_gev_con_m <- sapply(1:length(alpha), function(i) compute_ES(alpha[i], var_gev_con_m[i]))
var_gev_con_m
es_gev_con_m

# intervalove odhady - bootstrap:
set.seed(123)
B <- 1000
VaR_boot_bmmM <- data.frame(numeric(B),numeric(B), numeric(B), numeric(B))
ES_boot_bmmM <- data.frame(numeric(B),numeric(B), numeric(B), numeric(B))
bmm_bootM <- data.frame(numeric(B),numeric(B), numeric(B), numeric(B))
# POZOR, bootstrap je vypocetne narocny, trva to dele!
for (i in 1:B) {
  resample <- sample(m_n, replace = TRUE)
  fit_boot <- fevd(resample, type = "GEV")
  params_boot <- fit_boot$results$par
  for (j in 1:length(alpha)) {
    VaR_boot_bmmM[i,j] <- -((params_boot["location"] -(params_boot["scale"]/params_boot["shape"])*(1-((-m*log(1-alpha[j]))^(-params_boot["shape"])))))
    integrand <- function(L) {
      L * (1 + params_boot["shape"] * ((L-params_boot["location"])/params_boot["scale"]))^(-1/params_boot["shape"] - 1) *
        exp(- (1/m) * (1 + params_boot["shape"] * ((L-params_boot["location"])/params_boot["scale"]))^(-1/params_boot["shape"]))
    }
    # Pokus o integraci
    integral_result <- try(integrate(integrand, lower = -VaR_boot_bmmM[i,j], upper = Inf), silent = TRUE)
    
    # Kontrola, zda integrace proběhla úspěšně
    if (inherits(integral_result, "try-error") || is.nan(integral_result$value)) {
      ES_boot_bmmM[i,j] <- NaN
    } else {
      ES_boot_bmmM[i,j] <- -(1 / (alpha[j] * m * params_boot["scale"])) * integral_result$value
    }
  }
  for (k in 1:3) {
    bmm_bootM[i,k] <- params_boot[k]
  }
}
# 95% interval spolehlivosti:
ci_var_bmmM<- data.frame(numeric(2), numeric(2), numeric(2))
ci_es_bmmM <- data.frame(numeric(2), numeric(2), numeric(2))
for (j in 1:length(alpha)) {
  ci_var_bmmM[j,] <- quantile(VaR_boot_bmmM[,j], probs = c(0.025,.5, 0.975), na.rm = TRUE)
  ci_es_bmmM[j,] <- quantile(ES_boot_bmmM[,j], probs = c(0.025,.5, 0.975), na.rm = TRUE)
}
ci_var_bmmM
ci_es_bmmM

# bias odhadnuty empiricky z dat pomoci bootstrapu:
biasM <- c(abs(mean(bmm_bootM[[1]])-loc), abs(mean(bmm_bootM[[2]])-scale), abs(mean(bmm_bootM[[3]])-shape))
biasM

# ------------------------------------------------------------------------------------------------------------------
# BLOK = 1 TYDEN
# ------------------------------------------------------------------------------------------------------------------
m_n <- apply.weekly(rpc,max)
m_n_index <- which(index(rpc) %in% index(rpc[rpc %in% m_n])) 

fit_con_gev <- fevd(m_n, type = "GEV")
summary(fit_con_gev)
ci(fit_con_gev, type = "parameter")

loc <- fit_con_gev$results$par[1]
scale <- fit_con_gev$results$par[2]
shape <- fit_con_gev$results$par[3]
#ulozeni parametru pro backtesting:
loc_bmmW_c <- loc
scale_bmmW_c <- scale
shape_bmmW_c <- shape

#udaje do tabulky:
is <- round(ci(fit_con_gev, type = "parameter")[1:3,1:3],5) # IS v pripade MLE pracuje s approx. normalitou
is_char <- c(paste0("(", is[,1], ";", is[,3], ")"))
df_par_est_c <- rbind(df_par_est_c, cbind(is,is_char))
colnames(df_par_est_c) <- c( "lower", "Bodový odhad", "upper", "Intervalový odhad")
rownames(df_par_est_c) <- rep(c("mu", "sigma", "xi"), 3)
sirka <- c("1 čtvrtletí", "1 měsíc", "1 týden")
xtable(data.frame('Šířka bloku' = cbind(rep(sirka,3)), df_par_est_c[c(1,4,7,2,5,8,3,6,9),c(2,4)]))

# ------------------------------------------------------------------------------------------------------------------
# DIAGNOSTIKA (BLOK = 1 TYDEN)
# ------------------------------------------------------------------------------------------------------------------
#ECDF a nafitovana teoreticka CDF
empirical_cdf <- ecdf(as.vector(m_n)) #ECDF
x_vals <- seq(-5,25, length.out = 10000)  # Body na ose x
theoretical_cdf <- pevd(x_vals, loc = loc, scale = scale, shape = shape, type="GEV")  # Teoretická CDF
plot(empirical_cdf, verticals = TRUE, do.points = FALSE, col=2, xlim=c(-5,20), main = "Konzervativní portfolio")
lines(x_vals, theoretical_cdf, type="l")

#PPPLOT:
empirical_cdf <- ecdf(as.vector(m_n))
empirical_probs <- empirical_cdf(as.vector(m_n))
theoretical_probs <- pevd(as.vector(m_n), loc = loc, scale = scale, shape = shape, type="GEV")  # Teoretická CDF
plot_data <- data.frame(empirical = empirical_probs,theoretical = theoretical_probs)
ppplot <- ggplot(plot_data, aes(x = theoretical, y = empirical)) +
  geom_point(color = "black") +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Teoretické pravděpodobnosti", y = "Empirické pravděpodobnosti", title = "P-P Plot - Konzervativní p." ) +
  theme_bw()

#QQPLOT:
qqplot <- ggplot(m_n, aes(sample = as.vector(m_n))) +
  stat_qq(distribution = extRemes::qevd, dparams = list(loc = loc, scale = scale,shape = shape,type = "GEV")) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Teoretické kvantily", y = "Empirické kvantily", title = "Q-Q Plot") +
  xlim(0,6) + ylim(0,6) +
  theme_bw()

# Histogram s hustotou odhadnuteho rozdeleni
hist <- ggplot(m_n, aes(x = as.vector(m_n))) +
  geom_histogram(mapping = aes(y = after_stat(density)),binwidth = .4,col = "gray30", fill = "dimgrey", alpha = .2,) +
  stat_function(fun = extRemes::devd, args = list(loc = loc, scale = scale, shape = shape, type = "GEV"), col = "black", size = .7) +
  labs(x = "x", y = "Hustota", title = "Histogram") +
  theme_bw()

# Return Level Plot
return_periods <- c(1.1, 2, 5, 10, 20, 50, 100, 500)  # Doby návratu
return_levels <- return.level(fit_con_gev, return.period = return_periods) 
ci <- ci(fit_con_gev, type = "return.level", return.period = return_periods) # Intervaly spolehlivosti pro návratové hladiny
plot_data <- data.frame(return_period = return_periods,  return_level = return_levels,  ci_lower = ci[, 1],  ci_upper = ci[, 3])
empirical_levels <- sort(as.vector(m_n), decreasing = TRUE)
empirical_periods <- (((length(rpc)) + 1) / index(empirical_levels))*length(m_n)/(7*n)
empirical_data <- data.frame(period = empirical_periods, level = empirical_levels)
rlplot <- ggplot(plot_data, aes(x = return_period, y = return_level)) +
  geom_line(color = "black", size = 1) +  # Teoretická křivka
  geom_point(data = empirical_data, aes(x = period, y = level), color = "black", size = 2) +  # Empirické body
  geom_ribbon(data = plot_data, aes(x= return_period, ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "darkgray") +  # Intervaly spolehlivosti
  scale_x_log10() +  # Logaritmická osa x
  labs(title = "Return Level Plot", x = "Návratová perioda (log)", y = "Návratová úroveň") +
  ylim(c(0,6))+
  theme_bw()

grid.arrange(
  ppplot, qqplot, rlplot, hist,
  nrow = 2, ncol = 2
)

# ------------------------------------------------------------------------------------------------------------------
# ODHAD VaR a ES
# ------------------------------------------------------------------------------------------------------------------
# uvedeno jiz opet jako zaporna hodnota, tzn vynosy = kladne, ztraty = zaporne
alpha <- c(.05,.025,.01,.005)
m <- (7*n)/length(m_n)
var_gev_con_w <- -((loc -(scale/shape)*(1-((-m*log(1-alpha))^(-shape)))))
es_gev_con_w <- sapply(1:length(alpha), function(i) compute_ES(alpha[i], var_gev_con_w[i]))
var_gev_con_w
es_gev_con_w

# intervalove odhady - bootstrap:
set.seed(123)
B <- 1000
VaR_boot_bmmW <- data.frame(numeric(B),numeric(B), numeric(B), numeric(B))
ES_boot_bmmW <- data.frame(numeric(B),numeric(B), numeric(B), numeric(B))
bmm_bootW <- data.frame(numeric(B),numeric(B), numeric(B), numeric(B))
# POZOR, bootstrap je vypocetne narocny, trva to dele!
for (i in 1:B) {
  resample <- sample(m_n, replace = TRUE)
  fit_boot <- fevd(resample, type = "GEV")
  params_boot <- fit_boot$results$par
  for (j in 1:length(alpha)) {
    VaR_boot_bmmW[i,j] <- -((params_boot["location"] -(params_boot["scale"]/params_boot["shape"])*(1-((-m*log(1-alpha[j]))^(-params_boot["shape"])))))
    integrand <- function(L) {
      L * (1 + params_boot["shape"] * ((L-params_boot["location"])/params_boot["scale"]))^(-1/params_boot["shape"] - 1) *
        exp(- (1/m) * (1 + params_boot["shape"] * ((L-params_boot["location"])/params_boot["scale"]))^(-1/params_boot["shape"]))
    }
    # Pokus o integraci
    integral_result <- try(integrate(integrand, lower = -VaR_boot_bmmW[i,j], upper = Inf), silent = TRUE)
    
    # Kontrola, zda integrace proběhla úspěšně
    if (inherits(integral_result, "try-error") || is.nan(integral_result$value)) {
      ES_boot_bmmW[i,j] <- NaN
    } else {
      ES_boot_bmmW[i,j] <- -(1 / (alpha[j] * m * params_boot["scale"])) * integral_result$value
    }
  }
  for (k in 1:3) {
    bmm_bootW[i,k] <- params_boot[k]
  }
}
# 95% interval spolehlivosti:
ci_var_bmmW_c<- data.frame(numeric(2), numeric(2), numeric(2))
ci_es_bmmW_c <- data.frame(numeric(2), numeric(2), numeric(2))
for (j in 1:length(alpha)) {
  ci_var_bmmW_c[j,] <- quantile(VaR_boot_bmmW[,j], probs = c(0.025,.5, 0.975), na.rm = TRUE)
  ci_es_bmmW_c[j,] <- quantile(ES_boot_bmmW[,j], probs = c(0.025,.5, 0.975), na.rm = TRUE)
}
ci_var_bmmW_c
ci_es_bmmW_c

# bias odhadnuty empiricky z dat pomoci bootstrapu:
biasW <- c(abs(mean(bmm_bootW[[1]])-loc), abs(mean(bmm_bootW[[2]])-scale), abs(mean(bmm_bootW[[3]])-shape))
biasW

# ------------------------------------------------------------------------------------------------------------------
# BACKTESTING - TESTOVACI DATA
# ------------------------------------------------------------------------------------------------------------------
# viz samostatny soubor Tomancova_backtesting.R



# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# BMM PRO DYNAMICKE PORTFOLIO
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# BLOK = 1 KVARTAL
# ------------------------------------------------------------------------------------------------------------------
m_n <- apply.quarterly(rpd,max)
m_n_index <- which(index(rpd) %in% index(rpd[rpd %in% m_n])) 

fit_con_gev <- fevd(m_n, type = "GEV")
summary(fit_con_gev)
summary(fevd(m_n, type="GEV", method="Lmoments")) # kontrola robustnosti mle odhadu
ci(fit_con_gev, type = "parameter") # IS s kvantilem st. norm. rozd.
ci(fit_con_gev, type = "parameter", method = "boot") # IS metodou bootstrap
loc <- fit_con_gev$results$par[1]
scale <- fit_con_gev$results$par[2]
shape <- fit_con_gev$results$par[3]
#ulozeni parametru pro backtesting:
loc_bmmQ_d <- loc
scale_bmmQ_d <- scale
shape_bmmQ_d <- shape

#udaje do tabulky:
is <- round(ci(fit_con_gev, type = "parameter")[1:3,1:3],5) 
is_char <- c(paste0("(", is[,1], ";", is[,3], ")"))
df_par_est_d <- data.frame(matrix(nrow=3, ncol=4))
df_par_est_d <- cbind(is,is_char)

# ------------------------------------------------------------------------------------------------------------------
# DIAGNOSTIKA (BLOK = 1 KVARTAL)
# ------------------------------------------------------------------------------------------------------------------
#ECDF a nafitovana teoreticka CDF
empirical_cdf <- ecdf(as.vector(m_n)) #ECDF
x_vals <- seq(-5,55, length.out = 10000)  # Body na ose x
theoretical_cdf <- pevd(x_vals, loc = loc, scale = scale, shape = shape, type="GEV")  # Teoretická CDF
plot(empirical_cdf, verticals = TRUE, do.points = FALSE, col=2, xlim=c(-5,50), main = "Dynamické portfolio")
lines(x_vals, theoretical_cdf, type="l")

#PPPLOT:
empirical_cdf <- ecdf(as.vector(m_n))
empirical_probs <- empirical_cdf(as.vector(m_n))
theoretical_probs <- pevd(as.vector(m_n), loc = loc, scale = scale, shape = shape, type="GEV")  # Teoretická CDF
plot_data <- data.frame(empirical = empirical_probs, theoretical = theoretical_probs)
ppplot <- ggplot(plot_data, aes(x = theoretical, y = empirical)) +
  geom_point(color = "black") +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Teoretické pravděpodobnosti", y = "Empirické pravděpodobnosti", title = "P-P Plot - Dynamické p." ) +
  theme_bw()


#QQPLOT:
qqplot <- ggplot(m_n, aes(sample = as.vector(m_n))) +
  stat_qq(distribution = extRemes::qevd,dparams = list(loc = loc, scale = scale,shape = shape,type = "GEV")) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Teoretické kvantily", y = "Empirické kvantily", title = "Q-Q Plot") +
  xlim(0,20) +  ylim(0,20) +
  theme_bw()

# Histogram s hustotou odhadnuteho rozdeleni
hist <- ggplot(m_n, aes(x = as.vector(m_n))) +
  geom_histogram(mapping = aes(y = after_stat(density)),binwidth = 1,col = "gray30",fill = "dimgrey",alpha = .2,) +
  stat_function(fun = extRemes::devd,  args = list(loc = loc, scale = scale, shape = shape, type = "GEV"), col = "black", size = .7) +
  labs(x = "x", y = "Hustota", title = "Histogram") +
  theme_bw()

# Return Level Plot
return_periods <- c(1.1, 2, 5, 10, 20, 50, 100, 500)  # Doby návratu
return_levels <- return.level(fit_con_gev, return.period = return_periods) 
ci <- ci(fit_con_gev, type = "return.level", return.period = return_periods) # Intervaly spolehlivosti pro návratové hladiny
plot_data <- data.frame(return_period = return_periods,  return_level = return_levels,  ci_lower = ci[, 1],  ci_upper = ci[, 3])
empirical_levels <- sort(as.vector(m_n), decreasing = TRUE)
empirical_periods <- (((length(rpd)) + 1) / index(empirical_levels))*4/n
empirical_data <- data.frame(period = empirical_periods, level = empirical_levels)
rlplot <- ggplot(data = plot_data, aes(x = return_period, y = return_level)) +
  geom_line(color = "black", size = 1) +  # Teoretická křivka
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
# ODHAD VaR a ES 
# ------------------------------------------------------------------------------------------------------------------
# uvedeno jiz opet jako zaporna hodnota, tzn vynosy = kladne, ztraty = zaporne
alpha <- c(.05,.025,.01, .005)
m <- n/4 # velikost jednoho bloku
var_gev_con_q <- -((loc -(scale/shape)*(1-((-m*log(1-alpha))^(-shape)))))
es_gev_con_q <- sapply(1:length(alpha), function(i) compute_ES(alpha[i], var_gev_con_q[i]))
var_gev_con_q
es_gev_con_q 

# intervalove odhady - bootstrap:
set.seed(123)
B <- 1000
VaR_boot_bmmQ <- data.frame(numeric(B),numeric(B), numeric(B), numeric(B))
ES_boot_bmmQ <- data.frame(numeric(B),numeric(B), numeric(B), numeric(B))
bmm_bootQ <- data.frame(numeric(B),numeric(B), numeric(B), numeric(B))
# POZOR, bootstrap je vypocetne narocny, trva to dele!
for (i in 1:B) {
  resample <- sample(m_n, replace = TRUE)
  fit_boot <- fevd(resample, type = "GEV")
  params_boot <- fit_boot$results$par
  for (j in 1:length(alpha)) {
    VaR_boot_bmmQ[i,j] <- -((params_boot["location"] -(params_boot["scale"]/params_boot["shape"])*(1-((-m*log(1-alpha[j]))^(-params_boot["shape"])))))
    integrand <- function(L) {
      L * (1 + params_boot["shape"] * ((L-params_boot["location"])/params_boot["scale"]))^(-1/params_boot["shape"] - 1) *
        exp(- (1/m) * (1 + params_boot["shape"] * ((L-params_boot["location"])/params_boot["scale"]))^(-1/params_boot["shape"]))
    }
    # Pokus o integraci
    integral_result <- try(integrate(integrand, lower = -VaR_boot_bmmQ[i,j], upper = Inf), silent = TRUE)
    
    # Kontrola, zda integrace proběhla úspěšně
    if (inherits(integral_result, "try-error") || is.nan(integral_result$value)) {
      ES_boot_bmmQ[i,j] <- NaN
    } else {
      ES_boot_bmmQ[i,j] <- -(1 / (alpha[j] * m * params_boot["scale"])) * integral_result$value
    }
  }
  for (k in 1:3) {
    bmm_bootQ[i,k] <- params_boot[k]
  }
}
# 95% interval spolehlivosti:
ci_var_bmmQ_d<- data.frame(numeric(2), numeric(2), numeric(2))
ci_es_bmmQ_d <- data.frame(numeric(2), numeric(2), numeric(2))
for (j in 1:length(alpha)) {
  ci_var_bmmQ_d[j,] <- quantile(VaR_boot_bmmQ[,j], probs = c(0.025,.5, 0.975), na.rm = TRUE)
  ci_es_bmmQ_d[j,] <- quantile(ES_boot_bmmQ[,j], probs = c(0.025,.5, 0.975), na.rm = TRUE)
}
ci_var_bmmQ_d
ci_es_bmmQ_d

# bias odhadnuty empiricky z dat pomoci bootstrapu:
biasQ <- c(abs(mean(bmm_bootQ[[1]])-loc), abs(mean(bmm_bootQ[[2]])-scale), abs(mean(bmm_bootQ[[3]])-shape))
biasQ

# ------------------------------------------------------------------------------------------------------------------
# BLOK = 1 MESIC
# ------------------------------------------------------------------------------------------------------------------
m_n <- apply.monthly(rpd,max)
m_n_index <- which(index(rpd) %in% index(rpd[rpd %in% m_n])) 

fit_con_gev <- fevd(m_n, type = "GEV")
summary(fit_con_gev)
ci(fit_con_gev, type = "parameter") # IS s kvantilem st. norm. rozd.
ci(fit_con_gev, type = "parameter", method = "boot") # IS metodou bootstrap
loc <- fit_con_gev$results$par[1]
scale <- fit_con_gev$results$par[2]
shape <- fit_con_gev$results$par[3]
#ulozeni parametru pro backtesting:
loc_bmmM_d <- loc
scale_bmmM_d <- scale
shape_bmmM_d <- shape

#udaje do tabulky:
is <- round(ci(fit_con_gev, type = "parameter")[1:3,1:3],5) # IS v pripade MLE pracuje s approx. normalitou
is_char <- c(paste0("(", is[,1], ";", is[,3], ")"))
df_par_est_d <- rbind(df_par_est_d, cbind(is,is_char))

# ------------------------------------------------------------------------------------------------------------------
# DIAGNOSTIKA (BLOK = 1 MESIC)
# ------------------------------------------------------------------------------------------------------------------
#ECDF a nafitovana teoreticka CDF
empirical_cdf <- ecdf(as.vector(m_n)) #ECDF
x_vals <- seq(-5,55, length.out = 10000)  # Body na ose x
theoretical_cdf <- pevd(x_vals, loc = loc, scale = scale, shape = shape, type="GEV")  # Teoretická CDF
plot(empirical_cdf, verticals = TRUE, do.points = FALSE, col=2, xlim=c(-5,50), main = "Dynamické portfolio")
lines(x_vals, theoretical_cdf, type="l")

#PPPLOT:
empirical_cdf <- ecdf(as.vector(m_n))
empirical_probs <- empirical_cdf(as.vector(m_n))
theoretical_probs <- pevd(as.vector(m_n), loc = loc, scale = scale, shape = shape, type="GEV")  # Teoretická CDF
plot_data <- data.frame(empirical = empirical_probs,theoretical = theoretical_probs)
ppplot <- ggplot(plot_data, aes(x = theoretical, y = empirical)) +
  geom_point(color = "black") +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "P-P Plot - Dynamické p.", x = "Teoretické pravděpodobnosti",y = "Empirické pravděpodobnosti") +
  theme_bw()

#QQPLOT:
qqplot <- ggplot(m_n, aes(sample = as.vector(m_n))) +
  stat_qq(distribution = extRemes::qevd,dparams = list(loc = loc, scale = scale,shape = shape,type = "GEV")) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Teoretické kvantily", y = "Empirické kvantily", title = "Q-Q Plot") +
  xlim(0,20) + ylim(0,20) +
  theme_bw()

# Histogram s hustotou odhadnuteho rozdeleni
hist <- ggplot(m_n, aes(x = as.vector(m_n))) +
  geom_histogram(mapping = aes(y = after_stat(density)),binwidth = 1, col = "gray30",fill = "dimgrey",alpha = .2,) +
  stat_function(fun = extRemes::devd, args = list(loc = loc, scale = scale, shape = shape, type = "GEV" ), col = "black", size = .7) +
  labs(x = "x", y = "Hustota", title = "Histogram") +
  theme_bw()

# Return Level Plot
return_periods <- c(1.1, 2, 5, 10, 20, 50, 100, 500)  # Doby návratu
return_levels <- return.level(fit_con_gev, return.period = return_periods) 
ci <- ci(fit_con_gev, type = "return.level", return.period = return_periods) # Intervaly spolehlivosti pro návratové hladiny
plot_data <- data.frame(return_period = return_periods,  return_level = return_levels,  ci_lower = ci[, 1],  ci_upper = ci[, 3])
empirical_levels <- sort(as.vector(m_n), decreasing = TRUE)
empirical_periods <- (((length(rpd)) + 1) / index(empirical_levels))*12/n
empirical_data <- data.frame(period = empirical_periods, level = empirical_levels)
rlplot <- ggplot(data = plot_data, aes(x = return_period, y = return_level)) +
  geom_line(color = "black", size = 1) +  # Teoretická křivka
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
# ODHAD VaR a ES
# ------------------------------------------------------------------------------------------------------------------
# uvedeno jiz opet jako zaporna hodnota, tzn vynosy = kladne, ztraty = zaporne
alpha <- c(.05,.025,.01,.005)
m <- n/12 # velikost jednoho bloku
var_gev_con_m <- -((loc -(scale/shape)*(1-((-m*log(1-alpha))^(-shape)))))
es_gev_con_m <- sapply(1:length(alpha), function(i) compute_ES(alpha[i], var_gev_con_m[i]))
var_gev_con_m
es_gev_con_m

# intervalove odhady - bootstrap:
set.seed(123)
B <- 1000
VaR_boot_bmmM <- data.frame(numeric(B),numeric(B), numeric(B), numeric(B))
ES_boot_bmmM <- data.frame(numeric(B),numeric(B), numeric(B), numeric(B))
bmm_bootM <- data.frame(numeric(B),numeric(B), numeric(B), numeric(B))
# POZOR, bootstrap je vypocetne narocny, trva to dele!
for (i in 1:B) {
  resample <- sample(m_n, replace = TRUE)
  fit_boot <- fevd(resample, type = "GEV")
  params_boot <- fit_boot$results$par
  for (j in 1:length(alpha)) {
    VaR_boot_bmmM[i,j] <- -((params_boot["location"] -(params_boot["scale"]/params_boot["shape"])*(1-((-m*log(1-alpha[j]))^(-params_boot["shape"])))))
    integrand <- function(L) {
      L * (1 + params_boot["shape"] * ((L-params_boot["location"])/params_boot["scale"]))^(-1/params_boot["shape"] - 1) *
        exp(- (1/m) * (1 + params_boot["shape"] * ((L-params_boot["location"])/params_boot["scale"]))^(-1/params_boot["shape"]))
    }
    # Pokus o integraci
    integral_result <- try(integrate(integrand, lower = -VaR_boot_bmmM[i,j], upper = Inf), silent = TRUE)
    
    # Kontrola, zda integrace proběhla úspěšně
    if (inherits(integral_result, "try-error") || is.nan(integral_result$value)) {
      ES_boot_bmmM[i,j] <- NaN
    } else {
      ES_boot_bmmM[i,j] <- -(1 / (alpha[j] * m * params_boot["scale"])) * integral_result$value
    }
  }
  for (k in 1:3) {
    bmm_bootM[i,k] <- params_boot[k]
  }
}
# 95% interval spolehlivosti:
ci_var_bmmM_d<- data.frame(numeric(2), numeric(2), numeric(2))
ci_es_bmmM_d <- data.frame(numeric(2), numeric(2), numeric(2))
for (j in 1:length(alpha)) {
  ci_var_bmmM_d[j,] <- quantile(VaR_boot_bmmM[,j], probs = c(0.025,.5, 0.975), na.rm = TRUE)
  ci_es_bmmM_d[j,] <- quantile(ES_boot_bmmM[,j], probs = c(0.025,.5, 0.975), na.rm = TRUE)
}
ci_var_bmmM_d
ci_es_bmmM_d

# bias odhadnuty empiricky z dat pomoci bootstrapu:
biasM <- c(abs(mean(bmm_bootM[[1]])-loc), abs(mean(bmm_bootM[[2]])-scale), abs(mean(bmm_bootM[[3]])-shape))
biasM


# ------------------------------------------------------------------------------------------------------------------
# BLOK = 1 TYDEN
# ------------------------------------------------------------------------------------------------------------------
m_n <- apply.weekly(rpd,max)
summary(m_n)

fit_con_gev <- fevd(m_n, type = "GEV")
summary(fit_con_gev)
ci(fit_con_gev, type = "parameter") # IS s kvantilem st. norm. rozd.

loc <- fit_con_gev$results$par[1]
scale <- fit_con_gev$results$par[2]
shape <- fit_con_gev$results$par[3]
#ulozeni parametru pro backtesting:
loc_bmmW_d <- loc
scale_bmmW_d <- scale
shape_bmmW_d <- shape

#udaje do tabulky:
is <- round(ci(fit_con_gev, type = "parameter")[1:3,1:3],5) 
is_char <- c(paste0("(", is[,1], ";", is[,3], ")"))
df_par_est_d <- rbind(df_par_est_d, cbind(is,is_char))
colnames(df_par_est_d) <- c( "lower", "Bodový odhad", "upper", "Intervalový odhad")
rownames(df_par_est_d) <- rep(c("mu", "sigma", "xi"), 3)
xtable(data.frame('Šířka bloku' = cbind(rep(sirka,3)), df_par_est_d[c(1,4,7,2,5,8,3,6,9),c(2,4)]))
# ------------------------------------------------------------------------------------------------------------------
# DIAGNOSTIKA (BLOK = 1 TÝDEN)
# ------------------------------------------------------------------------------------------------------------------
#ECDF a nafitovana teoreticka CDF
empirical_cdf <- ecdf(as.vector(m_n)) #ECDF
x_vals <- seq(-5,55, length.out = 10000)  # Body na ose x
theoretical_cdf <- pevd(x_vals, loc = loc, scale = scale, shape = shape, type="GEV")  # Teoretická CDF
plot(empirical_cdf, verticals = TRUE, do.points = FALSE, col=2, xlim=c(-5,50), main = "Dynamické portfolio")
lines(x_vals, theoretical_cdf, type="l")

#PPPLOT:
empirical_cdf <- ecdf(as.vector(m_n))
empirical_probs <- empirical_cdf(as.vector(m_n))
theoretical_probs <- pevd(as.vector(m_n), loc = loc, scale = scale, shape = shape, type="GEV")  # Teoretická CDF
plot_data <- data.frame(empirical = empirical_probs,theoretical = theoretical_probs)
ppplot <- ggplot(plot_data, aes(x = theoretical, y = empirical)) +
  geom_point(color = "black") +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Teoretické pravděpobnosti", y = "Empirické pravděpodobnosti", title = "P-P Plot - Dynamické p." ) +
  theme_bw()


#QQPLOT:
qqplot <- ggplot(m_n, aes(sample = as.vector(m_n))) +
  stat_qq(distribution = extRemes::qevd,dparams = list(loc = loc, scale = scale,shape = shape,type = "GEV")) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Teoretické kvantily", y = "Empirické kvantily", title = "Q-Q Plot") +
  xlim(0,20) + ylim(0,20) +
  theme_bw()

# Histogram s hustotou odhadnuteho rozdeleni
hist <- ggplot(m_n, aes(x = as.vector(m_n))) +
  geom_histogram(mapping = aes(y = after_stat(density)), binwidth = 1, col = "gray30", fill = "dimgrey", alpha = .2,) +
  stat_function(fun = extRemes::devd, args = list(loc = loc, scale = scale, shape = shape, type = "GEV"), col = "black", size = .7) +
  labs(x = "x", y = "Hustota", title = "Histogram") +
  theme_bw()

# Return Level Plot
return_periods <- c(1.1, 2, 5, 10, 20, 50, 100, 500)  # Doby návratu
return_levels <- return.level(fit_con_gev, return.period = return_periods) 
ci <- ci(fit_con_gev, type = "return.level", return.period = return_periods) # Intervaly spolehlivosti pro návratové hladiny
plot_data <- data.frame(return_period = return_periods,  return_level = return_levels,  ci_lower = ci[, 1],  ci_upper = ci[, 3])
empirical_levels <- sort(as.vector(m_n), decreasing = TRUE)
empirical_periods <- ((((length(rpd)) + 1) / index(empirical_levels))*length(m_n))/(n*7)
empirical_data <- data.frame(period = empirical_periods, level = empirical_levels)
rlplot <- ggplot(data = plot_data, aes(x = return_period, y = return_level)) +
  geom_line(color = "black", size = 1) +  # Teoretická křivka
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
# ODHAD VaR a ES
# ------------------------------------------------------------------------------------------------------------------
# uvedeno jiz opet jako zaporna hodnota, tzn vynosy = kladne, ztraty = zaporne
alpha <- c(.05,.025,.01,.005)
m <- 7*n/length(m_n)
var_gev_con_w <- -((loc -(scale/shape)*(1-((-m*log(1-alpha))^(-shape)))))
es_gev_con_w <- sapply(1:length(alpha), function(i) compute_ES(alpha[i], var_gev_con_w[i]))
var_gev_con_w
es_gev_con_w

# intervalove odhady - bootstrap:
set.seed(123)
B <- 1000
VaR_boot_bmmW <- data.frame(numeric(B),numeric(B), numeric(B), numeric(B))
ES_boot_bmmW <- data.frame(numeric(B),numeric(B), numeric(B), numeric(B))
bmm_bootW <- data.frame(numeric(B),numeric(B), numeric(B), numeric(B))
# POZOR, bootstrap je vypocetne narocny, trva to dele!
for (i in 1:B) {
  resample <- sample(m_n, replace = TRUE)
  fit_boot <- fevd(resample, type = "GEV")
  params_boot <- fit_boot$results$par
  for (j in 1:length(alpha)) {
    VaR_boot_bmmW[i,j] <- -((params_boot["location"] -(params_boot["scale"]/params_boot["shape"])*(1-((-m*log(1-alpha[j]))^(-params_boot["shape"])))))
    integrand <- function(L) {
      L * (1 + params_boot["shape"] * ((L-params_boot["location"])/params_boot["scale"]))^(-1/params_boot["shape"] - 1) *
        exp(- (1/m) * (1 + params_boot["shape"] * ((L-params_boot["location"])/params_boot["scale"]))^(-1/params_boot["shape"]))
    }
    # Pokus o integraci
    integral_result <- try(integrate(integrand, lower = -VaR_boot_bmmW[i,j], upper = Inf), silent = TRUE)
    
    # Kontrola, zda integrace proběhla úspěšně
    if (inherits(integral_result, "try-error") || is.nan(integral_result$value)) {
      ES_boot_bmmW[i,j] <- NaN
    } else {
      ES_boot_bmmW[i,j] <- -(1 / (alpha[j] * m * params_boot["scale"])) * integral_result$value
    }
  }
  for (k in 1:3) {
    bmm_bootW[i,k] <- params_boot[k]
  }
}
# 95% interval spolehlivosti:
ci_var_bmmW_d<- data.frame(numeric(2), numeric(2), numeric(2))
ci_es_bmmW_d <- data.frame(numeric(2), numeric(2), numeric(2))
for (j in 1:length(alpha)) {
  ci_var_bmmW_d[j,] <- quantile(VaR_boot_bmmW[,j], probs = c(0.025,.5, 0.975), na.rm = TRUE)
  ci_es_bmmW_d[j,] <- quantile(ES_boot_bmmW[,j], probs = c(0.025,.5, 0.975), na.rm = TRUE)
}
ci_var_bmmW_d
ci_es_bmmW_d

# bias odhadnuty empiricky z dat pomoci bootstrapu:
biasW <- c(abs(mean(bmm_bootW[[1]])-loc), abs(mean(bmm_bootW[[2]])-scale), abs(mean(bmm_bootW[[3]])-shape))
biasW


# ------------------------------------------------------------------------------------------------------------------
# BACKTESTING - TESTOVACI DATA
# ------------------------------------------------------------------------------------------------------------------
# viz samostatny soubor Tomancova_backtesting.R


# ------------------------------------------------------------------------------------------------------------------
# KONEC SCRIPTU PRO METODU BLOKOVYCH MAXIM
# ------------------------------------------------------------------------------------------------------------------
