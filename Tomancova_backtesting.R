# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# RSCRIPT PRO APLIKACNI CAST PRACE - ZPETNE TESTOVANI
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------------------------
# nacteni pckgs
# ------------------------------------------------------------------------------------------------------------------
library(xtable)
library(GAS)
library(dplyr)
# ------------------------------------------------------------------------------------------------------------------
# KUPIECUV POF TEST - KONZERVATIVNI PORTFOLIO
# ------------------------------------------------------------------------------------------------------------------
# --- VSTUPNÍ DATA ---
# Vektor skutečných výnosů na testovacím vzorku 
actual_returns <- rpcon_test 

# Data frame: sloupce jsou odhady VaR pro každý model a hladinu
VaR_matrix <- t(df_c_var[,c(3)])
colnames(VaR_matrix) <- df_c_var[,1]
VaR_matrix
# --- PŘEDPŘIPRAVENÝ SEZNAM ALFA HLADIN ---
alpha_levels <- c(0.05, 0.025, 0.01, 0.005)

# --- FUNKCE PRO TEST ---
kupiec_test <- function(actual, var_series, alpha) {
  # vytvoří binární vektor porušení
  exceedances <- ifelse(actual < var_series, 1, 0)
  x <- sum(exceedances)
  n <- length(actual)
  pi_hat <- x / n
  pi_0 <- alpha
  
  # Log-likelihood ratio
  LR_uc <- -2 * (log((1 - pi_0)^(n - x) * pi_0^x) -
                   log((1 - pi_hat)^(n - x) * pi_hat^x))
  p_value <- (1-pchisq(LR_uc, df = 1))
  
  return(data.frame(
    exceedances = x,
    LR_uc = LR_uc,
    p_value = p_value
  ))
}

# --- HLAVNÍ LOOP PŘES MODELY ---
results <- data.frame()
i=0
for (col in colnames(VaR_matrix)) {
  # Zjistíme hladinu VaR ze jména sloupce (např. hist_99 -> 0.01)
  alpha_str <- gsub(".*_(\\d+)$", "\\1", col)
  i=i+1
  if (nchar(alpha_str)==2) {
  alpha_num <- 1- (as.numeric(alpha_str) / 100)
  }
  else if (nchar(alpha_str)==3) {
    alpha_num <- 1-(as.numeric(alpha_str) / 1000)
  }
  if (!(round(alpha_num, 2) %in% round(alpha_levels,2))) next  # kontrola
  
  test_result <- kupiec_test(actual_returns, VaR_matrix[i], alpha_num)
  results <- rbind(results, data.frame(model = col, VaR = round(VaR_matrix[i],5), alpha = alpha_num, 
                                       exceedances = test_result[1], LR = round(test_result[2],5), p_value = round(test_result[3],5), vysl_pof = test_result[3]>.05))
}

# --- VÝSLEDEK ---
print(results[results[,7]==TRUE,])

vysl_kup <- cbind(results, Odhad = rep(c("95% VaR","97,5% VaR","99% VaR","99,5% VaR"),9), 
                  Metoda = c(rep("HS",4), rep("Var.-Covar.",4), rep("GP pro $u_1$",4), rep("Decl. GP pro $u_1$",4),
                  rep("GP pro $u_2$",4), rep("Decl. GP pro $u_2$",4), rep("GEV čtvrtletní",4), rep("GEV měsíční",4), rep("GEV týdenní",4)))
vysl_kup <- data.frame(vysl_kup$Odhad, vysl_kup$Metoda, vysl_kup$VaR, vysl_kup$exceedances, vysl_kup$LR_uc, vysl_kup$p_value)
colnames(vysl_kup)<- c("Odhad", "Metoda", "hat{VaR}", "Počet překročení", "Testová statistika", "p-hodnota")
vysl_kup2 <- vysl_kup[c(1,5,9,13,17,21,25,29,33,2,6,10,14,18,22,26,30,34,3,7,11,15,19,23,27,31,35,4,8,12,16,20,24,28,32,36),]
vysl_kup2

xtable(vysl_kup2, caption = "Výsledky Kupiecova testu pokrytí. Tučným písmem jsou vyznačeny p-hodnoty vyšší než hladina významnosti $5%$.",
       label = "back_kup_tab", digits = c(0,0,0,5,0,5,5))


# ------------------------------------------------------------------------------------------------------------------
# KUPIECUV POF TEST - DYNAMICKE PORTFOLIO
# ------------------------------------------------------------------------------------------------------------------
# --- VSTUPNÍ DATA ---
# Vektor skutečných výnosů na testovacím vzorku 
actual_returns <- rpdyn_test 

# Data frame: sloupce jsou odhady VaR pro každý model a hladinu
VaR_matrix <- t(df_d_var[,c(3)])
colnames(VaR_matrix) <- df_d_var[,1]
VaR_matrix

# --- HLAVNÍ LOOP PŘES MODELY ---
results <- data.frame()
i=0
for (col in colnames(VaR_matrix)) {
  # Zjistíme hladinu VaR ze jména sloupce (např. hist_99 -> 0.01)
  alpha_str <- gsub(".*_(\\d+)$", "\\1", col)
  i=i+1
  if (nchar(alpha_str)==2) {
    alpha_num <- 1- (as.numeric(alpha_str) / 100)
  }
  else if (nchar(alpha_str)==3) {
    alpha_num <- 1-(as.numeric(alpha_str) / 1000)
  }
  if (!(round(alpha_num, 2) %in% round(alpha_levels,2))) next  # kontrola
  
  test_result <- kupiec_test(actual_returns, VaR_matrix[i], alpha_num)
  results <- rbind(results, data.frame(model = col, VaR = VaR_matrix[i], alpha = alpha_num, 
                                       exceedances = test_result[1], LR = test_result[2], p_value = test_result[3], vysl_pof = test_result[3]>.05))
}

# --- VÝSLEDEK ---
print(results[results[,7]==TRUE,])

vysl_kup_d <- cbind(results, Odhad = rep(c("95% VaR","97,5% VaR","99% VaR","99,5% VaR"),9), 
                  Metoda = c(rep("HS",4), rep("Var.-Kovar.",4), rep("GP pro $u_1$",4), rep("Decl. GP pro $u_1$",4),
                             rep("GP pro $u_2$",4), rep("Decl. GP pro $u_2$",4), rep("GEV čtvrtletní",4), rep("GEV měsíční",4), rep("GEV týdenní",4)))
vysl_kup_d <- data.frame(vysl_kup_d$Odhad, vysl_kup_d$Metoda, vysl_kup_d$VaR, vysl_kup_d$exceedances, vysl_kup_d$LR_uc, round(vysl_kup_d$p_value,5))
colnames(vysl_kup_d)<- c("Odhad", "Metoda", "$hat{VaR}$", "Překročení", "Test. statistika", "p-hodnota")
vysl_kup_d2 <- vysl_kup_d[c(1,5,9,13,17,21,25,29,33,2,6,10,14,18,22,26,30,34,3,7,11,15,19,23,27,31,35,4,8,12,16,20,24,28,32,36),]
vysl_kup_d2

xtable(vysl_kup_d2, caption = "Výsledky Kupiecova testu pokrytí pro dynamické portfolio.",
       label = "back_kup_d_tab", digits = c(0,0,0,5,0,5,5))

# ------------------------------------------------------------------------------------------------------------------
# C.- CURRAN TEST PRO ES - KONZERVATIVNI PORTFOLIO
# ------------------------------------------------------------------------------------------------------------------
alpha_levels <- c(0.05, 0.025, 0.01, 0.005)
actual_losses <- rpc_test 
T_ <- length(actual_returns)
past_losses <- rpc

loc_bmmQ <- loc_bmmQ_c
loc_bmmM <- loc_bmmM_c
loc_bmmW <- loc_bmmW_c
scale_bmmQ <- scale_bmmQ_c
scale_bmmM <- scale_bmmM_c
scale_bmmW <- scale_bmmW_c
shape_bmmQ <- shape_bmmQ_c
shape_bmmM <- shape_bmmM_c
shape_bmmW <- shape_bmmW_c

u1 <- u1c
u2 <- u2c
scale_pot1 <- scale_pot1_c
scale_potD1 <- scale_potD1_c
scale_pot2 <- scale_pot2_c
scale_potD2 <- scale_potD2_c
shape_pot1 <- shape_pot1_c
shape_potD1 <- shape_potD1_c
shape_pot2 <- shape_pot2_c
shape_potD2 <- shape_potD2_c
rate_pot1 <- rate_pot1_c
rate_potD1 <- rate_potD1_c
rate_pot2 <- rate_pot2_c
rate_potD2 <- rate_potD2_c

VaR_matrix <- - t(df_c_var[,c(3)])
colnames(VaR_matrix) <- df_c_var[,1]
VaR_matrix

ES_matrix <- t(df_c_es[,c(3)])
colnames(ES_matrix) <- df_c_es[,1]
ES_matrix

# funkce pro vypocet H_T
cc_test_fun <- function(actual, past, var_series, alpha, model) {
  # vytvoří binární vektor porušení
  exceedances <- ifelse(actual > var_series, 1, 0)
  x <- sum(exceedances)
  if (substr(model, 5,7) == "his") {
    G_emp <- ecdf(as.vector(past))
    G_Y_value <- G_emp(as.vector(actual))
  }
  else if (substr(model, 5,7) == "par") {
    G_Y_value <- pnorm(actual, mu_loss, sigma_loss)
  }
  else if (substr(model, 5,7) == "bmm") {
    if (substr(model, 8,8) == "Q") {
      n = 365/4
      loc = loc_bmmQ
      scale = scale_bmmQ
      shape = shape_bmmQ
    }
    else if (substr(model, 8,8) == "M") {
      n = 365/12
      loc = loc_bmmM
      scale = scale_bmmM
      shape = shape_bmmM
    }
    else if (substr(model, 8,8) == "W") {
      n=7
      loc = loc_bmmW
      scale = scale_bmmW
      shape = shape_bmmW
    }
    G_Y_value <- pevd(actual, loc = loc, scale = scale, shape = shape, type = "GEV")^(1/n)
  }
  else if (substr(model, 5,7) == "pot") {
    if (substr(model, 8,8) == "_") {
      u = u1
      scale = scale_pot1
      shape = shape_pot1
      rate = rate_pot1
    }
    else if (substr(model, 8,8) == "2") {
      u = u2
      scale = scale_pot2
      shape = shape_pot2
      rate = rate_pot2
    }
    else if (substr(model, 8,9) == "D_") {
      u = u1
      scale = scale_potD1
      shape = shape_potD1
      rate = rate_potD1
    }
    else if (substr(model, 8,9) == "D2") {
      u = u2
      scale = scale_potD2
      shape = shape_potD2
      rate = rate_potD2
    }
    G_Y_value <- 1-rate*((1+((shape*(actual-u))/scale))^(-1/shape))
  }
  df <- data.frame(exceedances, G_Y_value)
  colnames(df) <- c("exceedances", "G_Y_value")
  df <- df[df$exceedances==1,]
  zavorka <- na.omit(df$G_Y_value) + alpha -1
  H_T <- (1/(alpha*T_))*sum(zavorka)
  return(H_T)
}

# --- HLAVNÍ LOOP PŘES MODELY ---
results <- data.frame()
i=0
for (col in colnames(VaR_matrix)) {
  # Zjistíme hladinu VaR ze jména sloupce (např. hist_99 -> 0.01)
  alpha_str <- gsub(".*_(\\d+)$", "\\1", col)
  i=i+1
  if (nchar(alpha_str)==2) {
    alpha_num <- 1- (as.numeric(alpha_str) / 100)
  }
  else if (nchar(alpha_str)==3) {
    alpha_num <- 1-(as.numeric(alpha_str) / 1000)
  }
  if (!(round(alpha_num, 2) %in% round(alpha_levels,2))) next  # kontrola
  
  test_result <- cc_test_fun(actual_losses, past_losses, VaR_matrix[i], alpha_num, col)
  Z <- sqrt(3*T_)*(((2*test_result)-alpha_num)/(sqrt(alpha_num*(4-(3*alpha_num)))))
  p_val <- 2 * (1 - pnorm(abs(Z)))
  results <- rbind(results, data.frame(model = col, VaR = round(VaR_matrix[i],5), ES= round(ES_matrix[i],5), alpha = alpha_num, H_T = round(test_result,5), 
                                       Z = round(Z,5), p_value = round(p_val,5), vysl_curran = p_val>.05))
}

# --- VÝSLEDEK ---
print(results[results[,8]==TRUE,])

vysl_curr_c <- cbind(results, Odhad = rep(c("95% ES","97,5% ES","99% ES","99,5% ES"),9), 
                    Metoda = c(rep("HS",4), rep("Var.-Kovar.",4), rep("GP pro $u_1$",4), rep("Decl. GP pro $u_1$",4),
                               rep("GP pro $u_2$",4), rep("Decl. GP pro $u_2$",4), rep("GEV čtvrtletní",4), rep("GEV měsíční",4), rep("GEV týdenní",4)))
vysl_curr_c <- data.frame(vysl_curr_c$Odhad, vysl_curr_c$Metoda, vysl_curr_c$ES, vysl_curr_c$Z, vysl_curr_c$p_value)
colnames(vysl_curr_c)<- c("Odhad", "Metoda", "$hat{ES}$", "Test. statistika", "p-hodnota")
vysl_curr_c[c(1,5,9,13,17,21,25,29,33,2,6,10,14,18,22,26,30,34,3,7,11,15,19,23,27,31,35,4,8,12,16,20,24,28,32,36),]

xtable(vysl_curr_c, caption = "Výsledky Costanzino-Curranova testu pro konzervativní portfolio.",
       label = "back_curr_c_tab", digits = c(0,0,0,5,5,5))


# ------------------------------------------------------------------------------------------------------------------
# C.- CURRAN TEST PRO ES - DYNAMICKE PORTFOLIO
# ------------------------------------------------------------------------------------------------------------------
alpha_levels <- c(0.05, 0.025, 0.01, 0.005)
actual_losses <- rpd_test 
T_ <- length(actual_returns)
past_losses <- rpd

loc_bmmQ <- loc_bmmQ_d
loc_bmmM <- loc_bmmM_d
loc_bmmW <- loc_bmmW_d
scale_bmmQ <- scale_bmmQ_d
scale_bmmM <- scale_bmmM_d
scale_bmmW <- scale_bmmW_d
shape_bmmQ <- shape_bmmQ_d
shape_bmmM <- shape_bmmM_d
shape_bmmW <- shape_bmmW_d

u1 <- u1d
u2 <- u2d
scale_pot1 <- scale_pot1_d
scale_potD1 <- scale_potD1_d
scale_pot2 <- scale_pot2_d
scale_potD2 <- scale_potD2_d
shape_pot1 <- shape_pot1_d
shape_potD1 <- shape_potD1_d
shape_pot2 <- shape_pot2_d
shape_potD2 <- shape_potD2_d
rate_pot1 <- rate_pot1_d
rate_potD1 <- rate_potD1_d
rate_pot2 <- rate_pot2_d
rate_potD2 <- rate_potD2_d

VaR_matrix <- - t(df_d_var[,c(3)])
colnames(VaR_matrix) <- df_d_var[,1]
VaR_matrix

ES_matrix <- t(df_d_es[,c(3)])
colnames(ES_matrix) <- df_d_es[,1]
ES_matrix

# --- HLAVNÍ LOOP PŘES MODELY ---
results <- data.frame()
i=0
for (col in colnames(VaR_matrix)) {
  # Zjistíme hladinu VaR ze jména sloupce (např. hist_99 -> 0.01)
  alpha_str <- gsub(".*_(\\d+)$", "\\1", col)
  i=i+1
  if (nchar(alpha_str)==2) {
    alpha_num <- 1- (as.numeric(alpha_str) / 100)
  }
  else if (nchar(alpha_str)==3) {
    alpha_num <- 1-(as.numeric(alpha_str) / 1000)
  }
  if (!(round(alpha_num, 2) %in% round(alpha_levels,2))) next  # kontrola
  
  test_result <- cc_test_fun(actual_losses, past_losses, VaR_matrix[i], alpha_num, col)
  Z <- sqrt(3*T_)*(((2*test_result)-alpha_num)/(sqrt(alpha_num*(4-(3*alpha_num)))))
  p_val <- 2 * (1 - pnorm(abs(Z)))
  results <- rbind(results, data.frame(model = col, VaR = round(VaR_matrix[i],5), ES = round(ES_matrix[i],5), alpha = alpha_num, H_T = round(test_result,5), 
                                       Z = round(Z,5), p_value = round(p_val,5), vysl_curran = p_val>.05))
}

# --- VÝSLEDEK ---
print(results[results[,8]==TRUE,])

vysl_curr_d <- cbind(results, Odhad = rep(c("95% ES","97,5% ES","99% ES","99,5% ES"),9), 
                     Metoda = c(rep("HS",4), rep("Var.-Kovar.",4), rep("GP pro $u_1$",4), rep("Decl. GP pro $u_1$",4),
                                rep("GP pro $u_2$",4), rep("Decl. GP pro $u_2$",4), rep("GEV čtvrtletní",4), rep("GEV měsíční",4), rep("GEV týdenní",4)))
vysl_curr_d <- data.frame(vysl_curr_d$Odhad, vysl_curr_d$Metoda, vysl_curr_d$ES, vysl_curr_d$Z, vysl_curr_d$p_value)
colnames(vysl_curr_d)<- c("Odhad", "Metoda", "$hat{ES}$", "Test. statistika", "p-hodnota")
vysl_curr_d[c(1,5,9,13,17,21,25,29,33,2,6,10,14,18,22,26,30,34,3,7,11,15,19,23,27,31,35,4,8,12,16,20,24,28,32,36),c(9,10,3,6,7)]

xtable(vysl_curr_d, caption = "Výsledky Costanzino-Curranova testu pro dynamické portfolio.",
       label = "back_curr_d_tab", digits = c(0,0,0,5,5,5))

# ------------------------------------------------------------------------------------------------------------------
# FZ ZTRATOVA FUNKCE - KONZERVATIVNI PORTFOLIO
# ------------------------------------------------------------------------------------------------------------------
alpha_levels <- c(0.05, 0.025, 0.01, 0.005)
actual_returns <- rpcon_test 
T_ <- length(actual_returns)

# Data frame: sloupce jsou odhady VaR pro každý model a hladinu
VaR_matrix <- t(df_c_var[,c(3)])
colnames(VaR_matrix) <- df_c_var[,1]
VaR_matrix
ES_matrix <- t(df_c_es[,c(3)])
colnames(ES_matrix) <- df_c_es[,1]
ES_matrix

# --- HLAVNÍ LOOP PŘES MODELY ---
results <- data.frame()
i=0
for (col in colnames(VaR_matrix)) {
  # Zjistíme hladinu VaR ze jména sloupce (např. hist_99 -> 0.01)
  alpha_str <- gsub(".*_(\\d+)$", "\\1", col)
  i=i+1
  if (nchar(alpha_str)==2) {
    alpha_num <- 1- (as.numeric(alpha_str) / 100)
  }
  else if (nchar(alpha_str)==3) {
    alpha_num <- 1-(as.numeric(alpha_str) / 1000)
  }
  if (!(round(alpha_num, 2) %in% round(alpha_levels,2))) next  # kontrola
  
  test_result <- sum(FZLoss(actual_returns, VaR_matrix[i], ES_matrix[i], alpha_num))/T_
  results <- rbind(results, data.frame(model = col, VaR = VaR_matrix[i], ES = ES_matrix[i], alpha = alpha_num, 
                                       FZ_lossfun = test_result))
}

# Přeskupení a vytvoření sloupce rank
results_ranked <- results %>%
  mutate(Odhad = rep(c("95% ES","97,5% ES","99% ES","99,5% ES"),9)) %>%
  mutate(Metoda = c(rep("HS",4), rep("Var.-Kovar.",4), rep("GP pro $u_1$",4), rep("Decl. GP pro $u_1$",4),
                    rep("GP pro $u_2$",4), rep("Decl. GP pro $u_2$",4), rep("GEV čtvrtletní",4), rep("GEV měsíční",4), rep("GEV týdenní",4))) %>%
  arrange(desc(alpha), FZ_lossfun) %>% # Seřadí podle alpha a pak podle FZ_lossfun
  group_by(alpha) %>%           # Seskupí podle alpha
  mutate(rank = rank(FZ_lossfun, ties.method = "first")) %>% # Vytvoří rank
  ungroup()  %>%                 # Zruší skupinování, aby byla tabulka plně dostupná
  as.data.frame()                   

# Výsledná tabulka
vysl_fz_c <- data.frame(results_ranked$Odhad, results_ranked$Metoda, results_ranked$VaR, results_ranked$ES, results_ranked$FZ_lossfun)
colnames(vysl_fz_c)<- c("Odhad", "Metoda", "$widehat{VaR}$", "$widehat{ES}$", "FZ ztrátová funkce")
vysl_fz_c

xtable(vysl_fz_c, caption = "Hodnoty FZ ztrátové funkce pro konzervativní portfolio. Uspořádané v rámci jednotlivých hladin pravděpodobnosti vzestupně podle hodnoty ztrátové funkce. (Vlastní zdroj.)",
       label = "back_fz_c_tab", digits = c(0,0,0,5,5,5))

# --- VÝSLEDEK - prumerne poradi ---
# Výpočet průměrného ranku pro každou metodu
avg_rank_c <- results_ranked %>%
  group_by(Metoda) %>%                      # Seskupení podle metody
  summarise(
    avg_rank = mean(rank, na.rm = TRUE),    # Výpočet průměrného ranku
    avg_FZ = mean(FZ_lossfun, na.rm = TRUE) # Výpočet průměrné hodnoty FZ funkce
  ) %>%
  arrange(avg_rank) %>%                     # Seřazení podle průměrného ranku
  as.data.frame()

print(avg_rank_c) 
xtable(avg_rank_c, caption = "Průměrné pořadí metod odhadů VaR a ES dle hodnoty FZ ztrátové funkce pro konzervativní portfolio.",
       label = "back_fz_avg_c_tab", digits = c(0,0,2,5))


# ------------------------------------------------------------------------------------------------------------------
# FZ ZTRATOVA FUNKCE - DYNAMICKE PORTFOLIO
# ------------------------------------------------------------------------------------------------------------------
actual_returns <- rpdyn_test 
T_ <- length(actual_returns)

# Data frame: sloupce jsou odhady VaR pro každý model a hladinu
VaR_matrix <- t(df_d_var[,c(3)])
colnames(VaR_matrix) <- df_d_var[,1]
VaR_matrix
ES_matrix <- t(df_d_es[,c(3)])
colnames(ES_matrix) <- df_d_es[,1]
ES_matrix

# --- HLAVNÍ LOOP PŘES MODELY ---
results <- data.frame()
i=0
for (col in colnames(VaR_matrix)) {
  # Zjistíme hladinu VaR ze jména sloupce (např. hist_99 -> 0.01)
  alpha_str <- gsub(".*_(\\d+)$", "\\1", col)
  i=i+1
  if (nchar(alpha_str)==2) {
    alpha_num <- 1- (as.numeric(alpha_str) / 100)
  }
  else if (nchar(alpha_str)==3) {
    alpha_num <- 1-(as.numeric(alpha_str) / 1000)
  }
  if (!(round(alpha_num, 2) %in% round(alpha_levels,2))) next  # kontrola
  
  test_result <- sum(FZLoss(actual_returns, VaR_matrix[i], ES_matrix[i], alpha_num))/T_
  results <- rbind(results, data.frame(model = col, VaR = VaR_matrix[i], ES = ES_matrix[i], alpha = alpha_num, 
                                       FZ_lossfun = test_result))
}

# Přeskupení a vytvoření sloupce rank
results_ranked <- results %>%
  mutate(Odhad = rep(c("95% ES","97,5% ES","99% ES","99,5% ES"),9)) %>%
  mutate(Metoda = c(rep("HS",4), rep("Var.-Kovar.",4), rep("GP pro $u_1$",4), rep("Decl. GP pro $u_1$",4),
                    rep("GP pro $u_2$",4), rep("Decl. GP pro $u_2$",4), rep("GEV čtvrtletní",4), rep("GEV měsíční",4), rep("GEV týdenní",4))) %>%
  arrange(desc(alpha), FZ_lossfun) %>% # Seřadí podle alpha a pak podle FZ_lossfun
  group_by(alpha) %>%           # Seskupí podle alpha
  mutate(rank = rank(FZ_lossfun, ties.method = "first")) %>% # Vytvoří rank
  ungroup()  %>%                 # Zruší skupinování, aby byla tabulka plně dostupná
  as.data.frame()                   

# Výsledná tabulka
print(results_ranked)


vysl_fz_d <- data.frame(results_ranked$Odhad, results_ranked$Metoda, results_ranked$VaR, results_ranked$ES, results_ranked$FZ_lossfun)
colnames(vysl_fz_d)<- c("Odhad", "Metoda", "$widehat{VaR}$", "$widehat{ES}$", "FZ ztrátová funkce")
vysl_fz_d

xtable(vysl_fz_d, caption = "Hodnoty FZ ztrátové funkce pro dynamické portfolio. Uspořádané v rámci jednotlivých hladin pravděpodobnosti vzestupně podle hodnoty ztrátové funkce. (Vlastní zdroj.)",
       label = "back_fz_d_tab", digits = c(0,0,0,5,5,5))

# --- VÝSLEDEK - prumerne poradi ---
# Výpočet průměrného ranku pro každou metodu
avg_rank_d <- results_ranked %>%
  group_by(Metoda) %>%                      # Seskupení podle metody
  summarise(
    avg_rank = mean(rank, na.rm = TRUE),    # Výpočet průměrného ranku
    avg_FZ = mean(FZ_lossfun, na.rm = TRUE) # Výpočet průměrné hodnoty FZ funkce
  ) %>%
  arrange(avg_rank) %>%                     # Seřazení podle průměrného ranku
  as.data.frame()

print(avg_rank_d) 
xtable(avg_rank_d, caption = "Průměrné pořadí metod odhadů VaR a ES dle hodnoty FZ ztrátové funkce pro dynamické portfolio.",
       label = "back_fz_avg_d_tab", digits = c(0,0,2,5))


# ------------------------------------------------------------------------------------------------------------------
# KONEC SCRIPTU PRO ZPETNE TESTOVANI
# ------------------------------------------------------------------------------------------------------------------
