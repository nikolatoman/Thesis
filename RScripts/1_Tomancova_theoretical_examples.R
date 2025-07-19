#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------
# RSCRIPT K TEORETICKE CASTI DIPLOMOVE PRACE
#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------

#------------------------------------------------------------------
# UVODNI GRAF VaR
#------------------------------------------------------------------
# Nastavení rozsahu x
x <- seq(-4.99, 5.01, length.out = 100)
# Výpočet hustoty pravděpodobnosti
y <- dnorm(x, mean = 0.01, sd = 1.5)
# Výpočet 5% kvantilu
quantile_5 <- qnorm(0.05, mean = 0.01, sd = 1.5)
quantile_5
# Vytvoření grafu
plot(x, y, type = "l", lwd = 2, col = "blue",
     main = "Hustota pravděpodobnosti výnosu investičního instrumentu",
     xlab = "Logaritmický výnos", ylab = "Hustota pravděpodobnosti")
abline(v = quantile_5, col = "red", lwd = 2, lty = 2)  # Přidání svislé čáry
# Přidání legendy
legend("topright", legend = "5% kvantil", col = "red", lty = 2, lwd = 2)

#------------------------------------------------------------------
# UVODNI GRAF PRO ES (srovnani - ruzne ES pri stejnem VaR)
# ------------------------------------------------------------------------------------------------------------------
library(EnvStats)
# Výpočet 95 % VaR
VaR_level <- 0.05
VaR_student <- qt(VaR_level, 16)
VaR_mixture <- qnormMix(VaR_level, mean1 = 0, sd1 = 1, mean2 = -6, sd2 = 0.25, p.mix = 0.01)
# Výpočet ES
r_student <- rt(1000,16)
es_student <- mean(r_student[r_student<VaR_student])
r_mixture <- rnormMix(1000, mean1 = 0, sd1 = 1, mean2 = -6, sd2 = 0.25, p.mix = 0.01)
es_mixture <- mean(r_mixture[r_mixture<VaR_mixture])
# Vytvoření grafu
x <- seq(-8, 6, by = 0.01)
pdf_student <- dt(x,16)
pdf_mixture <- dnormMix(x, mean1=0, sd1=1, mean2=-6, sd2=0.25, p.mix=0.01)
plot(x, pdf_student, type = "l", lwd = 2, col = "blue",
     main = "Srovnání ES pro 2 rozdílné distribuce",
     xlab = "x", ylab = "f(x)")
lines(x, pdf_mixture, lwd =2, col = "red")
abline(v = VaR_mixture, col = "darkgray", lwd = 2, lty = 2)  # Přidání svislé čáry
abline(v = es_mixture, col = "red", lwd = 2, lty = 2)  # Přidání svislé čáry
abline(v = es_student, col = "blue", lwd = 2, lty = 2)  # Přidání svislé čáry
# Přidání legendy
legend("topright", legend = c(expression(f[1](x)), expression(f[2](x)), expression(VaR[0.05]), expression(ES[1]), expression(ES[2]))
       , col = c("blue", "red", "darkgray", "blue", "red"), lwd = 2, lty = c(1,1,2,2,2))

# ------------------------------------------------------------------------------------------------------------------
# EVT ÚVODNÍ GRAF - SROVNÁNÍ BM A POT
# ------------------------------------------------------------------------------------------------------------------
set.seed(1234)  # Pro reprodukovatelnost
# Generování dat
n <- 20  # Počet bodů
data <- rnorm(n, mean = 5, sd = 1)  # Normální rozdělení
# Metoda blokových maxim
block_size <- 4
blocks <- split(data, ceiling(seq_along(data) / block_size))
block_maxima <- sapply(blocks, max)
block_max_positions <- sapply(blocks, function(block) {
  which(data == max(block))[1]  # Vrací pozici v původní řadě
})
# Metoda spicek nad prahem
threshold <- 5  # Stanovení prahové hodnoty
exceedances <- data[data > threshold]
# Vykreslení grafu
# Block maxima
plot(data, type = "h", col = 'darkgrey', lwd = 2, main = " ",
     xlab = "Index", ylab = "Hodnota")
abline(v = seq(0.5, n+1, by = 4), col = "blue", lty = 2, lwd = 3)  # Přidání bloků
points(block_max_positions, block_maxima, pch = 19)
# Přidání popisků k blokovým maximům
labels_bm <- paste("X_",block_max_positions, sep = "")  # Popisky ve formátu X_číslo
text(block_max_positions, block_maxima, labels_bm, pos = c(4,4,2,2,2), col = "black")  # Přidání popisků

# Překročení prahové hodnoty
plot(data, type = "h", lwd = 2, col = 'darkgrey', main = " ",
     xlab = "Index", ylab = "Hodnota")
abline(h = threshold, col = "blue", lwd = 3, lty = 2)  # Prahová hodnota
points(which(data > threshold), exceedances, pch = 19)
# Přidání popisků k bodům nad prahem
labels_pot <- paste("X_", which(data > threshold), sep = "")  # Popisky ve formátu X_číslo
text(which(data > threshold), exceedances, labels_pot, pos = c(2,4,2,4,3,4,2), col = "black")  # Přidání popisků

# ------------------------------------------------------------------------------------------------------------------
# GRAFY FRECHETOVA, WEIBULLOVA A GUMBELOVA ROZDĚLENÍ
# ------------------------------------------------------------------------------------------------------------------
library(extRemes)
x <- seq(-5, 15, by = 0.01)

# 1. Fréchetovo rozdělení
den_frechet1 <- devd(x, loc = 0, scale = 1, shape = 1, type = "GEV")
den_frechet2 <- devd(x, loc = 0, scale = 1, shape = 0.5, type = "GEV")
den_frechet3 <- devd(x, loc = 0, scale = 1, shape = 0.05, type = "GEV")
plot(x = x, y = den_frechet1, type = "l", lwd = 2, col = 'black', main = "Fréchetovo rozdělení",
     xlab = "x", ylab = "Hustota h(x)")
lines(x = x, y = den_frechet2, type = "l", lwd = 2, col = 'cyan4')
lines(x = x, y = den_frechet3, type = "l", lwd = 2, col = 'coral2')
# Přidání legendy
legend("topright", legend = c(expression(xi == 1),expression(xi == 0.5), expression(xi == 0.05)), col = c("black", "cyan4", "coral2"),lwd = 2)

# 2. Weibullovo rozdělení
x2 <- seq(-10, 10, by = 0.01)
den_weibull1 <- devd(x2, loc = 0, scale = 1, shape = -1, type = "GEV")
den_weibull2 <- devd(x2, loc = 0, scale = 1, shape = -0.5, type = "GEV")
den_weibull3 <- devd(x2, loc = 0, scale = 1, shape = -0.05, type = "GEV")
plot(x = x2, y = den_weibull1, type = "l", lwd = 2, col = 'black', main = "Weibullovo rozdělení",
     xlab = "x", ylab = "Hustota h(x)")
lines(x = x2, y = den_weibull2, type = "l", lwd = 2, col = 'darkorange2')
lines(x = x2, y = den_weibull3, type = "l", lwd = 2, col = 'darkolivegreen4')
# Přidání legendy
legend("topright", legend = c(expression(xi == -1),expression(xi == -0.5), expression(xi == -0.05)), col = c("black", "darkorange2", "darkolivegreen4"),lwd = 2)

# 3. Gumbelovo rozdělení
den_gumbel1 <- devd(x, loc = 0, scale = 1, shape = 0, type = "GEV")
plot(x = x, y = den_gumbel1, type = "l", lwd = 2, col = 'black', main = "Gumbelovo rozdělení",
     xlab = "x", ylab = "Hustota h(x)", ylim = c(0,0.45))
lines(x = x2, y = den_weibull2, type = "l", lwd = 1, lty = 2, col = 'darkorange2')
lines(x = x, y = den_frechet2, type = "l", lwd = 1, lty = 2, col = 'cyan4')
# Přidání legendy
legend("topright", legend = c(expression("Gumbel (" ~ xi == 0 ~ ")"),expression("Fréchet (" ~ xi == 0.5 ~ ")"),expression("Weibull (" ~ xi == -0.5 ~ ")")), col = c("black", "cyan4", "darkorange2"),lwd = c(2,1,1), lty =c(1,2,2))


# ------------------------------------------------------------------------------------------------------------------
# POT - vztah mezi F(x) a F_u(y)
# ------------------------------------------------------------------------------------------------------------------
# Nastavení parametrů
u <- 5
x <- seq(0, 15, length.out = 100)
F_x <- plnorm(x, meanlog = 1, sdlog = .5)
y <- seq(0, 15 - u, length.out = 100)
F_u <- function(y, u) {
  # Funkce pro podmíněnou distribuční funkci
  return(plnorm(y + u, meanlog = 1, sdlog = 0.5))
}
# Vytvoření grafů
par(mfrow = c(1, 2))
# Levý graf: Distribuční funkce F(x)
plot(c(-.002, 15+.5), c(-0.001, 1.05), type = "l", col = "white", xlab = "", ylab = "",
     main = expression("Distribucni funkce F(x)"),
     xaxt = "n" , yaxt = "n" , yaxs="i", xaxs = "i", bty = "l") # prazdny graf
lines(x[x<u], F_x[F_x<plnorm(u,1,.5)], type = "l", lty = 2, col = "blue", lwd = 2)  # prvni cast F(x), pro x < u
lines(x[x>u-.15], F_x[F_x>plnorm(u-.15,1,.5)], type = "l", lty = 1, col = "blue", lwd = 2) # druha cast F(x), pro x > u
points(u, plnorm(u, 1,.5), pch = 20, col = "blue") # bod [u,F(u)]
segments(u, 0, u, plnorm(u, 1,.5), lty = 3) # lepsi vertikala pro x = u 
segments(-.002, 1, 15, 1, lty = 3) # lepsi horizontala pro F(x) = 1 
axis(1, at = c(0,u), labels = c("0", "u")) # dulezite hodnoty na ose x
axis(2, at = c(-.001,1), labels = c("","1"), las = 1) # dulezite hodnoty na ose y
axis(1, at = 7.75, labels = "x", line = 1, tick = FALSE) # popisek osy x umisteny blize k ose x
axis(2, at = .5, labels = "F(x)", line = .5, tick = FALSE) # popisek osy y umisteny blize k ose y
# Pravý graf: Podmíněná distribuční funkce F_u(y)
plot(c(-.01, 15-u+.3), c(plnorm(u, 1,.5)-.0001, 1.005), type = "l", col = "white", 
     xlab = "", ylab = "", main = expression("Podminena distribucni funkce "* F[u](y)), 
     xaxt = "n" , yaxt = "n" , yaxs="i", xaxs = "i", bty = "l")  # prazdny graf
lines(y, F_u(y, u), type = "l", col = "blue", lwd = 2) #F_u(y)
segments(-.002, plnorm(15, 1,.5), 15-u, plnorm(15, 1,.5), lty = 3)  # lepsi horizontala pro F_u(y) = 1 
axis(1, at = -.01, labels = "0") # dulezite hodnoty na ose x
axis(2, at = c(plnorm(u, 1,.5)-.0001,plnorm(15, 1,.5)), labels = c("","1"), las = 1) # dulezite hodnoty na ose y
axis(1, at = 5.15, labels = "y", line = 1, tick = FALSE) # popisek osy x umisteny blize k ose x
axis(2, at = .945, labels = expression(F[u](y)), line = .5, tick = FALSE) # popisek osy y umisteny blize k ose y


# ------------------------------------------------------------------------------------------------------------------
# GPD pro ruzne volby parametru:
# ------------------------------------------------------------------------------------------------------------------
library(extRemes)
x <- seq(.001, 10, length.out = 500)
# Hustota pravděpodobnosti (PDF)
pdf1 <- devd(x, scale = 1, shape = 0, threshold = 0, type = "GP")
pdf2 <- devd(x, scale = 1, shape = 1, threshold = 0, type = "GP")
pdf3 <- devd(x, scale = 1, shape = -.4, threshold = 0, type = "GP")

# Vykreslení PDF
plot(x, pdf1, type = "l", lwd = 2,
     main = "Hustota zobecneneho Paretova rozdeleni",
     xlab = "x", ylab = "G(x)",
     cex.lab = 1.5)
lines(x,pdf2,  col = "blue", lwd = 2)
lines(x, pdf3, col = "red", lwd = 2)
# Přidání legendy
legend("topright", legend = c(expression(xi == 0),expression(xi == 1),expression(xi == -0.4)), col = c("black", "blue", "red"),lwd = 2, cex=1.2)

# ------------------------------------------------------------------------------------------------------------------
# MEAN EXCESS PLOT
# ------------------------------------------------------------------------------------------------------------------
# Generování dat z lognormálního rozdělení
set.seed(123)
data <- rlnorm(1000, meanlog = 0, sdlog = 1)
plot(density(data), xlab = "x", ylab = "f(x)", main = "", cex.lab = 1.5, lwd = 2, cex.axis = 1.5)#, main = "Lnorm(0,1)")

# Mean Excess Plot
library(extRemes)
mrlplot(data, nint = 100, alpha = 0.05, xlab = "u", cex.lab = 1.5, cex.axis = 1.5)

# Počet pozorování přesahujících práh u
thresholds <- seq(min(data)+.01, max(data)-.01, length.out = 100)  # Získání prahů
exc_cnt_f <- function(u, data) {
  return(sum(data > u))
}
exceedances_cnt <- sapply(thresholds, exc_cnt_f, data = data)  # Získání počtu pozorování přesahujících daný práh u, tj. n_u
axis(3, at = thresholds[seq(1,100,10)], labels = exceedances_cnt[seq(1,100,10)], 
     col.axis = "darkgray", col.ticks = "darkgray", cex.axis = 1.2)
mtext(expression(n[u]), side = 3, line = 2, col = "darkgrey", cex = 1.5)

#výpočet intervalu spolehlivosti pro volbu prahu u=3.5
mean_exc_ci_f <- function(u, data) {
  exceedances <- data[data > u] - u
  alpha <- 0.05
  sd_excess <- sd(exceedances)
  mean_excess <- mean(exceedances)
  n <- length(exceedances)
  z_value <- qnorm(1-alpha/2)  # Kritická hodnota normálního rozdělení
  ci_lower <- mean_excess - z_value * sd_excess / sqrt(n)
  ci_upper <- mean_excess + z_value * sd_excess / sqrt(n)
  cat("Počet pozorování přesahujícíh práh",u,":\n", n)
  cat("\n\nEmpirický odhad střední hodnoty excesů :\n", mean_excess)
  cat("\n\n95% interval spolehlivosti :\n")
  print(c(ci_lower,ci_upper))
}
mean_exc_ci_f(3.5, data)  # Získání hodnot pro volbu prahu 3.5

threshrange.plot(data, c(2.5,13), type = c("GP"), nint = 100, alpha = 0.05, set.panels = TRUE, verbose = FALSE)
threshrange.plot(data, c(0,9), type = c("GP"), nint = 100, alpha = 0.05, set.panels = TRUE, verbose = FALSE)

# ------------------------------------------------------------------------------------------------------------------
# KONEC SCRIPTU PRO TEORETICKOU CAST
# ------------------------------------------------------------------------------------------------------------------
