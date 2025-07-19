# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# UVODNI RSCRIPT PRO APLIKACNI CAST PRACE - VYBER A ZPRACOVANI DAT, EDA
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------------------------
# VYBER AKCII DO PORTFOLIA - ZISKANI DAT Z YAHOO FINANCE
# ------------------------------------------------------------------------------------------------------------------
library(quantmod)
# Definice období a cesty pro uložení
start_date <- "2015-01-01"
end_date <- "2025-01-01"
save_path <- "DIPLOMKA/data_final"

# Historický vývoj měnového kurzu
getSymbols("EURUSD=X", src = "yahoo", from = start_date, to = end_date)
write.csv(as.data.frame(`EURUSD=X`), file = paste0(save_path, "/kurz_eur_usd.csv"), row.names = TRUE)

# Konzervativní portfolio:
# S&P500 ETF EUR Hedged
getSymbols("IUSE.L", src = "yahoo", from = start_date, to = end_date)
write.csv(as.data.frame(`IUSE.L`), file = paste0(save_path, "/iuse_l.csv"), row.names = TRUE)
# Vládní dluhopisy Eurozóny 7-10 let
getSymbols("SXRQ.DE", src = "yahoo", from = start_date, to = end_date)
write.csv(as.data.frame(`SXRQ.DE`), file = paste0(save_path, "/sxrq_de.csv"), row.names = TRUE)
# Korporátní dluhopisy Eurozóny
getSymbols("D5BG.DE", src = "yahoo", from = start_date, to = end_date)
write.csv(as.data.frame(`D5BG.DE`), file = paste0(save_path, "/d5bg_de.csv"), row.names = TRUE)
# Vanguard FTSE All-World ex-US Index Fund ETF
getSymbols("VEU", src = "yahoo", from = start_date, to = end_date)
write.csv(as.data.frame(`VEU`), file = paste0(save_path, "/veu.csv"), row.names = TRUE)
# Americké státní dluhopisy 7-10 let
getSymbols("ITPS.SW", src = "yahoo", from = start_date, to = end_date)
write.csv(as.data.frame(`ITPS.SW`), file = paste0(save_path, "/itps_sw.csv"), row.names = TRUE)
# SPDR Gold Shares
getSymbols("XAD5.MI", src = "yahoo", from = start_date, to = end_date)
write.csv(as.data.frame(`XAD5.MI`), file = paste0(save_path, "/xad5_mi.csv"), row.names = TRUE)
# iShares U.S. Real Estate ETF
getSymbols("XREA.DE", src = "yahoo", from = start_date, to = end_date)
write.csv(as.data.frame(`XREA.DE`), file = paste0(save_path, "/xrea_de.csv"), row.names = TRUE)

# Dynamické portfolio:
# PPC-USD
getSymbols("PPC-EUR", src = "yahoo", from = start_date, to = end_date)
write.csv(as.data.frame(`PPC-EUR`), file = paste0(save_path, "/ppc_eur.csv"), row.names = TRUE)
# BTC-USD
getSymbols("BTC-EUR", src = "yahoo", from = start_date, to = end_date)
write.csv(as.data.frame(`BTC-EUR`), file = paste0(save_path, "/btc_eur.csv"), row.names = TRUE)
# iShares MSCI USA Small Cap ESG Enhanced UCITS ETF USD (Acc)
getSymbols("CUSS.L", src = "yahoo", from = start_date, to = end_date)
write.csv(as.data.frame(`CUSS.L`), file = paste0(save_path, "/cuss_l.csv"), row.names = TRUE)
# iShares MSCI Europe SRI UCITS ETF
getSymbols("IUSK.DE", src = "yahoo", from = start_date, to = end_date)
write.csv(as.data.frame(`IUSK.DE`), file = paste0(save_path, "/iusk_de.csv"), row.names = TRUE)
# iShares Core MSCI EM IMI UCITS ETF USD (Acc)
getSymbols("EIMI.L", src = "yahoo", from = start_date, to = end_date)
write.csv(as.data.frame(`EIMI.L`), file = paste0(save_path, "/eimi_l.csv"), row.names = TRUE)
# iShares Core S&P 500 UCITS ETF
getSymbols("CSPX.L", src = "yahoo", from = start_date, to = end_date)
write.csv(as.data.frame(`CSPX.L`), file = paste0(save_path, "/cspx_l.csv"), row.names = TRUE)
# iShares Core MSCI Japan IMI UCITS ETF USD (Acc)
getSymbols("IJPA.AS", src = "yahoo", from = start_date, to = end_date)
write.csv(as.data.frame(`IJPA.AS`), file = paste0(save_path, "/ijpa_as.csv"), row.names = TRUE)

# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------         EDA        -------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------

# Načtení CSV souborů pro všechna aktiva a převedení na časovou řadu
library(xts)
#Kurz EURUSD
EURUSD <- read.csv(paste0(save_path, "/kurz_eur_usd.csv"))
EURUSD$X <- as.Date(EURUSD$X, format = "%Y-%m-%d")
EURUSD <- xts(EURUSD[, -1], order.by = EURUSD$X)
# nalezena chyba v datovém souboru, bylo nutné ji ještě dodatečně manuálně opravit, aby nedošlo k časovému posunu
# Posuneme indexy o 1 den dopředu
index(EURUSD)[which(index(EURUSD) >= as.Date("2014-03-29"))] <- index(EURUSD)[which(index(EURUSD) >= as.Date("2014-03-29"))] + 1
na.locf(EURUSD)

# Konzervativní portfolio:
IUSE_L <- read.csv(paste0(save_path, "/iuse_l.csv"))
IUSE_L$X <- as.Date(IUSE_L$X, format = "%Y-%m-%d")
IUSE_L <- xts(IUSE_L[, -1], order.by = IUSE_L$X)

SXRQ_DE <- read.csv(paste0(save_path, "/sxrq_de.csv"))
SXRQ_DE$X <- as.Date(SXRQ_DE$X, format = "%Y-%m-%d")
SXRQ_DE <- xts(SXRQ_DE[, -1], order.by = SXRQ_DE$X)

D5BG_DE <- read.csv(paste0(save_path, "/d5bg_de.csv"))
D5BG_DE$X <- as.Date(D5BG_DE$X, format = "%Y-%m-%d")
D5BG_DE <- xts(D5BG_DE[, -1], order.by = D5BG_DE$X)

VEU <- read.csv(paste0(save_path, "/veu.csv"))
VEU$X <- as.Date(VEU$X, format = "%Y-%m-%d")
VEU <- xts(VEU[, -1], order.by = VEU$X)

ITPS_SW <- read.csv(paste0(save_path, "/itps_sw.csv"))
ITPS_SW$X <- as.Date(ITPS_SW$X, format = "%Y-%m-%d")
ITPS_SW <- xts(ITPS_SW[, -1], order.by = ITPS_SW$X)

XAD5_MI <- read.csv(paste0(save_path, "/xad5_mi.csv"))
XAD5_MI$X <- as.Date(XAD5_MI$X, format = "%Y-%m-%d")
XAD5_MI <- xts(XAD5_MI[, -1], order.by = XAD5_MI$X)

XREA_DE <- read.csv(paste0(save_path, "/xrea_de.csv"))
XREA_DE$X <- as.Date(XREA_DE$X, format = "%Y-%m-%d")
XREA_DE <- xts(XREA_DE[, -1], order.by = XREA_DE$X)

# Dynamické portfolio:
PPC_EUR <- read.csv(paste0(save_path, "/ppc_eur.csv"))
PPC_EUR$X <- as.Date(PPC_EUR$X, format = "%Y-%m-%d")
PPC_EUR <- xts(PPC_EUR[, -1], order.by = PPC_EUR$X)

BTC_EUR <- read.csv(paste0(save_path, "/btc_eur.csv"))
BTC_EUR$X <- as.Date(BTC_EUR$X, format = "%Y-%m-%d")
BTC_EUR <- xts(BTC_EUR[, -1], order.by = BTC_EUR$X)

CUSS_L <- read.csv(paste0(save_path, "/cuss_l.csv"))
CUSS_L$X <- as.Date(CUSS_L$X, format = "%Y-%m-%d")
CUSS_L <- xts(CUSS_L[, -1], order.by = CUSS_L$X)

IUSK_DE <- read.csv(paste0(save_path, "/iusk_de.csv"))
IUSK_DE$X <- as.Date(IUSK_DE$X, format = "%Y-%m-%d")
IUSK_DE <- xts(IUSK_DE[, -1], order.by = IUSK_DE$X)

EIMI_L <- read.csv(paste0(save_path, "/eimi_l.csv"))
EIMI_L$X <- as.Date(EIMI_L$X, format = "%Y-%m-%d")
EIMI_L <- xts(EIMI_L[, -1], order.by = EIMI_L$X)

CSPX_L <- read.csv(paste0(save_path, "/cspx_l.csv"))
CSPX_L$X <- as.Date(CSPX_L$X, format = "%Y-%m-%d")
CSPX_L <- xts(CSPX_L[, -1], order.by = CSPX_L$X)

IJPA_AS <- read.csv(paste0(save_path, "/ijpa_as.csv"))
IJPA_AS$X <- as.Date(IJPA_AS$X, format = "%Y-%m-%d")
IJPA_AS <- xts(IJPA_AS[, -1], order.by = IJPA_AS$X)

# ------------------------------------------------------------------------------------------------------------------
# LOGARITMICKÉ DENNÍ VÝNOSY (VČETNĚ ZOHLEDNĚNÍ MĚNOVÉHO KURZU)
# ------------------------------------------------------------------------------------------------------------------
EURUSDa <- EURUSD$EURUSD.X.Adjusted

r_IUSE2 <- na.omit(diff(log(IUSE_L$IUSE.L.Adjusted)))*100 
r_SXRQ2 <- na.omit(diff(log(SXRQ_DE$SXRQ.DE.Adjusted)))*100
r_D5BG2 <- na.omit(diff(log(D5BG_DE$D5BG.DE.Adjusted)))*100
r_VEU2 <- na.omit(diff(log(VEU$VEU.Adjusted * EURUSDa)))*100 
r_ITPS2 <- na.omit(diff(log(ITPS_SW$ITPS.SW.Adjusted * EURUSDa)))*100
r_XAD52 <- na.omit(diff(log(XAD5_MI$XAD5.MI.Adjusted * EURUSDa)))*100
r_XREA2 <- na.omit(diff(log(XREA_DE$XREA.DE.Adjusted * EURUSDa)))*100

r_PPC2 <- na.omit(diff(log(PPC_EUR$PPC.EUR.Adjusted)))*100
r_BTC2 <- na.omit(diff(log(BTC_EUR$BTC.EUR.Adjusted)))*100
r_CUSS2 <- na.omit(diff(log(CUSS_L$CUSS.L.Adjusted * EURUSDa)))*100
r_IUSK2 <- na.omit(diff(log(IUSK_DE$IUSK.DE.Adjusted)))*100
r_EIMI2 <- na.omit(diff(log(EIMI_L$EIMI.L.Adjusted * EURUSDa)))*100
r_CSPX2 <- na.omit(diff(log(CSPX_L$CSPX.L.Adjusted * EURUSDa)))*100
r_IJPA2 <- na.omit(diff(log(IJPA_AS$IJPA.AS.Adjusted)))*100

# ------------------------------------------------------------------------------------------------------------------
# HISTOGRAMY + jádrové odhady hustoty + srovnání s hustotou odpovídajícího normálního rozdělení
# ------------------------------------------------------------------------------------------------------------------

# Konzervativní portfolio:
par(mfrow = c(7, 2), mar = c(5, 4, 0, 0))
hist(r_IUSE2, xlab = "r_IUSE (log)", ylab = "Hustota", breaks = 30, freq = FALSE, col = "lightgrey", ylim=c(0,.70), main="")
lines(density(r_IUSE2, bw = "nrd0", kernel = "gaussian"), lwd = 2, col = "red")
x = seq(min(r_IUSE2), max(r_IUSE2), length = 100)
curve(dnorm(x, mean = mean(r_IUSE2), sd = sd(r_IUSE2)), col = "blue", lwd = 2, lty = 2, add = TRUE)

hist(r_SXRQ2, xlab = "r_SXRQ (log)", ylab = "Hustota", breaks = 30, freq = FALSE, col = "lightgrey", ylim=c(0,1.90), main="")
lines(density(r_SXRQ2, bw = "nrd0", kernel = "gaussian"), lwd = 2, col = "red")
x = seq(min(r_SXRQ2), max(r_SXRQ2), length = 100)
curve(dnorm(x, mean = mean(r_SXRQ2), sd = sd(r_SXRQ2)), col = "blue", lwd = 2, lty = 2, add = TRUE)

hist(r_D5BG2, xlab = "r_D5BG (log)", ylab = "Hustota", breaks = 50, freq = FALSE, col = "lightgrey", ylim=c(0,2.60), main="")
lines(density(r_D5BG2, bw = "nrd0", kernel = "gaussian"), lwd = 2, col = "red")
x = seq(min(r_D5BG2), max(r_D5BG2), length = 100)
curve(dnorm(x, mean = mean(r_D5BG2), sd = sd(r_D5BG2)), col = "blue", lwd = 2, lty = 2, add = TRUE)

hist(r_VEU2, xlab = "r_VEU (log)", ylab = "Hustota", breaks = 30, freq = FALSE, col = "lightgrey", main="")
lines(density(r_VEU2, bw = "nrd0", kernel = "gaussian"), lwd = 2, col = "red")
x = seq(min(r_VEU2), max(r_VEU2), length = 100)
curve(dnorm(x, mean = mean(r_VEU2), sd = sd(r_VEU2)), col = "blue", lwd = 2, lty = 2, add = TRUE)

hist(r_ITPS2, xlab = "r_ITPS (log)", ylab = "Hustota", breaks = 30, freq = FALSE, col = "lightgrey", main="")
lines(density(r_ITPS2, bw = "nrd0", kernel = "gaussian"), lwd = 2, col = "red")
x = seq(min(r_ITPS2), max(r_ITPS2), length = 100)
curve(dnorm(x, mean = mean(r_ITPS2), sd = sd(r_ITPS2)), col = "blue", lwd = 2, lty = 2, add = TRUE)

hist(r_XAD52, xlab = "r_XAD5 (log)", ylab = "Hustota", breaks = 30, freq = FALSE, col = "lightgrey", ylim=c(0,.5), main="")
lines(density(r_XAD52, bw = "nrd0", kernel = "gaussian"), lwd = 2, col = "red")
x = seq(min(r_XAD52), max(r_XAD52), length = 100)
curve(dnorm(x, mean = mean(r_XAD52), sd = sd(r_XAD52)), col = "blue", lwd = 2, lty = 2, add = TRUE)

hist(r_XREA2, xlab = "r_XREA (log)", ylab = "Hustota", breaks = 30, freq = FALSE, col = "lightgrey", ylim=c(0,.45), main="")
lines(density(r_XREA2, bw = "nrd0", kernel = "gaussian"), lwd = 2, col = "red")
x = seq(min(r_XREA2), max(r_XREA2), length = 100)
curve(dnorm(x, mean = mean(r_XREA2), sd = sd(r_XREA2)), col = "blue", lwd = 2, lty = 2, add = TRUE)

# Dynamické portfolio:
hist(r_PPC2, xlab = "r_PPC (log)", ylab = "Hustota", breaks = 30, freq = FALSE, col = "lightgrey", ylim=c(0,.13), main="")
lines(density(r_PPC2, bw = "nrd0", kernel = "gaussian"), lwd = 2, col = "red")
x = seq(min(r_PPC2), max(r_PPC2), length = 100)
curve(dnorm(x, mean = mean(r_PPC2), sd = sd(r_PPC2)), col = "blue", lwd = 2, lty = 2, add = TRUE)

hist(r_BTC2, xlab = "r_BTC (log)", ylab = "Hustota", breaks = 30, freq = FALSE, col = "lightgrey", ylim=c(0,.20), main="")
lines(density(r_BTC2, bw = "nrd0", kernel = "gaussian"), lwd = 2, col = "red")
x = seq(min(r_BTC2), max(r_BTC2), length = 100)
curve(dnorm(x, mean = mean(r_BTC2), sd = sd(r_BTC2)), col = "blue", lwd = 2, lty = 2, add = TRUE)

hist(r_CUSS2, xlab = "r_CUSS (log)", ylab = "Hustota", breaks=30, freq = FALSE, col = "lightgrey", main="")
lines(density(r_CUSS2, bw = "nrd0", kernel = "gaussian"), lwd = 2, col = "red")
x = seq(min(r_CUSS2), max(r_CUSS2), length = 100)
curve(dnorm(x, mean = mean(r_CUSS2), sd = sd(r_CUSS2)), col = "blue", lwd = 2, lty = 2, add = TRUE)

hist(r_IUSK2, xlab = "r_IUSK (log)", ylab = "Hustota", breaks = 30, freq = FALSE, col = "lightgrey", ylim=c(0,.60), main="")
lines(density(r_IUSK2, bw = "nrd0", kernel = "gaussian"), lwd = 2, col = "red")
x = seq(min(r_IUSK2), max(r_IUSK2), length = 100)
curve(dnorm(x, mean = mean(r_IUSK2), sd = sd(r_IUSK2)), col = "blue", lwd = 2, lty = 2, add = TRUE)

hist(r_EIMI2, xlab = "r_EIMI (log)", ylab = "Hustota", breaks = 30, freq = FALSE, col = "lightgrey", ylim=c(0,.40), main="")
lines(density(r_EIMI2, bw = "nrd0", kernel = "gaussian"), lwd = 2, col = "red")
x = seq(min(r_EIMI2), max(r_EIMI2), length = 100)
curve(dnorm(x, mean = mean(r_EIMI2), sd = sd(r_EIMI2)), col = "blue", lwd = 2, lty = 2, add = TRUE)

hist(r_CSPX2, xlab = "r_CSPX (log)", ylab = "Hustota", breaks = 30, freq = FALSE, col = "lightgrey", main="")
lines(density(r_CSPX2, bw = "nrd0", kernel = "gaussian"), lwd = 2, col = "red")
x = seq(min(r_CSPX2), max(r_CSPX2), length = 100)
curve(dnorm(x, mean = mean(r_CSPX2), sd = sd(r_CSPX2)), col = "blue", lwd = 2, lty = 2, add = TRUE)

hist(r_IJPA2, xlab = "r_IJPA (log)", ylab = "Hustota", breaks = 30, freq = FALSE, col = "lightgrey", ylim = c(0,.5), main="")
lines(density(r_IJPA2, bw = "nrd0", kernel = "gaussian"), lwd = 2, col = "red")
x = seq(min(r_IJPA2), max(r_IJPA2), length = 100)
curve(dnorm(x, mean = mean(r_IJPA2), sd = sd(r_IJPA2)), col = "blue", lwd = 2, lty = 2, add = TRUE)

dev.off()

# ------------------------------------------------------------------------------------------------------------------
# CELKOVE DENNI VYNOSY Z KONZERVATIVNIHO PORTFOLIA
# ------------------------------------------------------------------------------------------------------------------
# uprava dat tak, aby byly xts stejne delky -> doplneni hodnot metodou LOCF
fill_missing_dates <- function(data_xts) {
  all_dates <- seq(from = start(data_xts), to = end(data_xts), by = "days")
  complete_xts <- merge.xts(data_xts, xts(, all_dates))
  complete_xts <- na.locf(complete_xts)
  return(complete_xts)
}
EURUSDa <- fill_missing_dates(EURUSD$EURUSD.X.Adjusted)

r_IUSE2 <- na.omit(diff(log(fill_missing_dates(IUSE_L$IUSE.L.Adjusted))))*100 
r_SXRQ2 <- na.omit(diff(log(fill_missing_dates(SXRQ_DE$SXRQ.DE.Adjusted))))*100
r_D5BG2 <- na.omit(diff(log(fill_missing_dates(D5BG_DE$D5BG.DE.Adjusted))))*100
r_VEU2 <- na.omit(diff(log(fill_missing_dates(VEU$VEU.Adjusted) * EURUSDa)))*100 
r_ITPS2 <- na.omit(diff(log(fill_missing_dates(ITPS_SW$ITPS.SW.Adjusted) * EURUSDa)))*100
r_XAD52 <- na.omit(diff(log(fill_missing_dates(XAD5_MI$XAD5.MI.Adjusted) * EURUSDa)))*100
r_XREA2 <- na.omit(diff(log(fill_missing_dates(XREA_DE$XREA.DE.Adjusted) * EURUSDa)))*100

conservative <- list(r_IUSE2, r_SXRQ2, r_D5BG2, r_VEU2, r_ITPS2, r_XAD52, r_XREA2)
# Najdeme společný časový rozsah
od <- max(sapply(conservative, function(x) min(index(x))))
do <- min(sapply(conservative, function(x) max(index(x))))
# Ořízneme aktiva na společný rozsah a převedeme je do data.frame
dfc <- data.frame(lapply(conservative, function(x) x[which(index(x) == od):which(index(x) == do)]))
names(dfc) <- c("r_IUSE2", "r_SXRQ2", "r_D5BG2", "r_VEU2", "r_ITPS2", "r_XAD52", "r_XREA2")
head(dfc)

# vynosy z portfolia r_p
vahyc <- c(.2,.2,.1,.1,.2,.1,.1)
rp_con <- xts(as.matrix(dfc) %*% vahyc, order.by = as.Date(rownames(dfc), format = "%Y-%m-%d"))
colnames(rp_con) <- "Daily r_p cons."
head(rp_con)

# ------------------------------------------------------------------------------------------------------------------
# CELKOVE DENNI VYNOSY Z DYNAMICKEHO PORTFOLIA
# ------------------------------------------------------------------------------------------------------------------
r_PPC2 <- na.omit(diff(log(fill_missing_dates(PPC_EUR$PPC.EUR.Adjusted))))*100
r_BTC2 <- na.omit(diff(log(fill_missing_dates(BTC_EUR$BTC.EUR.Adjusted))))*100
r_CUSS2 <- na.omit(diff(log(fill_missing_dates(CUSS_L$CUSS.L.Adjusted) * EURUSDa)))*100
r_IUSK2 <- na.omit(diff(log(fill_missing_dates(IUSK_DE$IUSK.DE.Adjusted))))*100
r_EIMI2 <- na.omit(diff(log(fill_missing_dates(EIMI_L$EIMI.L.Adjusted) * EURUSDa)))*100
r_CSPX2 <- na.omit(diff(log(fill_missing_dates(CSPX_L$CSPX.L.Adjusted) * EURUSDa)))*100
r_IJPA2 <- na.omit(diff(log(fill_missing_dates(IJPA_AS$IJPA.AS.Adjusted))))*100

dynamic <- list(r_PPC2,r_BTC2,r_CUSS2,r_IUSK2,r_EIMI2,r_CSPX2,r_IJPA2)
# Najdeme společný časový rozsah
od <- max(sapply(dynamic, function(x) min(index(x))))
do <- min(sapply(dynamic, function(x) max(index(x))))
# Ořízneme aktiva na společný rozsah a převedeme je do data.frame
dfd <- data.frame(lapply(dynamic, function(x) x[which(index(x) == od):which(index(x) == do)]))
names(dfd) <- c("r_PPC2","r_BTC2","r_CUSS2","r_IUSK2","r_EIMI2","r_CSPX2","r_IJPA2")
head(dfd)

# vynosy z portfolia r_p
vahyd <- c(.05,.15,.2,.2,.1,.1,.2)
rp_dyn <- xts(as.matrix(dfd) %*% vahyd, order.by = as.Date(rownames(dfd), format = "%Y-%m-%d"))
colnames(rp_dyn) <- "Daily r_p dynam."
head(rp_dyn)


# ------------------------------------------------------------------------------------------------------------------
# SPLIT NA TRENOVACI A TESTOVACI DATA
# ------------------------------------------------------------------------------------------------------------------
rpcon_train = rp_con[1:which(index(rp_con) =="2021-12-31"), ]
rpcon_test = rp_con[which(index(rp_con) > "2021-12-31"), ]

rpdyn_train = rp_dyn[1:which(index(rp_dyn) =="2021-12-31"), ]
rpdyn_test = rp_dyn[which(index(rp_dyn) > "2021-12-31"), ]


# ------------------------------------------------------------------------------------------------------------------
# VZTAHY MEZI AKTIVY V PORTFOLIU
# ------------------------------------------------------------------------------------------------------------------
par(mar = c(0, 0, 0, 0))
# Scatterplot pro konzervativní portfolio
pairs(dfc, main="", pch=19, cex.labels = 2)
# Scatterplot pro dynamické portfolio
pairs(dfd, main="", pch=19, cex.labels = 2)
dev.off()


# ------------------------------------------------------------------------------------------------------------------
# EDA - ZAKL. POPISNE STATISTIKY PRO TRENOVACI DATA
# ------------------------------------------------------------------------------------------------------------------
library(xtable)

# Výpočet základních popisných statistik
popisne_statistiky <- function(data) {
  return(data.frame(
    "Počet pozorování" = sapply(data, function(x) sum(!is.na(x))),
    "Minimální hodnota" = sapply(data, function(x) round(min(x, na.rm = TRUE), 5)),
    "1. kvartil" = sapply(data, function(x) round(quantile(x, 0.25, na.rm = TRUE), 5)),
    "Medián" = sapply(data, function(x) round(median(x, na.rm = TRUE), 5)),
    "Průměr" = sapply(data, function(x) round(mean(x, na.rm = TRUE), 5)),
    "3. kvartil" = sapply(data, function(x) round(quantile(x, 0.75, na.rm = TRUE), 5)),
    "Maximální hodnota" = sapply(data, function(x) round(max(x, na.rm = TRUE), 5)),
    "Směrodatná odchylka" = sapply(data, function(x) round(sd(x, na.rm = TRUE), 5))
  ))
}

# Vytvoření popisných statistik pro oba datové soubory
statistiky_rpcon <- popisne_statistiky(rpcon_train)
statistiky_rpdyn <- popisne_statistiky(rpdyn_train)

# Vytvoření LaTeX tabulek pomocí xtable
xtable(statistiky_rpcon, caption = "Popisné statistiky pro rpcon\\_train", label = "tab:rpcon",digits=c(0,0,5,5,5,5,5,5,5))
xtable(statistiky_rpdyn, caption = "Popisné statistiky pro rpdyn\\_train", label = "tab:rpdyn",digits=c(0,0,5,5,5,5,5,5,5))

# ------------------------------------------------------------------------------------------------------------------
# EDA PRO VYNOSY Z KONZERVATIVNIHO PORTFOLIA
# ------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(gridExtra)

# graf casove rady vynosu z portfolia
plot1 <- ggplot(rp_con, aes(x = index(rp_con), y = rp_con$`Daily r_p cons.`)) +
  geom_line(color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype = "dashed", color = "red") +
  labs(title = "Konzervativní portfolio", y = "Denní výnos (%)", x = NULL) +
  ylim(-19, 8) +
  theme_minimal()

# histogram
hist(rpcon_train, xlab = "Logaritmické výnosy portfolia", ylab = "Hustota", main = "Konzervativní portfolio", breaks = 40, freq = FALSE, col = "lightgrey", ylim=c(0,3))
lines(density(rpcon_train, bw = "nrd0", kernel = "gaussian"), lwd = 2, col = "red")
x = seq(min(rpcon_train), max(rpcon_train), length = 100)
curve(dnorm(x, mean = mean(rpcon_train), sd = sd(rpcon_train)), col = "blue", lwd = 2, lty = 2, add = TRUE)

# QQ plot pro test normality výnosů konzervativního portfolia
# par(mar=c(4,4.2,3,.5))
stats::qqnorm(rpcon_train, main="QQ Plot konzervativního portfolia", ylab = "Empirické kvantily", xlab = "Teoretické kvantily", cex.lab=1.5, cex.main=1.8)
qqline(rpcon_train)

# ------------------------------------------------------------------------------------------------------------------
# EDA PRO VYNOSY Z DYNAMICKEHO PORTFOLIA
# ------------------------------------------------------------------------------------------------------------------
summary(rpdyn_train)

# graf casove rady vynosu z portfolia
plot2 <- ggplot(rp_dyn, aes(x = index(rp_dyn), y = rp_dyn$`Daily r_p dynam.`)) +
  geom_line(color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype = "dashed", color = "red") +
  labs(title = "Dynamické portfolio", y = "Denní výnos (%)", x = NULL) +
  ylim(-19, 8) +
  theme_minimal()
# Kombinace grafů vedle sebe
grid.arrange(plot1, plot2, ncol = 2)

# histogram
hist(rpdyn_train, xlab = "Logaritmické výnosy portfolia", ylab = "Hustota", main = "Dynamické portfolio", breaks = 40, freq = FALSE, col = "lightgrey", ylim=c(0,.6))
lines(density(rpdyn_train, bw = "nrd0", kernel = "gaussian"), lwd = 2, col = "red")
x = seq(min(rpdyn_train), max(rpdyn_train), length = 100)
curve(dnorm(x, mean = mean(rpdyn_train), sd = sd(rpdyn_train)), col = "blue", lwd = 2, lty = 2, add = TRUE)

# QQ plot pro test normality výnosů dynamického portfolia
stats::qqnorm(rpdyn_train, main="QQ Plot dynamického portfolia", ylab = "Empirické kvantily", xlab = "Teoretické kvantily", cex.lab=1.5, cex.main=1.8)
qqline(rpdyn_train)


# ------------------------------------------------------------------------------------------------------------------
# KONEC SCRIPTU PRO VYBER DAT, ZPRACOVANI DAT A EDA
# ------------------------------------------------------------------------------------------------------------------


