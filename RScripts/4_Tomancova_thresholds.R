# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# RSCRIPT PRO APLIKACNI CAST PRACE - VOLBA PRAHU u
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------

# nacteni pckgs
library(gridExtra)
library(POT)

# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# VOLBA PRAHU u - POT PRO KONZERVATIVNI PORTFOLIO
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# funkce pro výpočet n_u
# ------------------------------------------------------------------------------------------------------------------
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
mean_exc_ci_f(.35, rpc)  # Získání hodnot pro volbu prahu .35
mean_exc_ci_f(.65, rpc)  # Získání hodnot pro volbu prahu .65
mean_exc_ci_f(.85, rpc)  # Získání hodnot pro volbu prahu .85
mean_exc_ci_f(1.6, rpc)  # Získání hodnot pro volbu prahu 1.6

# ------------------------------------------------------------------------------------------------------------------
# graf stredni hodnoty excesu
# ------------------------------------------------------------------------------------------------------------------
mean_exc_ci_f_plot <- function(u, data) {
  exceedances <- data[data > u] - u
  alpha <- 0.05
  sd_excess <- sd(exceedances)
  mean_excess <- mean(exceedances)
  n <- length(exceedances)
  z_value <- qnorm(1-alpha/2)  # Kritická hodnota normálního rozdělení
  ci_lower <- mean_excess - z_value * sd_excess / sqrt(n)
  ci_upper <- mean_excess + z_value * sd_excess / sqrt(n)
  list(e_u = mean_excess, e_lower = ci_lower, e_upper = ci_upper,
       n_u = n, th = u)
}

thresholds <- seq(0, 1.6, length.out = 100)  # Rozsah prahů
results <- lapply(thresholds, function(x) mean_exc_ci_f_plot(x,rpc))
e_u <- sapply(results, function(x) x$e_u)
e_lower <- sapply(results, function(x) x$e_lower)
e_upper <- sapply(results, function(x) x$e_upper)
n_u <- sapply(results, function(x) x$n_u)
mrlplot_data <- data.frame(Threshold = thresholds, MeanExcess = e_u, Lower = e_lower, Upper = e_upper, n_u = n_u)

# Vykreslení grafu pomocí ggplot2
ggplot(mrlplot_data, aes(x = Threshold, y = MeanExcess)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray80", alpha = 0.5) +
  geom_vline(xintercept = c(.35, .65, .85), color = "red", linetype = "dashed") + # Vertikální přímka
  scale_x_continuous(sec.axis = sec_axis(trans = ~ ., name = expression(n[u]), 
                                         breaks = mrlplot_data$Threshold[sapply(seq(0,1.5,.25), function(x) which.min(abs(mrlplot_data$Threshold - x)))], 
                                         labels = mrlplot_data$n_u[sapply(seq(0,1.5,.25), function(x) which.min(abs(mrlplot_data$Threshold - x)))])) +
  labs(x = "u", y = expression(hat(e)(u))) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title.y = element_text(size = 12, angle = 0, vjust = .5),
    axis.title.x = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.text.x.top = element_text(size = 10), # Text pro horní osu
    axis.title.x.top = element_text(size = 12) # Titulek horní osy
  )



# ------------------------------------------------------------------------------------------------------------------
# grafy zavislosti odhadu xi a sigma* na u
# ------------------------------------------------------------------------------------------------------------------
# Funkce pro odhad parametrů GP nad prahem
fit_gpd <- function(data, threshold) {
  fit <- fevd(data, threshold = threshold, type = "GP")
  params <- fit$results$par
  conf_int <- ci(fit, type = "parameter")
  list(scale = params["scale"], scale_lower = conf_int[1, 1], scale_upper = conf_int[1, 3],
       shape = params["shape"], shape_lower = conf_int[2, 1], shape_upper = conf_int[2, 3], th = threshold)
}

thresholds <- seq(0, 1.3, length.out = 100)  # Rozsah prahů

# Výpočet parametrů a intervalů spolehlivosti pro každý práh
results <- lapply(thresholds, function(th) fit_gpd(rpc, th))
scale2_params <- sapply(results, function(x) x$scale-(x$shape*x$th))
scale2_lower <- sapply(results, function(x) x$scale_lower-(x$shape*x$th))
scale2_upper <- sapply(results, function(x) x$scale_upper-(x$shape*x$th))
shape_params <- sapply(results, function(x) x$shape)
shape_lower <- sapply(results, function(x) x$shape_lower)
shape_upper <- sapply(results, function(x) x$shape_upper)

# Data pro ggplot
data_scale <- data.frame(
  Threshold = thresholds,
  Estimate = scale2_params,
  Lower = scale2_lower,
  Upper = scale2_upper
)

data_shape <- data.frame(
  Threshold = thresholds,
  Estimate = shape_params,
  Lower = shape_lower,
  Upper = shape_upper
)

# Graf pro škálovací parametr
plot_scale <- ggplot(data_scale, aes(x = Threshold, y = Estimate)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray80", alpha = 0.5) +
  geom_vline(xintercept = c(.35, .65, .85), color = "red", linetype = "dashed") + # Vertikální přímka
  labs(x = "u", y = expression(hat(sigma)~"*")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title.y = element_text(size = 12, angle = 0, vjust = .5),
    axis.title.x = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Graf pro tvarový parametr
plot_shape <- ggplot(data_shape, aes(x = Threshold, y = Estimate)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray80", alpha = 0.5) +
  geom_vline(xintercept = c(.35, .65, .85), color = "red", linetype = "dashed") + # Vertikální přímka
  labs(title = "Konzervativní portfolio - GP", x = "u", y = expression(hat(xi)~" ")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title.y = element_text(size = 12, angle = 0, vjust = .5),
    axis.title.x = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Zobrazení grafů vedle sebe
grid.arrange(plot_shape, plot_scale, ncol = 1)


# ------------------------------------------------------------------------------------------------------------------
# L-Moments Plot
# ------------------------------------------------------------------------------------------------------------------
par(mfrow = c(1,2))
lmomplot(as.vector(rpc), u.range = c(0,1.6), nt = 50, identify = FALSE, xlim=c(0.3,.6), ylim = c(.2,.5))
lmomplot(as.vector(rpc), u.range = c(.35,.85), nt = 50, identify = FALSE, xlim=c(.45,.6),ylim=c(.3,.4))


# ------------------------------------------------------------------------------------------------------------------
# finalni volba prahu
# ------------------------------------------------------------------------------------------------------------------
u1c <- .65 
u2c <- .85



# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# VOLBA PRAHU u - POT PRO DYNAMICKE PORTFOLIO
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# výpočet n_u
# ------------------------------------------------------------------------------------------------------------------
mean_exc_ci_f(1.9, rpd)  # Získání hodnot pro volbu prahu 1.9
mean_exc_ci_f(2.35, rpd)  # Získání hodnot pro volbu prahu 2.35
mean_exc_ci_f(3.05, rpd)  # Získání hodnot pro volbu prahu 3.05
mean_exc_ci_f(4.25, rpd)  # Získání hodnot pro volbu prahu 4.25

# ------------------------------------------------------------------------------------------------------------------
# graf stredni hodnoty excesu
# ------------------------------------------------------------------------------------------------------------------
thresholds <- seq(0, 4.25, length.out = 100)  # Rozsah prahů

results <- lapply(thresholds, function(x) mean_exc_ci_f_plot(x,rpd))
e_u <- sapply(results, function(x) x$e_u)
e_lower <- sapply(results, function(x) x$e_lower)
e_upper <- sapply(results, function(x) x$e_upper)
n_u <- sapply(results, function(x) x$n_u)
mrlplot_data <- data.frame(Threshold = thresholds, MeanExcess = e_u, Lower = e_lower, Upper = e_upper, n_u = n_u)

# Vykreslení grafu pomocí ggplot2
ggplot(mrlplot_data, aes(x = Threshold, y = MeanExcess)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray80", alpha = 0.5) +
  geom_vline(xintercept = c(1.9, 2.35, 3.05), color = "red", linetype = "dashed") + # Vertikální přímka
  scale_x_continuous(sec.axis = sec_axis(trans = ~ ., name = expression(n[u]), 
                                         breaks = mrlplot_data$Threshold[sapply(seq(0,4,.5), function(x) which.min(abs(mrlplot_data$Threshold - x)))], 
                                         labels = mrlplot_data$n_u[sapply(seq(0,4,.5), function(x) which.min(abs(mrlplot_data$Threshold - x)))])) +
  labs(x = "u", y = expression(hat(e)(u))) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title.y = element_text(size = 12, angle = 0, vjust = .5),
    axis.title.x = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.text.x.top = element_text(size = 10), # Text pro horní osu
    axis.title.x.top = element_text(size = 12) # Titulek horní osy
  )


# ------------------------------------------------------------------------------------------------------------------
# grafy zavislosti odhadu xi a sigma* na u
# ------------------------------------------------------------------------------------------------------------------
thresholds <- seq(0.5, 3.75, length.out = 100)  # Rozsah prahů

# Výpočet parametrů a intervalů spolehlivosti pro každý práh
results <- lapply(thresholds, function(th) fit_gpd(rpd, th))
scale2_params <- sapply(results, function(x) x$scale-(x$shape*x$th))
scale2_lower <- sapply(results, function(x) x$scale_lower-(x$shape*x$th))
scale2_upper <- sapply(results, function(x) x$scale_upper-(x$shape*x$th))
shape_params <- sapply(results, function(x) x$shape)
shape_lower <- sapply(results, function(x) x$shape_lower)
shape_upper <- sapply(results, function(x) x$shape_upper)

# Data pro ggplot
data_scale <- data.frame(
  Threshold = thresholds,
  Estimate = scale2_params,
  Lower = scale2_lower,
  Upper = scale2_upper
)

data_shape <- data.frame(
  Threshold = thresholds,
  Estimate = shape_params,
  Lower = shape_lower,
  Upper = shape_upper
)

# Graf pro škálovací parametr
plot_scale <- ggplot(data_scale, aes(x = Threshold, y = Estimate)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray80", alpha = 0.5) +
  geom_vline(xintercept = c(1.9, 2.35, 3.05), color = "red", linetype = "dashed") + # Vertikální přímka
  labs(x = "u", y = expression(hat(sigma)~"*")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title.y = element_text(size = 12, angle = 0, vjust = .5),
    axis.title.x = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Graf pro tvarový parametr
plot_shape <- ggplot(data_shape, aes(x = Threshold, y = Estimate)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray80", alpha = 0.5) +
  geom_vline(xintercept = c(1.9, 2.35, 3.05), color = "red", linetype = "dashed") + # Vertikální přímka
  labs(title = "Dynamické portfolio - GP", x = "u", y = expression(hat(xi)~" ")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title.y = element_text(size = 12, angle = 0, vjust = .5),
    axis.title.x = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Zobrazení grafů vedle sebe
grid.arrange(plot_shape, plot_scale, ncol = 1)


# ------------------------------------------------------------------------------------------------------------------
# L-Moments Plot
# ------------------------------------------------------------------------------------------------------------------
par(mfrow = c(1,2))
lmomplot(as.vector(rpd), u.range = c(0,4.25), nt = 50, identify = FALSE, xlim=c(.35,.85), ylim = c(.2,.7))
lmomplot(as.vector(rpd), u.range = c(2.35,3.35), nt = 50, identify = FALSE, xlim=c(.52,.66), ylim=c(.41,.5))


# ------------------------------------------------------------------------------------------------------------------
# finalni volba prahu
# ------------------------------------------------------------------------------------------------------------------
u1d <- 2.35 
u2d <- 3.05


# ------------------------------------------------------------------------------------------------------------------
# KONEC SCRIPTU PRO VOLBU PRAHU u
# ------------------------------------------------------------------------------------------------------------------



