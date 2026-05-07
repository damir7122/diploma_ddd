getwd()
required <- c("quantmod", "vars", "tseries", "lmtest", "zoo")
to_install <- setdiff(required, rownames(installed.packages()))
if (length(to_install) > 0) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
}
invisible(lapply(required, library, character.only = TRUE))

set.seed(42)
dir.create("figures", showWarnings = FALSE)

TAB_BLUE   <- "#1F77B4"
TAB_ORANGE <- "#FF7F0E"
TAB_RED    <- "#D62728"

PNG_TYPE <- if (capabilities("aqua")) "quartz" else
            if (capabilities("cairo")) "cairo" else "Xlib"

open_png <- function(filename, width = 1600, height = 1200, dpi = 300) {
  png(file.path("figures", filename), width = width, height = height,
      res = dpi, family = "serif", type = PNG_TYPE, bg = "white")
}
mpl_par <- function(mfrow = c(1, 1), mar = c(4.5, 4.5, 3, 1)) {
  par(mfrow = mfrow, mar = mar, family = "serif", cex = 0.95,
      tcl = -0.3, mgp = c(2.6, 0.6, 0), bty = "l", las = 1)
}
draw_grid <- function() grid(col = "grey85", lty = 1, lwd = 0.5)

# ===== Data ================================================================
cat("Downloading Brent and ICLN from Yahoo Finance...\n")
getSymbols("BZ=F", src = "yahoo", from = "2013-01-01", to = "2021-12-31", auto.assign = TRUE)
getSymbols("ICLN", src = "yahoo", from = "2013-01-01", to = "2021-12-31", auto.assign = TRUE)

brent_raw <- Cl(`BZ=F`); colnames(brent_raw) <- "Brent"
icln_raw  <- Cl(ICLN);   colnames(icln_raw)  <- "ICLN"

all_days <- seq.Date(as.Date("2012-06-01"), as.Date("2021-12-31"), by = "day")
biz_days <- all_days[!format(all_days, "%u") %in% c("6", "7")]

ffill_biz <- function(x, biz_days) {
  out <- merge(x, xts(, biz_days), all = TRUE)
  out <- na.locf(out, na.rm = FALSE)
  out <- na.locf(out, fromLast = TRUE)
  out[biz_days]
}

prices <- merge(ffill_biz(brent_raw, biz_days), ffill_biz(icln_raw, biz_days), all = FALSE)
colnames(prices) <- c("Brent", "ICLN")
prices <- na.omit(prices)
returns <- na.omit(diff(log(prices)))

prices_p1  <- prices ["2013-01-01/2017-12-31"]
prices_p2  <- prices ["2019-01-01/2021-12-31"]
endog_p1   <- returns["2013-01-01/2017-12-31"][, c("Brent", "ICLN")]
endog_p2   <- returns["2019-01-01/2021-12-31"][, c("Brent", "ICLN")]

covid_p2 <- as.numeric(index(endog_p2) >= as.Date("2020-03-01") &
                       index(endog_p2) <= as.Date("2020-12-31"))
exo_p2 <- matrix(covid_p2, ncol = 1); colnames(exo_p2) <- "COVID"

cat(sprintf("\nP1 (2013-2017): N=%d, no exogen\n", nrow(endog_p1)))
cat(sprintf("P2 (2019-2021): N=%d, exogen=COVID dummy (active %d days)\n",
            nrow(endog_p2), sum(covid_p2)))

# ===== Lag selection
cat("\nAIC lag selection (lag.max=15)...\n")
sel_p1 <- VARselect(endog_p1, lag.max = 15, type = "const")
sel_p2 <- VARselect(endog_p2, lag.max = 15, type = "const", exogen = exo_p2)
aic_p1 <- as.integer(sel_p1$selection["AIC(n)"])
aic_p2 <- as.integer(sel_p2$selection["AIC(n)"])
selected_lag <- max(aic_p1, aic_p2)
cat(sprintf("  AIC P1=%d, AIC P2=%d, common lag=%d\n", aic_p1, aic_p2, selected_lag))
selected_p1 <- selected_lag
selected_p2 <- selected_lag

# ===== Estimate VARs
var_p1    <- VAR(endog_p1, p = selected_lag, type = "const")
var_p2    <- VAR(endog_p2, p = selected_lag, type = "const", exogen = exo_p2)
var_p2_nc <- VAR(endog_p2, p = selected_lag, type = "const")  # robustness

# ===== FIGURE 1, 2: time series ============================================
plot_ts_block <- function(prices_x, ret_x, title_main, title_ret, fname,
                          year_step) {
  open_png(fname, 1600, 1200)
  mpl_par(mfrow = c(2, 1), mar = c(3, 4.5, 2.5, 4.5))
  oil  <- as.numeric(prices_x$Brent); icln <- as.numeric(prices_x$ICLN)
  d    <- index(prices_x)
  plot(d, oil, type = "l", col = TAB_BLUE, lwd = 1, ylab = "", xlab = "",
       main = title_main, xaxt = "n", yaxt = "n")
  draw_grid()
  axis.Date(1, at = seq(min(d), max(d), by = year_step), format = "%Y")
  axis(2, col.axis = TAB_BLUE, col = TAB_BLUE)
  mtext("Oil Price (USD)", side = 2, line = 2.6, col = TAB_BLUE)
  par(new = TRUE)
  plot(d, icln, type = "l", col = TAB_ORANGE, lwd = 1,
       axes = FALSE, xlab = "", ylab = "")
  axis(4, col.axis = TAB_ORANGE, col = TAB_ORANGE)
  mtext("ICLN (USD)", side = 4, line = 2.6, col = TAB_ORANGE)
  legend("topleft",  legend = "Brent", col = TAB_BLUE,   lty = 1, bty = "n")
  legend("topright", legend = "ICLN",  col = TAB_ORANGE, lty = 1, bty = "n")
  rb <- as.numeric(ret_x$Brent); ri <- as.numeric(ret_x$ICLN); dr <- index(ret_x)
  plot(dr, rb, type = "l", col = TAB_BLUE, lwd = 0.5,
       ylab = "Log Returns", xlab = "", main = title_ret, xaxt = "n")
  draw_grid()
  abline(h = 0, col = "black", lwd = 0.6)
  lines(dr, ri, col = TAB_ORANGE, lwd = 0.5)
  axis.Date(1, at = seq(min(dr), max(dr), by = year_step), format = "%Y")
  legend("topright", legend = c("Oil", "ICLN"),
         col = c(TAB_BLUE, TAB_ORANGE), lty = 1, bty = "n", cex = 0.85)
  dev.off()
}
plot_ts_block(prices_p1, endog_p1, "Time Series 2013-2017", "Log Returns 2013-2017",
              "fig01_main_timeseries_2013_2017.png", "year")
plot_ts_block(prices_p2, endog_p2, "Time Series 2019-2021", "Log Returns 2019-2021",
              "fig02_main_timeseries_2019_2021.png", "6 months")

# ===== FIGURE 3: IRF 
cat("Computing IRFs (boot, 200 runs)...\n")
n_ahead <- 20; n_runs <- 200
irf_p1_o2i <- irf(var_p1, impulse = "Brent", response = "ICLN", n.ahead = n_ahead, boot = TRUE, ci = 0.95, runs = n_runs)
irf_p1_i2o <- irf(var_p1, impulse = "ICLN",  response = "Brent", n.ahead = n_ahead, boot = TRUE, ci = 0.95, runs = n_runs)
irf_p2_o2i <- irf(var_p2, impulse = "Brent", response = "ICLN", n.ahead = n_ahead, boot = TRUE, ci = 0.95, runs = n_runs)
irf_p2_i2o <- irf(var_p2, impulse = "ICLN",  response = "Brent", n.ahead = n_ahead, boot = TRUE, ci = 0.95, runs = n_runs)

draw_irf_panel <- function(x, impulse, title) {
  pt <- as.numeric(x$irf[[impulse]])
  lo <- as.numeric(x$Lower[[impulse]]); hi <- as.numeric(x$Upper[[impulse]])
  h <- 1:length(pt)
  plot(h, pt, type = "n", ylim = range(c(lo, hi)),
       main = title, xlab = "Periods", ylab = "")
  draw_grid()
  polygon(c(h, rev(h)), c(lo, rev(hi)),
          col = adjustcolor(TAB_RED, alpha.f = 0.15), border = NA)
  lines(h, lo, col = TAB_RED, lty = 2); lines(h, hi, col = TAB_RED, lty = 2)
  abline(h = 0, col = "grey40", lwd = 0.6)
  lines(h, pt, col = "black", lwd = 1.6)
  legend("bottomleft", legend = "95% Bootstrap CI", text.col = "grey50", bty = "n", cex = 0.75)
}
open_png("fig03_main_irf.png", 1800, 1200)
mpl_par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
draw_irf_panel(irf_p1_o2i, "Brent", "Oil->ICLN 2013-17")
draw_irf_panel(irf_p1_i2o, "ICLN",  "ICLN->Oil 2013-17")
draw_irf_panel(irf_p2_o2i, "Brent", "Oil->ICLN 2019-21")
draw_irf_panel(irf_p2_i2o, "ICLN",  "ICLN->Oil 2019-21")
dev.off()

# ===== FIGURE 4: cumulative IRF
cum_irf_p1 <- irf(var_p1, impulse = "Brent", response = "ICLN", n.ahead = n_ahead, cumulative = TRUE, boot = FALSE)
cum_irf_p2 <- irf(var_p2, impulse = "Brent", response = "ICLN", n.ahead = n_ahead, cumulative = TRUE, boot = FALSE)
cum_p1 <- as.numeric(cum_irf_p1$irf$Brent); cum_p2 <- as.numeric(cum_irf_p2$irf$Brent)
hseq <- 1:length(cum_p1)
open_png("fig04_main_cumulative_irf.png", 1800, 700)
mpl_par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
for (i in 1:2) {
  cum <- if (i == 1) cum_p1 else cum_p2
  col <- if (i == 1) TAB_BLUE else TAB_ORANGE
  ttl <- if (i == 1) "Cumulative IRF: Oil->ICLN (2013-2017)" else "Cumulative IRF: Oil->ICLN (2019-2021)"
  plot(hseq, cum, type = "n", main = ttl, xlab = "Periods", ylab = "",
       ylim = c(min(c(cum, 0)) * 1.05, max(c(cum, 0)) * 1.05))
  draw_grid()
  polygon(c(hseq, rev(hseq)), c(cum, rep(0, length(cum))),
          col = adjustcolor(col, alpha.f = 0.2), border = NA)
  lines(hseq, cum, col = col, lwd = 1.5); points(hseq, cum, col = col, pch = 16, cex = 0.7)
  abline(h = 0, col = "grey40", lwd = 0.6)
}
dev.off()

# ===== FIGURE 5: FEVD ======================================================
icln_fevd_p1 <- fevd(var_p1, n.ahead = 5)$ICLN
icln_fevd_p2 <- fevd(var_p2, n.ahead = 5)$ICLN
draw_fevd <- function(m, title) {
  bd <- t(m[, c("ICLN", "Brent")])
  barplot(bd, names.arg = 1:nrow(m), ylim = c(0, 1),
          col = c(TAB_ORANGE, TAB_BLUE), border = "white",
          main = title, xlab = "Forecast Horizon", ylab = "Proportion", space = 0.4)
  legend("bottom", legend = c("ICLN", "Oil"),
         fill = c(TAB_ORANGE, TAB_BLUE), border = NA, bty = "n", horiz = TRUE, cex = 0.85)
}
open_png("fig05_main_fevd.png", 1800, 700)
mpl_par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
draw_fevd(icln_fevd_p1, "FEVD of ICLN (2013-17)")
draw_fevd(icln_fevd_p2, "FEVD of ICLN (2019-21)")
dev.off()

# ===== Appendix figures (A1 hist, A2 scatter, A3 rolling, A4 IC, A6 ACF, A7 CUSUM) ===
draw_hist <- function(x, title, fc) {
  x <- as.numeric(x); h <- hist(x, breaks = 50, plot = FALSE)
  plot(h, col = fc, border = "white", main = title, xlab = "", ylab = "")
  draw_grid(); plot(h, col = fc, border = "white", add = TRUE)
  xs <- seq(min(x), max(x), length.out = 200)
  bw <- diff(h$breaks)[1]
  lines(xs, dnorm(xs, mean(x), sd(x)) * length(x) * bw, col = "black", lty = 2, lwd = 1.3)
  legend("topright", legend = "Normal", col = "black", lty = 2, bty = "n", cex = 0.85)
}
open_png("fig06_appA1_distribution.png", 1800, 1200)
mpl_par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
draw_hist(endog_p1$Brent, "Distribution: Oil 2013-17",  TAB_BLUE)
draw_hist(endog_p1$ICLN,  "Distribution: ICLN 2013-17", TAB_ORANGE)
draw_hist(endog_p2$Brent, "Distribution: Oil 2019-21",  TAB_BLUE)
draw_hist(endog_p2$ICLN,  "Distribution: ICLN 2019-21", TAB_ORANGE)
dev.off()

draw_scatter <- function(x, y, title, col) {
  x <- as.numeric(x); y <- as.numeric(y); r <- cor(x, y)
  plot(x, y, pch = 16, col = adjustcolor(col, alpha.f = 0.5), cex = 0.6,
       main = title, xlab = "Oil Log Returns", ylab = "ICLN Log Returns")
  draw_grid(); points(x, y, pch = 16, col = adjustcolor(col, alpha.f = 0.5), cex = 0.6)
  abline(lm(y ~ x), col = TAB_RED, lwd = 1.6)
  legend("topleft", legend = sprintf("r = %.4f", r), bty = "n", cex = 0.95)
}
open_png("fig07_appA2_correlation.png", 1800, 700)
mpl_par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
draw_scatter(endog_p1$Brent, endog_p1$ICLN, "Correlation (2013-2017)", TAB_BLUE)
draw_scatter(endog_p2$Brent, endog_p2$ICLN, "Correlation (2019-2021)", TAB_ORANGE)
dev.off()

rolling_corr <- function(x, y, win = 60) {
  out <- rep(NA, length(x))
  for (i in win:length(x)) out[i] <- cor(x[(i - win + 1):i], y[(i - win + 1):i])
  out
}
rc1 <- rolling_corr(as.numeric(endog_p1$Brent), as.numeric(endog_p1$ICLN), 60)
rc2 <- rolling_corr(as.numeric(endog_p2$Brent), as.numeric(endog_p2$ICLN), 60)
open_png("fig08_appA3_rolling_corr.png", 1800, 1200)
mpl_par(mfrow = c(2, 1), mar = c(3.5, 4.5, 2.5, 1))
for (i in 1:2) {
  rc  <- if (i == 1) rc1 else rc2; col <- if (i == 1) TAB_BLUE else TAB_ORANGE
  ttl <- if (i == 1) "60-Day Rolling Correlation (2013-2017)" else "60-Day Rolling Correlation (2019-2021)"
  d   <- if (i == 1) index(endog_p1) else index(endog_p2)
  plot(d, rc, type = "l", col = col, lwd = 1, ylab = "Correlation", xlab = "",
       main = ttl, xaxt = "n")
  draw_grid(); abline(h = 0, col = "grey20", lwd = 0.5)
  polygon(c(d, rev(d)), c(rc, rep(0, length(rc))),
          col = adjustcolor(col, alpha.f = 0.2), border = NA)
  lines(d, rc, col = col, lwd = 1)
  axis.Date(1, at = seq(min(d), max(d), by = if (i == 1) "year" else "3 months"), format = "%Y")
}
dev.off()

compute_ic <- function(ret_xts, exog = NULL, lags = 1:15) {
  rd <- as.matrix(ret_xts); n <- nrow(rd); k <- ncol(rd)
  aic <- numeric(length(lags)); bic <- numeric(length(lags))
  for (i in seq_along(lags)) {
    p <- lags[i]
    fit <- if (is.null(exog)) VAR(rd, p = p, type = "const")
           else VAR(rd, p = p, type = "const", exogen = exog)
    sig <- crossprod(resid(fit)) / (n - p)
    ld  <- log(det(sig)); npar <- k * (k * p + 1)
    aic[i] <- ld + 2 * npar / (n - p)
    bic[i] <- ld + log(n - p) * npar / (n - p)
  }
  list(aic = aic, bic = bic, lags = lags)
}
ic_p1 <- compute_ic(endog_p1, NULL,   1:15)
ic_p2 <- compute_ic(endog_p2, exo_p2, 1:15)
open_png("fig09_appA4_information_criteria.png", 1800, 700)
mpl_par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
for (i in 1:2) {
  ic  <- if (i == 1) ic_p1 else ic_p2
  ttl <- if (i == 1) "Information Criteria (2013-2017)" else "Information Criteria (2019-2021)"
  plot(ic$lags, ic$aic, type = "b", col = TAB_BLUE, pch = 16, lwd = 1.5,
       ylim = range(c(ic$aic, ic$bic)),
       main = ttl, xlab = "Lag Order", ylab = "")
  draw_grid()
  lines(ic$lags, ic$bic, type = "b", col = TAB_ORANGE, pch = 15, lwd = 1.5, lty = 2)
  abline(v = selected_lag, col = TAB_RED, lty = 3, lwd = 1.2)
  legend("topright", legend = c("AIC", "BIC", sprintf("Common p=%d", selected_lag)),
         col = c(TAB_BLUE, TAB_ORANGE, TAB_RED),
         lty = c(1, 2, 3), pch = c(16, 15, NA), bty = "n", cex = 0.85)
}
dev.off()

draw_acf <- function(x, title, col) {
  x <- as.numeric(x); a <- acf(x, lag.max = 20, plot = FALSE)
  ci <- 1.96 / sqrt(length(x))
  plot(a$lag, a$acf, type = "h", lwd = 8, col = col, lend = 1,
       ylim = c(-0.15, 1), main = title, xlab = "", ylab = "")
  draw_grid(); segments(a$lag, 0, a$lag, a$acf, lwd = 8, col = col, lend = 1)
  abline(h = 0, col = "black", lwd = 0.6)
  abline(h =  ci, col = TAB_RED, lty = 2); abline(h = -ci, col = TAB_RED, lty = 2)
}
res_p1 <- residuals(var_p1); res_p2 <- residuals(var_p2)
open_png("fig10_appA6_acf.png", 1800, 1200)
mpl_par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
draw_acf(res_p1[, "Brent"], "ACF resid Oil 2013-17",  TAB_BLUE)
draw_acf(res_p1[, "ICLN"],  "ACF resid ICLN 2013-17", TAB_ORANGE)
draw_acf(res_p2[, "Brent"], "ACF resid Oil 2019-21",  TAB_BLUE)
draw_acf(res_p2[, "ICLN"],  "ACF resid ICLN 2019-21", TAB_ORANGE)
dev.off()

stab_p1 <- stability(var_p1, type = "OLS-CUSUM")
stab_p2 <- stability(var_p2, type = "OLS-CUSUM")
draw_cusum <- function(s, vname, title) {
  proc <- s$stability[[vname]]$process
  n <- length(proc); xs <- seq(0, 1, length.out = n)
  upper <- 0.948 + 1.896 * xs; lower <- -upper
  plot(xs, proc, type = "l", col = "black", lwd = 1,
       ylim = c(min(c(lower, proc)) - 0.1, max(c(upper, proc)) + 0.1),
       main = title, xlab = "", ylab = "")
  draw_grid(); abline(h = 0, col = "black", lwd = 0.4)
  lines(xs, upper, col = TAB_RED, lwd = 1.2)
  lines(xs, lower, col = TAB_RED, lwd = 1.2)
  lines(xs, proc, col = "black", lwd = 1)
}
open_png("fig11_appA7_olscusum.png", 1800, 1200)
mpl_par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
draw_cusum(stab_p1, "Brent", "OLS-CUSUM: Oil (2013-2017)")
draw_cusum(stab_p1, "ICLN",  "OLS-CUSUM: ICLN (2013-2017)")
draw_cusum(stab_p2, "Brent", "OLS-CUSUM: Oil (2019-2021)")
draw_cusum(stab_p2, "ICLN",  "OLS-CUSUM: ICLN (2019-2021)")
dev.off()

# ===== Numbers dump 
sink("numbers_for_thesis_v2.txt")

cat("================================================================\n")
cat("  THESIS NUMBERS — bivariate VAR with COVID dummy in P2\n")
cat("  Generated:", as.character(Sys.time()), "\n")
cat("================================================================\n\n")

cat("### 0. SPECIFICATION ###\n")
cat("Endogenous: Brent log-returns, ICLN log-returns\n")
cat("Exogen P1:  none\n")
cat("Exogen P2:  COVID dummy (1 for 2020-03-01..2020-12-31)\n")
cat("Cholesky:   Brent first, ICLN second\n")
cat(sprintf("AIC P1=%d, AIC P2=%d, common lag=%d\n\n", aic_p1, aic_p2, selected_lag))

cat("### 1. SAMPLE SIZES ###\n")
cat(sprintf("P1 prices N=%d   returns N=%d\n", nrow(prices_p1), nrow(endog_p1)))
cat(sprintf("P2 prices N=%d   returns N=%d\n", nrow(prices_p2), nrow(endog_p2)))
cat(sprintf("COVID active: %d / %d (%.1f%%)\n\n",
            sum(covid_p2), length(covid_p2), 100 * sum(covid_p2) / length(covid_p2)))

cat("### 2. DESCRIPTIVE STATS - LEVELS (Table 1) ###\n")
desc_lvl <- function(x, lab) {
  x <- as.numeric(x); m <- mean(x); s <- sd(x); n <- length(x)
  sk <- (sum((x - m)^3) / n) / (s^3)
  cat(sprintf("  %-18s N=%4d mean=%8.3f sd=%7.3f min=%8.3f max=%8.3f skew=%6.3f\n",
              lab, n, m, s, min(x), max(x), sk))
}
desc_lvl(prices_p1$Brent, "Oil 2013-17")
desc_lvl(prices_p1$ICLN,  "ICLN 2013-17")
desc_lvl(prices_p2$Brent, "Oil 2019-21")
desc_lvl(prices_p2$ICLN,  "ICLN 2019-21")
cat("\n")

cat("### 3. DESCRIPTIVE STATS - LOG RETURNS (Table 2) ###\n")
desc_ret <- function(x, lab) {
  x <- as.numeric(x); m <- mean(x); s <- sd(x); n <- length(x)
  ku <- (sum((x - m)^4) / n) / (s^4) - 3
  cat(sprintf("  %-18s N=%4d mean=%9.6f sd=%7.4f min=%8.4f max=%8.4f exc.kurt=%7.3f\n",
              lab, n, m, s, min(x), max(x), ku))
}
desc_ret(endog_p1$Brent, "Oil 2013-17")
desc_ret(endog_p1$ICLN,  "ICLN 2013-17")
desc_ret(endog_p2$Brent, "Oil 2019-21")
desc_ret(endog_p2$ICLN,  "ICLN 2019-21")
cat("\n")

cat("### 4. CORRELATIONS ###\n")
cat(sprintf("P1 r(Brent,ICLN) = %.4f\n", cor(as.numeric(endog_p1$Brent), as.numeric(endog_p1$ICLN))))
cat(sprintf("P2 r(Brent,ICLN) = %.4f\n\n", cor(as.numeric(endog_p2$Brent), as.numeric(endog_p2$ICLN))))

cat("### 5. ADF ###\n")
adf_dump <- function(x, lab) {
  r <- suppressWarnings(adf.test(as.numeric(x), alternative = "stationary"))
  cat(sprintf("  %-22s ADF=%7.3f p=%.4f\n", lab, r$statistic, r$p.value))
}
cat("On levels:\n")
adf_dump(prices_p1$Brent, "Oil P1 (level)")
adf_dump(prices_p1$ICLN,  "ICLN P1 (level)")
adf_dump(prices_p2$Brent, "Oil P2 (level)")
adf_dump(prices_p2$ICLN,  "ICLN P2 (level)")
cat("On returns:\n")
adf_dump(endog_p1$Brent, "Oil P1 (returns)")
adf_dump(endog_p1$ICLN,  "ICLN P1 (returns)")
adf_dump(endog_p2$Brent, "Oil P2 (returns)")
adf_dump(endog_p2$ICLN,  "ICLN P2 (returns)")
cat("\n")

dump_var_eq <- function(v, eq, lab) {
  cat(sprintf("--- %s :: %s ---\n", lab, eq))
  s <- summary(v)$varresult[[eq]]
  co <- coef(s)
  for (i in seq_len(nrow(co))) {
    cat(sprintf("  %-12s est=%9.5f se=%8.5f t=%7.3f p=%.4f\n",
                rownames(co)[i], co[i, 1], co[i, 2], co[i, 3], co[i, 4]))
  }
  cat(sprintf("  F=%7.3f on (%d,%d) p=%.4g  R2=%.4f  AdjR2=%.4f\n",
              s$fstatistic[1], s$fstatistic[2], s$fstatistic[3],
              pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE),
              s$r.squared, s$adj.r.squared))
}
cat("### 6. VAR P1 (Table 3) ###\n")
dump_var_eq(var_p1, "Brent", "P1"); cat("\n")
dump_var_eq(var_p1, "ICLN",  "P1"); cat("\n")
cat("### 7. VAR P2 (Table 4, with COVID) ###\n")
dump_var_eq(var_p2, "Brent", "P2 main"); cat("\n")
dump_var_eq(var_p2, "ICLN",  "P2 main"); cat("\n")
cat("### 8. ROBUSTNESS — P2 without COVID dummy ###\n")
s_nc <- summary(var_p2_nc)$varresult
cat(sprintf("  Brent eq AdjR2 = %.4f\n", s_nc$Brent$adj.r.squared))
cat(sprintf("  ICLN eq  AdjR2 = %.4f\n\n", s_nc$ICLN$adj.r.squared))

cat("### 9. GRANGER CAUSALITY (Wald) ###\n")
gc_dump <- function(v, cause, lab) {
  r <- causality(v, cause = cause)$Granger
  cat(sprintf("  %-30s F=%7.3f df=(%d,%d) p=%.4f\n",
              lab, r$statistic, r$parameter[1], r$parameter[2], r$p.value))
}
gc_dump(var_p1, "Brent", "P1: Oil->ICLN")
gc_dump(var_p1, "ICLN",  "P1: ICLN->Oil")
gc_dump(var_p2, "Brent", "P2: Oil->ICLN")
gc_dump(var_p2, "ICLN",  "P2: ICLN->Oil")
cat("Robustness P2 NC:\n")
gc_dump(var_p2_nc, "Brent", "P2 NC: Oil->ICLN")
gc_dump(var_p2_nc, "ICLN",  "P2 NC: ICLN->Oil")
cat("\n")

cat("### 10. DIAGNOSTICS (Table 6) ###\n")
diag_dump <- function(v, lab) {
  cat(sprintf("--- %s ---\n", lab))
  pt <- serial.test(v, type = "PT.adjusted")
  cat(sprintf("  Portmanteau adj: chi2=%.3f df=%d p=%.4f\n",
              pt$serial$statistic, pt$serial$parameter, pt$serial$p.value))
  ar <- arch.test(v, multivariate.only = TRUE)
  cat(sprintf("  ARCH-LM mv:      chi2=%.3f df=%d p=%.4g\n",
              ar$arch.mul$statistic, ar$arch.mul$parameter, ar$arch.mul$p.value))
  jb <- normality.test(v, multivariate.only = TRUE)
  cat(sprintf("  JB mv:           chi2=%.3f df=%d p=%.4g\n",
              jb$jb.mul$JB$statistic, jb$jb.mul$JB$parameter, jb$jb.mul$JB$p.value))
  res <- residuals(v)
  for (col in colnames(res)) {
    lb <- Box.test(res[, col], lag = 10, type = "Ljung-Box")
    cat(sprintf("  Ljung-Box (10) %-5s: Q=%.3f p=%.4g\n", col, lb$statistic, lb$p.value))
  }
}
diag_dump(var_p1, "P1")
cat("\n")
diag_dump(var_p2, "P2 main (with COVID)")
cat("\n")

cat("### 11. CUMULATIVE IRF ###\n")
cat(sprintf("P1 peak = %.5f\n", max(cum_p1)))
cat(sprintf("P2 peak = %.5f\n", max(cum_p2)))
cat(sprintf("ratio   = %.2f\n", max(cum_p2) / max(cum_p1)))
cat(sprintf("P1 final h=20 = %.5f\n", cum_p1[length(cum_p1)]))
cat(sprintf("P2 final h=20 = %.5f\n\n", cum_p2[length(cum_p2)]))
cat("Full P1:\n"); print(round(cum_p1, 5))
cat("\nFull P2:\n"); print(round(cum_p2, 5)); cat("\n")

cat("### 12. ROBUSTNESS — P2 without COVID dummy ###\n")
cum_irf_p2_nc <- irf(var_p2_nc, impulse = "Brent", response = "ICLN",
                     n.ahead = n_ahead, cumulative = TRUE, boot = FALSE)
cum_p2_nc <- as.numeric(cum_irf_p2_nc$irf$Brent)
cat(sprintf("P2 NC peak = %.5f  (main=%.5f)\n", max(cum_p2_nc), max(cum_p2)))
cat(sprintf("P2/P1 NC   = %.2f  (main=%.2f)\n",
            max(cum_p2_nc) / max(cum_p1), max(cum_p2) / max(cum_p1))); cat("\n")

cat("### 13. FEVD ICLN (Tables 7, 8) ###\n")
cat("P1 main:\n"); print(round(icln_fevd_p1, 4))
cat("\nP2 main:\n"); print(round(icln_fevd_p2, 4))
cat("\nP2 NC (robustness):\n"); print(round(fevd(var_p2_nc, n.ahead = 5)$ICLN, 4))
cat("\n")

cat("### 14. AIC/BIC scan (1..15) ###\n")
ic_scan <- function(o, lab) {
  cat(sprintf("--- %s ---\n", lab))
  for (i in seq_along(o$lags)) cat(sprintf("  p=%2d AIC=%9.4f BIC=%9.4f\n", o$lags[i], o$aic[i], o$bic[i]))
}
ic_scan(ic_p1, "P1")
cat("\n")
ic_scan(ic_p2, "P2 (with COVID)")

cat("\n================================================================\n")
cat("  END\n================================================================\n")

sink()

cat("\nAll figures + numbers_for_thesis_v2.txt written.\n")
