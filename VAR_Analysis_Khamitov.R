# ═══════════════════════════════════════════════════════════════════════
# The Dynamic Relationship between Global Oil Price Shocks
# and Renewable Energy Market Returns:
# Evidence from a Vector Autoregression Model
#
# Author: Damir Khamitov
# Tallinn University of Technology, 2026
# ═══════════════════════════════════════════════════════════════════════

# ── 1. Install & Load Packages ──────────────────────────────────────────

required <- c("quantmod", "xts", "vars", "tseries", "pastecs", "lmtest", "zoo")
new_pkg  <- required[!(required %in% installed.packages()[, "Package"])]
if (length(new_pkg)) install.packages(new_pkg)

library(quantmod)
library(xts)
library(vars)
library(tseries)
library(pastecs)
library(lmtest)
library(zoo)

cat("All libraries loaded.\n\n")

# ── 2. Download Data ────────────────────────────────────────────────────

cat("Downloading data from Yahoo Finance...\n")

getSymbols("ICLN", src = "yahoo", from = "2013-01-01", to = "2021-12-31", auto.assign = TRUE)
getSymbols("BZ=F", src = "yahoo", from = "2013-01-01", to = "2021-12-31", auto.assign = TRUE)

icln_price <- Ad(ICLN)
oil_price  <- Ad(`BZ=F`)

merged <- merge(oil_price, icln_price, join = "inner")
colnames(merged) <- c("Oil", "ICLN")
merged <- na.omit(merged)

cat("Total observations:", nrow(merged), "\n")

# ── 3. Split into Two Periods ───────────────────────────────────────────

data1 <- merged["2013-01-01/2017-12-31"]
data2 <- merged["2019-01-01/2021-12-31"]

cat("Period 1 (2013-2017):", nrow(data1), "obs\n")
cat("Period 2 (2019-2021):", nrow(data2), "obs\n\n")

# ── 4. Descriptive Statistics ───────────────────────────────────────────

cat("=== DESCRIPTIVE STATISTICS: LEVELS ===\n\n")
cat("--- 2013-2017 ---\n")
cat("Oil:\n");  print(round(stat.desc(as.numeric(data1$Oil)), 4))
cat("\nICLN:\n"); print(round(stat.desc(as.numeric(data1$ICLN)), 4))
cat("\n--- 2019-2021 ---\n")
cat("Oil:\n");  print(round(stat.desc(as.numeric(data2$Oil)), 4))
cat("\nICLN:\n"); print(round(stat.desc(as.numeric(data2$ICLN)), 4))

# ── 5. Plot Level Data ──────────────────────────────────────────────────

par(mfrow = c(2, 1), mar = c(4, 4, 3, 4))
plot(data1$Oil, main = "Oil & ICLN (2013-2017)", ylab = "Oil (USD)", col = "#1f4e79")
par(new = TRUE)
plot(data1$ICLN, col = "#c45911", axes = FALSE, xlab = "", ylab = "")
axis(4); mtext("ICLN (USD)", side = 4, line = 2.5)
legend("topright", c("Brent", "ICLN"), col = c("#1f4e79", "#c45911"), lty = 1)

plot(data2$Oil, main = "Oil & ICLN (2019-2021)", ylab = "Oil (USD)", col = "#1f4e79")
par(new = TRUE)
plot(data2$ICLN, col = "#c45911", axes = FALSE, xlab = "", ylab = "")
axis(4); mtext("ICLN (USD)", side = 4, line = 2.5)
legend("topright", c("Brent", "ICLN"), col = c("#1f4e79", "#c45911"), lty = 1)

# ── 6. ADF on Levels ────────────────────────────────────────────────────

cat("\n=== ADF TESTS: LEVELS ===\n\n")
for (nm in c("Oil", "ICLN")) {
  for (pd in list(list("2013-17", data1), list("2019-21", data2))) {
    test <- adf.test(as.numeric(pd[[2]][, nm]))
    cat(nm, pd[[1]], ": ADF =", round(test$statistic, 2), ", p =", round(test$p.value, 4), "\n")
  }
}
cat("=> Non-stationary\n")

# ── 7. Log Returns ──────────────────────────────────────────────────────

ret1 <- na.omit(diff(log(data1))); colnames(ret1) <- c("Oil", "ICLN")
ret2 <- na.omit(diff(log(data2))); colnames(ret2) <- c("Oil", "ICLN")
cat("\nReturns: 2013-17 =", nrow(ret1), ", 2019-21 =", nrow(ret2), "\n")

# ── 8. ADF on Returns ──────────────────────────────────────────────────

cat("\n=== ADF TESTS: RETURNS ===\n\n")
for (nm in c("Oil", "ICLN")) {
  for (pd in list(list("2013-17", ret1), list("2019-21", ret2))) {
    test <- adf.test(as.numeric(pd[[2]][, nm]))
    cat(nm, pd[[1]], ": ADF =", round(test$statistic, 2), ", p =", round(test$p.value, 4), "\n")
  }
}
cat("=> Stationary (p < 0.01)\n")

# Return stats
cat("\n=== RETURN STATISTICS ===\n")
for (pd in list(list("2013-17", ret1), list("2019-21", ret2))) {
  cat("\n---", pd[[1]], "---\n")
  for (nm in c("Oil", "ICLN")) {
    x <- as.numeric(pd[[2]][, nm])
    sk <- sum((x - mean(x))^3) / (length(x) * sd(x)^3)
    ku <- sum((x - mean(x))^4) / (length(x) * sd(x)^4) - 3
    cat(sprintf("  %s: mean=%.6f sd=%.4f min=%.4f max=%.4f skew=%.2f kurt=%.2f\n",
                nm, mean(x), sd(x), min(x), max(x), sk, ku))
  }
}

# Plot returns
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
plot(ret1$Oil,  main = "Oil Returns 2013-17",  col = "#1f4e79", lwd = 0.4)
plot(ret1$ICLN, main = "ICLN Returns 2013-17", col = "#c45911", lwd = 0.4)
plot(ret2$Oil,  main = "Oil Returns 2019-21",  col = "#1f4e79", lwd = 0.4)
plot(ret2$ICLN, main = "ICLN Returns 2019-21", col = "#c45911", lwd = 0.4)

# Histograms
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
for (pd in list(list("2013-17", ret1), list("2019-21", ret2))) {
  for (nm in c("Oil", "ICLN")) {
    x <- as.numeric(pd[[2]][, nm])
    hist(x, breaks = 60, freq = FALSE,
         col = adjustcolor(ifelse(nm == "Oil", "#1f4e79", "#c45911"), 0.5),
         main = paste(nm, pd[[1]]), xlab = "Log Return")
    curve(dnorm(x, mean(x), sd(x)), add = TRUE, col = "black", lty = 2, lwd = 1.5)
  }
}

# ── 9. Correlation ──────────────────────────────────────────────────────

cat("\n=== CORRELATION ===\n\n")
r1 <- cor(as.numeric(ret1$Oil), as.numeric(ret1$ICLN))
r2 <- cor(as.numeric(ret2$Oil), as.numeric(ret2$ICLN))
cat("r (2013-17):", round(r1, 4), "\n")
cat("r (2019-21):", round(r2, 4), "\n")

z_stat <- (atanh(r2) - atanh(r1)) / sqrt(1/(nrow(ret1)-3) + 1/(nrow(ret2)-3))
cat("Fisher z-test: z =", round(z_stat, 3), ", p =", round(2*(1-pnorm(abs(z_stat))), 4), "\n")

# Scatter
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
plot(as.numeric(ret1$Oil), as.numeric(ret1$ICLN), pch = 16, cex = 0.3,
     col = adjustcolor("#1f4e79", 0.4), xlab = "Oil", ylab = "ICLN",
     main = paste0("2013-17 (r=", round(r1,3), ")"))
abline(lm(as.numeric(ret1$ICLN) ~ as.numeric(ret1$Oil)), col = "red", lwd = 1.5)
plot(as.numeric(ret2$Oil), as.numeric(ret2$ICLN), pch = 16, cex = 0.3,
     col = adjustcolor("#c45911", 0.4), xlab = "Oil", ylab = "ICLN",
     main = paste0("2019-21 (r=", round(r2,3), ")"))
abline(lm(as.numeric(ret2$ICLN) ~ as.numeric(ret2$Oil)), col = "red", lwd = 1.5)

# Rolling correlation
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
rc1 <- rollapply(ret1, 60, function(x) cor(x[,1], x[,2]), by.column = FALSE, align = "right")
rc2 <- rollapply(ret2, 60, function(x) cor(x[,1], x[,2]), by.column = FALSE, align = "right")
plot(rc1, main = "Rolling Corr 60d (2013-17)", ylab = "r", col = "#1f4e79"); abline(h=0, lty=2)
plot(rc2, main = "Rolling Corr 60d (2019-21)", ylab = "r", col = "#c45911"); abline(h=0, lty=2)

# ── 10. VAR Lag Selection ───────────────────────────────────────────────

cat("\n=== LAG SELECTION ===\n\n")
sel1 <- VARselect(ret1, lag.max = 15, type = "const")
sel2 <- VARselect(ret2, lag.max = 15, type = "const")
print(sel1$selection); cat("\n"); print(sel2$selection)
p1 <- sel1$selection["AIC(n)"]; p2 <- sel2$selection["AIC(n)"]
cat("\nUsing: p1 =", p1, ", p2 =", p2, "\n")

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
plot(1:15, sel1$criteria["AIC(n)",], type="o", pch=16, col="#1f4e79",
     xlab="Lag", ylab="AIC", main="AIC (2013-17)"); abline(v=p1, col="red", lty=2)
plot(1:15, sel2$criteria["AIC(n)",], type="o", pch=16, col="#c45911",
     xlab="Lag", ylab="AIC", main="AIC (2019-21)"); abline(v=p2, col="red", lty=2)

# ── 11. VAR Estimation ──────────────────────────────────────────────────

cat("\n=== VAR ESTIMATION ===\n\n")
var1 <- VAR(ret1, p = p1, type = "const")
var2 <- VAR(ret2, p = p2, type = "const")
cat("--- 2013-2017 ---\n"); print(summary(var1))
cat("\n--- 2019-2021 ---\n"); print(summary(var2))

# ── 12. Diagnostics ─────────────────────────────────────────────────────

cat("\n=== DIAGNOSTICS ===\n\n")

pt1 <- serial.test(var1, lags.pt = 16, type = "PT.asymptotic")
pt2 <- serial.test(var2, lags.pt = 16, type = "PT.asymptotic")
cat("Portmanteau: 2013-17 p =", round(pt1$serial$p.value,4),
    ", 2019-21 p =", round(pt2$serial$p.value,4), "\n")

ar1 <- arch.test(var1, lags.multi = 5)
ar2 <- arch.test(var2, lags.multi = 5)
cat("ARCH: 2013-17 p =", round(ar1$arch.mul$p.value,4),
    ", 2019-21 p =", round(ar2$arch.mul$p.value,4), "\n")

nm1 <- normality.test(var1); nm2 <- normality.test(var2)
cat("Normality: 2013-17 p =", round(nm1$jb.mul$JB$p.value,4),
    ", 2019-21 p =", round(nm2$jb.mul$JB$p.value,4), "\n")

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
plot(stability(var1, type = "OLS-CUSUM"), main = "CUSUM 2013-17")
plot(stability(var2, type = "OLS-CUSUM"), main = "CUSUM 2019-21")

cat("\nLjung-Box:\n")
for (nm in c("Oil", "ICLN")) {
  cat(nm, ": 2013-17 p =", round(Box.test(residuals(var1)[,nm], lag=10, type="Ljung-Box")$p.value, 4),
      ", 2019-21 p =", round(Box.test(residuals(var2)[,nm], lag=10, type="Ljung-Box")$p.value, 4), "\n")
}

# ── 13. Granger Causality ───────────────────────────────────────────────

cat("\n=== GRANGER CAUSALITY ===\n\n")
gc <- function(v, cause) { r <- causality(v, cause = cause)$Granger
  cat("  F =", round(r$statistic, 3), ", p =", round(r$p.value, 4), "\n") }

cat("2013-17 Oil->ICLN:"); gc(var1, "Oil")
cat("2013-17 ICLN->Oil:"); gc(var1, "ICLN")
cat("2019-21 Oil->ICLN:"); gc(var2, "Oil")
cat("2019-21 ICLN->Oil:"); gc(var2, "ICLN")

# ── 14. IRF ─────────────────────────────────────────────────────────────

cat("\n=== IMPULSE RESPONSE FUNCTIONS ===\n")

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
plot(irf(var1, impulse="Oil", response="ICLN", n.ahead=20, boot=TRUE, runs=100))
plot(irf(var1, impulse="ICLN", response="Oil", n.ahead=20, boot=TRUE, runs=100))
plot(irf(var2, impulse="Oil", response="ICLN", n.ahead=20, boot=TRUE, runs=100))
plot(irf(var2, impulse="ICLN", response="Oil", n.ahead=20, boot=TRUE, runs=100))

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
plot(irf(var1, impulse="Oil", response="ICLN", n.ahead=20, boot=TRUE, runs=100, cumulative=TRUE),
     main = "Cumul. IRF 2013-17")
plot(irf(var2, impulse="Oil", response="ICLN", n.ahead=20, boot=TRUE, runs=100, cumulative=TRUE),
     main = "Cumul. IRF 2019-21")

# ── 15. FEVD ────────────────────────────────────────────────────────────

cat("\n=== FEVD ===\n\n")
fv1 <- fevd(var1, n.ahead = 5); fv2 <- fevd(var2, n.ahead = 5)
cat("2013-17 ICLN:\n"); print(round(fv1$ICLN, 4))
cat("\n2019-21 ICLN:\n"); print(round(fv2$ICLN, 4))

par(mfrow = c(1, 2)); plot(fv1); plot(fv2)

# ── 16. Robustness ──────────────────────────────────────────────────────

cat("\n=== REVERSE ORDERING ===\n\n")
fv1r <- fevd(VAR(ret1[,c("ICLN","Oil")], p=p1, type="const"), n.ahead=5)
fv2r <- fevd(VAR(ret2[,c("ICLN","Oil")], p=p2, type="const"), n.ahead=5)
cat("Reverse 2013-17:\n"); print(round(fv1r$ICLN, 4))
cat("\nReverse 2019-21:\n"); print(round(fv2r$ICLN, 4))

# ── 17. Summary ─────────────────────────────────────────────────────────

cat("\n==========================================\n")
cat("         SUMMARY OF KEY RESULTS          \n")
cat("==========================================\n\n")
cat("Adj R2 (ICLN): 2013-17 =", round(summary(var1)$varresult$ICLN$adj.r.squared, 4),
    ", 2019-21 =", round(summary(var2)$varresult$ICLN$adj.r.squared, 4), "\n")
cat("FEVD h=5 Oil: 2013-17 =", round(fv1$ICLN[5,"Oil"], 4),
    ", 2019-21 =", round(fv2$ICLN[5,"Oil"], 4), "\n")
cat("Correlation:  2013-17 =", round(r1, 4), ", 2019-21 =", round(r2, 4), "\n")
cat("\nDone!\n")
