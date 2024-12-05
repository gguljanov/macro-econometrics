rm(list = ls())

library(readr)
library(vars)
library(xtable)
library(stargazer)
library(ggplot2)
library(gridExtra)

# Read the data
real_gdp <- read_csv(file = "../Data/real_gdp-growth-Germany.csv")

unemp <- read_csv(file = "../Data/unemp-perc_change-Germany.csv")

# Plot the series, to get an initial idea
par(mfrow = c(2, 1))

plot(real_gdp$growth, type = "l")
plot(unemp$delta, type = "l")


# === Estimate with Cholesky Decomposition ===
len <- length(real_gdp$growth)

y_vec <- cbind(
    unemp_pchange = unemp$delta[2:len], gdp_gr = real_gdp$growth[2:len]
)

var_estim <- VAR(y_vec, p = 1, type = "const")

summary(var_estim)


# Determine the matrices B0 and D
var_sum <- summary(var_estim)

S_hat <- t(chol(var_sum$covres))

D_hat_sqroot <- diag(diag(S_hat))
D_hat <- diag((diag(S_hat))^2)

B0_hat <- solve(S_hat %*% solve(D_hat_sqroot))


# === Estimate with Likelihood ===
# Define B0, to be estimated parameters should be NA
B0_mat <- diag(2)
B0_mat[2, 1] <- NA

print(B0_mat)

# Define the matrix multiplying the shocks, i.e. square root of the matrix D
D_mat_sqroot <- diag(2)
diag(D_mat_sqroot) <- rep(NA, 2)

print(D_mat_sqroot)

svar_estim <- SVAR(
    var_estim,
    estmethod = "direct",
    Amat = B0_mat,
    Bmat = D_mat_sqroot,
    method = "BFGS"
)
summary(svar_estim)

svar_sum <- summary(svar_estim)

# Compare the results
print(B0_hat - svar_sum$A)

print(D_hat_sqroot - svar_sum$B)


# === Orthogonalized Impulse Response Function, with vars package ===
irf_res <- irf(
    x = var_estim, impulse = "unemp_pchange", ortho = TRUE, cumulative = FALSE
)

plot(irf_res)


# === Orthogonalized Impulse response functions, manually ===
mu_hat <- c(
    var_estim$varresult$unemp_pchange$coefficients["const"],
    var_estim$varresult$gdp_gr$coefficients["const"]
)

Phi1_hat <- c(
    var_estim$varresult$unemp_pchange$coefficients["unemp_pchange.l1"],
    var_estim$varresult$unemp_pchange$coefficients["gdp_gr.l1"],
    var_estim$varresult$gdp_gr$coefficients["unemp_pchange.l1"],
    var_estim$varresult$gdp_gr$coefficients["gdp_gr.l1"]
)
Phi1_hat <- matrix(Phi1_hat, nrow = 2, byrow = TRUE)

yt1 <- c(0, 0)

irf_coef <- matrix(
    NA,
    nrow = 11,
    ncol = 2,
    dimnames = list(c(), c("unemp_pchange", "gdp_gr"))
)

for (ii in 1:11) {
    if (ii == 1) {
        eta_t <- c(1, 0)
    } else {
        eta_t <- c(0, 0)
    }

    inv_B0 <- solve(B0_hat)

    yt <- Phi1_hat %*% yt1 + inv_B0 %*% D_hat_sqroot %*% eta_t

    yt1 <- yt

    irf_coef[ii, ] <- yt
}

irf_coef


# Let us compare to the results from vars package: small numerical errors
print(irf_coef - irf_res$irf$unemp_pchange)


# === IRF in the unemployment rate (not in its percentage change!) ===
irf_res_cum <- irf(
    x = var_estim, impulse = "unemp_pchange", ortho = TRUE, cumulative = TRUE
)

plot(irf_res_cum)

# Compare with our manually computed IRF
print(cumsum(irf_coef[, 1]) - irf_res_cum$irf$unemp_pchange[, 1])


# === Variance Decomposition, with vars package ===
fevd_res <- fevd(svar_estim, n.ahead = 3)$unemp_pchange
print(fevd_res)

# Variance Decomposition, manually
irf_all <- irf(var_estim, ortho = TRUE, cumulative = FALSE)

irf_l1 <- cbind(
    irf_all$irf$unemp_pchange[1, ],
    irf_all$irf$gdp_gr[1, ]
)

irf_l2 <- cbind(
    irf_all$irf$unemp_pchange[2, ],
    irf_all$irf$gdp_gr[2, ]
)

irf_l3 <- cbind(
    irf_all$irf$unemp_pchange[3, ],
    irf_all$irf$gdp_gr[3, ]
)

vd_h1 <- irf_l1 * irf_l1
vd_h2 <- irf_l2 * irf_l2 + vd_h1
vd_h3 <- irf_l3 * irf_l3 + vd_h2

vd_mat <- rbind(vd_h1[1, ], vd_h2[1, ], vd_h3[1, ])

vd_mat_norm <- vd_mat / rowSums(x = vd_mat)

print(vd_mat_norm)

# Compare vars package result with our result: Irrelevant numerical errors
print(fevd_res - vd_mat_norm)
