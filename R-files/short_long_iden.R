rm(list = ls())

library(readr)
library(vars)
library(xtable)
library(stargazer)
library(ggplot2)
library(gridExtra)
library(matrixcalc)
library(nleqslv)


# === Load data ===
# Real Gross Domestic Product for Germany, from FRED, sesonally adj, Quarterly
real_gdp <- read_csv("../Data/CLVMNACSCAB1GQDE.csv")

head(real_gdp)
tail(real_gdp)

# CPI for Germany, from FRED, seasonlly adjusted, Quarterly, percent
cpi <- read_csv("../Data/CP0000DEM086NEST.csv")

head(cpi)
tail(cpi)


# === Merge two data sets ===
dataset <- merge(x = real_gdp, y = cpi, by = "DATE")

len <- dim(dataset)[1]


# === Transform the data ===
# Log change in Output
dataset$d_output <- rep(NA, len)

dataset$d_output[2:len] <- diff(log(dataset$CLVMNACSCAB1GQDE))

# Log change in Prices
dataset$d_price <- rep(NA, len)

dataset$d_price[2:len] <- diff(log(dataset$CP0000DEM086NEST), lag = 1)


# === Plot the transformed data ===
# Plot the real GDP and its growth
output_pl <- ggplot(data = dataset, aes(x = DATE, y = CLVMNACSCAB1GQDE)) +
    geom_line() +
    xlab("date") +
    ylab("GDP")

d_output_pl <- ggplot(data = dataset, aes(x = DATE, y = d_output)) +
    geom_line() +
    xlab("date") +
    ylab("GDP - log change")

grid.arrange(output_pl, d_output_pl)

# Plot the unemployment and its change
price_pl <- ggplot(data = dataset, aes(x = DATE, y = CP0000DEM086NEST)) +
    geom_line() +
    xlab("date") +
    ylab("Prices")

d_price_pl <- ggplot(data = dataset, aes(x = DATE, y = d_price)) +
    geom_line() +
    xlab("date") +
    ylab("Prices - log change")

grid.arrange(price_pl, d_price_pl)


# # === Select optimal number of lags ===
# y_vec <- cbind(
#     unemployment = unemp$delta[2:len],
#     growth = real_gdp$growth[2:len]
# )

# lag_sel <- VARselect(y_vec, lag.max = 10, type = "const")

# print(lag_sel)

# lag_res <- stargazer(
#     lag_sel$criteria,
#     title = "Lag Selection with Information criteria",
#     rownames = TRUE
# )
# writeLines(
#     text = lag_res, con = "../Tex-files-auto/okun_law-lag_sel_table.tex"
# )


# === Reduced form estimation ===
y_vec <- cbind(
    output = dataset$d_output[2:len],
    price = dataset$d_price[2:len]
)

var_res <- VAR(y_vec, p = 1, type = "const")

summary(var_res)


# === vars: short run restriction ===
B0_mat <- diag(2)
B0_mat[1, 2] <- NA

print(B0_mat)

D_mat_sqroot <- diag(2)
diag(D_mat_sqroot) <- rep(NA, 2)

print(D_mat_sqroot)

short_res <- SVAR(
    x = var_res,
    estmethod = "direct",
    Amat = B0_mat,
    Bmat = D_mat_sqroot,
    method = "BFGS",
    lrtest = FALSE
)
summary(short_res)

short_irf <- irf(x = short_res, ortho = TRUE, cumulative = FALSE)
plot(short_irf)


# === Manual: short run restrictions, using Chapter 14.3 of MHH12 ===
var_sum <- summary(var_res)
var_sum$covres

short_eq <- function(param, V_mat) {
    S_mat <- diag(2)
    S_mat[1, 1] <- param[1]
    S_mat[1, 2] <- param[2]
    S_mat[2, 2] <- param[3]

    zero_mat <- V_mat - S_mat %*% t(S_mat)

    return(vech(zero_mat))
}

short_eq(param = c(1, 0, 1), var_sum$covres)

short_sol <- nleqslv(x = c(1, 0, 1), fn = short_eq, V_mat = var_sum$covres)

S_short <- diag(2)
S_short[1, 1] <- short_sol$x[1]
S_short[1, 2] <- short_sol$x[2]
S_short[2, 2] <- short_sol$x[3]

short_eq2 <- function(param, S_mat) {
    B0_mat <- diag(2)
    D_mat_sqroot <- diag(2)

    B0_mat[1, 2] <- param[1]
    D_mat_sqroot[1, 1] <- param[2]
    D_mat_sqroot[2, 2] <- param[3]

    zero_mat <- S_mat - solve(B0_mat) %*% D_mat_sqroot

    return(vech(t(zero_mat)))
}

short_eq2(param = c(0.1, 0.1, 0.1), S_mat = S_short)

short_sol2 <- nleqslv(x = c(0.1, -2, -2), fn = short_eq2, S_mat = S_short)

# Compare results from manual calc. and vars
short_man <- list()

short_man$B0 <- diag(2)
short_man$B0[1, 2] <- short_sol2$x[1]

short_man$D_hat_sqroot <- diag(2)
short_man$D_hat_sqroot[1, 1] <- short_sol2$x[2]
short_man$D_hat_sqroot[2, 2] <- short_sol2$x[3]

print(short_man$B0 - short_res$A)
print(short_man$D_hat_sqroot - short_res$B)


# === vars: long run restrictions, using Chapter 14.3 of MHH12 ===
long_res <- BQ(var_res)
summary(long_res)

long_irf <- irf(x = long_res, ortho = TRUE, cumulative = FALSE)
plot(long_irf)


# === Manual: long run restrictions ===
long_eq <- function(param, V_mat, Phi1) {
    F_mat <- diag(2)
    F_mat[1, 1] <- param[1]
    F_mat[2, 1] <- param[2]
    F_mat[2, 2] <- param[3]

    S_mat <- (diag(2) - Phi1) %*% F_mat

    zero_mat <- V_mat - S_mat %*% t(S_mat)

    return(vech(zero_mat))
}

Phi1_hat <- Acoef(var_res)[[1]]

long_eq(param = c(1, 0, 1), var_sum$covres, Phi1_hat)

long_sol <- nleqslv(
    x = c(1, 0, 1), fn = long_eq, V_mat = var_sum$covres, Phi1 = Phi1_hat
)

F_hat <- diag(2)
F_hat[1, 1] <- long_sol$x[1]
F_hat[2, 1] <- long_sol$x[2]
F_hat[2, 2] <- long_sol$x[3]

print(F_hat)

S_long <- (diag(2) - Phi1_hat) %*% F_hat

# Compare the manual and vars results
print(S_long - long_res$B)
print(F_hat - long_res$LRIM)


# === Short and Long run restrictions together, using Chapter 14.3 of MHH12 ===
Phi_1_inv <- solve(diag(2) - Phi1_hat)

get_Smat <- function(param, Phi_1_inv) {
    S_mat <- diag(2)
    S_mat[1, 1] <- param[1]
    S_mat[1, 2] <- param[2]
    S_mat[2, 2] <- -Phi_1_inv[1, 1] / Phi_1_inv[1, 2] * S_mat[1, 2]

    return(S_mat)
}

sl_nlogliki <- function(param, Phi_1_inv, resi) {
    #
    # Negative log-likelihood, concentrated version
    #

    nobs <- dim(resi)[1]

    S_mat <- get_Smat(param, Phi_1_inv)

    V_mat <- S_mat %*% t(S_mat)

    if (det(V_mat) < 1e-10) {
        return(-Inf)
    }

    summation_part1 <- resi %*% solve(V_mat)
    summation <- t(vec(t(summation_part1))) %*% vec(t(resi))

    nloglik_val <- 0.5 * log(det(V_mat)) + 0.5 / (2 * nobs) * summation

    return(nloglik_val)
}

# Minimizing nlogliki
sl_sol <- optim(
    par = c(1, 0.1),
    fn = sl_nlogliki,
    method = "Nelder-Mead",
    Phi_1_inv = Phi_1_inv,
    resi = residuals(var_res)
)

print(sl_sol)
summary(sl_sol)

S_sl <- get_Smat(sl_sol$par, Phi_1_inv)
print(S_sl)

F_sl <- Phi_1_inv %*% S_sl
print(F_sl)

V_mat <- S_sl %*% t(S_sl)
print(V_mat)
print(var_sum$covres)
