rm(list = ls())

library(readxl)
library(readr)
library(vars)
library(xtable)
library(stargazer)
library(ggplot2)
library(gridExtra)
library(matrixcalc)
library(nleqslv)


# === Read the Data ===
oil <- read_csv(file = "../Data/DCOILWTICO.csv")
head(oil)

output <- read_csv(file = "../Data/CLVMNACSCAB1GQDE.csv")
head(output)

price <- read_csv(file = "../Data/CP0000DEM086NEST.csv")
head(price)

int <- read_csv(file = "../Data/IR3TIB01DEM156N.csv")
head(int)


# === Transform the data ===
len_oil <- dim(oil)[1]
len_output <- dim(output)[1]
len_price <- dim(price)[1]
len_int <- dim(int)[1]

oil$oil_growth <- rep(NA, len_oil)
oil$oil_growth[2:len_oil] <- diff(log(oil$DCOILWTICO_NBD20100101))

output$output_growth <- rep(NA, len_output)
output$output_growth[2:len_output] <- diff(log(output$CLVMNACSCAB1GQDE))

price$inflation <- rep(NA, len_price)
price$inflation[2:len_price] <- diff(log(price$CP0000DEM086NEST))

names(int)[names(int) == "IR3TIB01DEM156N"] <- "int_rate"

# Merge
data_merge <- merge(x = oil, y = output, by = "DATE")
data_merge <- merge(x = data_merge, y = price, by = "DATE")
data_merge <- merge(x = data_merge, y = int, by = "DATE")

data_merge <- data_merge[-1, ]

rownames(data_merge) <- data_merge$DATE

head(data_merge)
tail(data_merge)

# === Plot the Data ===
par(mfrow = c(2, 1))
plot(data_merge$DATE, data_merge$DCOILWTICO_NBD20100101, type = "l")
plot(data_merge$DATE, data_merge$oil_growth, type = "l")

plot(data_merge$DATE, data_merge$CLVMNACSCAB1GQDE, type = "l")
plot(data_merge$DATE, data_merge$output_growth, type = "l")

plot(data_merge$DATE, data_merge$CP0000DEM086NEST, type = "l")
plot(data_merge$DATE, data_merge$inflation, type = "l")

plot(data_merge$DATE, data_merge$int_rate, type = "l")


# === Estimate the Reduced Model ===
y_vec <- data_merge[c("oil_growth", "output_growth", "inflation", "int_rate")]

var_res <- VAR(y_vec, p = 1, type = "const")

summary(var_res)


# === Short and Long run restrictions together, using Chapter 14.3 of MHH12 ===
sl_eq <- function(param, V_mat, Phi_1_inv) {
    S_mat <- diag(4)

    S_mat[1, 1] <- param[1]

    S_mat[2, 1] <- param[2]
    S_mat[2, 2] <- param[3]
    S_mat[2, 3] <- param[4]

    S_mat[3, 1] <- param[5]
    S_mat[3, 2] <- param[6]
    S_mat[3, 3] <- param[7]

    S_mat[4, 1] <- param[8]
    S_mat[4, 2] <- param[9]
    S_mat[4, 4] <- param[10]

    S_mat[3, 4] <- -Phi_1_inv[2, 4] / Phi_1_inv[2, 3] * S_mat[4, 4]
    S_mat[4, 3] <- -Phi_1_inv[2, 2] / Phi_1_inv[2, 4] * S_mat[2, 3] -
        Phi_1_inv[2, 3] / Phi_1_inv[2, 4] * S_mat[3, 3]

    zero_mat <- V_mat - S_mat %*% t(S_mat)

    return(vech(zero_mat))
}

var_sum <- summary(var_res)
print(var_sum$covres)

Phi1_hat <- Acoef(var_res)[[1]]

Phi_1_inv <- solve(diag(4) - Phi1_hat)

param0 <- c(
    s11 = 1,
    s21 = 0,
    s22 = 1,
    s23 = 0,
    s31 = 0,
    s32 = 0,
    s33 = 1,
    s41 = 0,
    s42 = 0,
    s44 = 1
)

sl_eq(param = param0, var_sum$covres, Phi_1_inv)

sl_sol <- nleqslv(
    x = param0, fn = sl_eq, V_mat = var_sum$covres, Phi_1_inv = Phi_1_inv
)

S_sl <- diag(4)
S_sl[1, 1] <- sl_sol$x[1]

S_sl[2, 1] <- sl_sol$x[2]
S_sl[2, 2] <- sl_sol$x[3]
S_sl[2, 3] <- sl_sol$x[4]

S_sl[3, 1] <- sl_sol$x[5]
S_sl[3, 2] <- sl_sol$x[6]
S_sl[3, 3] <- sl_sol$x[7]

S_sl[4, 1] <- sl_sol$x[8]
S_sl[4, 2] <- sl_sol$x[9]
S_sl[4, 4] <- sl_sol$x[10]

print(S_sl)

F_hat <- Phi_1_inv %*% S_sl
print(F_hat)
