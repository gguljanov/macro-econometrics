library(tseries)
library(urca)
library(readr)


# === GDP ===
real_gdp <- read_csv("../Data/CLVMNACSCAB1GQDE.csv")

plot(real_gdp$DATE, real_gdp$CLVMNACSCAB1GQDE, type = "l")
dev.print(pdf, "../Figs/real_gdp_tsplot.pdf")

# ADF-test
real_gdp_test <- ur.df(
    y = real_gdp$CLVMNACSCAB1GQDE, type = "trend", selectlags = "AIC"
)
summary(real_gdp_test)


# === Growth rate ===
len <- length(real_gdp$CLVMNACSCAB1GQDE)

real_gdp$growth <- rep(NA, len)

real_gdp$growth[2:len] <- 100 * (
    real_gdp$CLVMNACSCAB1GQDE[2:len] /
        real_gdp$CLVMNACSCAB1GQDE[1:(len - 1)] -
        1
)

plot(real_gdp$DATE, real_gdp$growth, type = "l")
dev.print(pdf, "../Figs/gdp_growth_tsplot.pdf")

growth_test <- ur.df(
    y = real_gdp$growth[2:len], type = "drift", selectlags = "AIC"
)
summary(growth_test)
