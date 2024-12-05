library(readr)
library(vars)
library(xtable)
library(stargazer)
library(ggplot2)
library(gridExtra)

# Unemployment for Germany, from FRED, seasonlly adjusted, Quarterly, percent
unemp <- read_csv("../Data/LRHUADTTDEQ156S.csv")

head(unemp)
tail(unemp)

len <- dim(unemp)[1]

# Percentage change
unemp$delta <- rep(NA, len)
unemp$delta[2:len] <- diff(unemp$LRHUADTTDEQ156S, lag = 1)

write.csv(x = unemp, file = "../Data/unemp-perc_change-Germany.csv")

# Real Gross Domestic Product for Germany, from FRED, sesonally adj, Quarterly
real_gdp <- read_csv("../Data/CLVMNACSCAB1GQDE.csv")

head(real_gdp)
tail(real_gdp)

len <- dim(real_gdp)[1]

# Growth rate
real_gdp$growth <- rep(NA, len)

real_gdp$growth[2:len] <- 100 * (
    real_gdp$CLVMNACSCAB1GQDE[2:len] /
        real_gdp$CLVMNACSCAB1GQDE[1:(len - 1)] -
        1
)

write.csv(x = real_gdp, file = "../Data/real_gdp-growth-Germany.csv")

# Plot the real GDP and its growth
real_gdp_pl <- ggplot(data = real_gdp, aes(x = DATE, y = CLVMNACSCAB1GQDE)) +
    geom_line() +
    xlab("date") +
    ylab("real GDP")

gdp_growth_pl <- ggplot(data = real_gdp, aes(x = DATE, y = growth)) +
    geom_line() +
    xlab("date") +
    ylab("GDP growth")

grid.arrange(real_gdp_pl, gdp_growth_pl)

# Plot the unemployment and its change
unemp_pl <- ggplot(data = unemp, aes(x = DATE, y = LRHUADTTDEQ156S)) +
    geom_line() +
    xlab("date") +
    ylab("Unemployment in percentage")

unemp_delta_pl <- ggplot(data = unemp, aes(x = DATE, y = delta)) +
    geom_line() +
    xlab("date") +
    ylab("Change in unemployment")

grid.arrange(unemp_pl, unemp_delta_pl)

# Select optimal number of lags
y_vec <- cbind(
    unemployment = unemp$delta[2:len],
    growth = real_gdp$growth[2:len]
)

lag_sel <- VARselect(y_vec, lag.max = 10, type = "const")

print(lag_sel)

lag_res <- stargazer(
    lag_sel$criteria,
    title = "Lag Selection with Information criteria",
    rownames = TRUE
)
writeLines(text = lag_res, con = "../Tex-files-auto/okun_law-lag_sel_table.tex")

# Estimate
res <- VAR(y_vec, p = 1, type = "const")

summary(res)

reg_unemployment <- xtable(res$varresult$unemployment)
print(reg_unemployment, file = "../Tex-files-auto/reg_unemployment.tex")

reg_growth <- xtable(res$varresult$growth)
print(reg_growth, file = "../Tex-files-auto/reg_growth.tex")

# Granger causality
caus_res <- causality(x = res, cause = "unemployment")

print(caus_res)

caus_res$Granger$statistic
caus_res$Granger$parameter
caus_res$Granger$p.value
caus_res$Granger$method
caus_res$Granger$data.name

tex_txt <- paste(
    c(caus_res$Granger$method, "; The p-value is: ", caus_res$Granger$p.value),
    collapse = ""
)

writeLines(text = tex_txt, con = "../Tex-files-auto/granger_res.tex")

# Impulse Response Function
irf_res <- irf(res, ortho = FALSE, cumulative = FALSE)

plot(irf_res)

# Impulse response functions manually
names(res)
names(res$varresult)
names(res$varresult$growth)
names(res$varresult$growth$coefficients)

mu_hat <- c(
    res$varresult$unemployment$coefficients["const"],
    res$varresult$growth$coefficients["const"]
)

Phi1_hat <- c(
    res$varresult$unemployment$coefficients["unemployment.l1"],
    res$varresult$unemployment$coefficients["growth.l1"],
    res$varresult$growth$coefficients["unemployment.l1"],
    res$varresult$growth$coefficients["growth.l1"]
)
Phi1_hat <- matrix(Phi1_hat, nrow = 2, byrow = TRUE)

yt1 <- c(0, 0)

irf_coef <- matrix(
    NA,
    nrow = 11,
    ncol = 2,
    dimnames = list(c(), c("unemployment", "growth"))
)

for (ii in 1:11) {
    if (ii == 1) {
        nu_vec <- c(1, 0)
    } else {
        nu_vec <- c(0, 0)
    }

    yt <- Phi1_hat %*% yt1 + nu_vec
    # yt <- Phi1_hat %*% yt1 + Phi2_hat %*% yt2 + nu_vec

    yt2 <- yt1
    yt1 <- yt

    irf_coef[ii, ] <- yt
}
