# === EUROSTAT ===
library(eurostat)

# Table of contents (TOC)
toc <- get_eurostat_toc()

head(toc)

colnames(toc)

head(toc$title)

# Search in Eurostat
search_gdp <- search_eurostat(pattern = "GDP", type = "dataset", fixed = TRUE)

head(search_gdp)

colnames(toc)

print(search_gdp$title)

# Download from Eurostat
ddn <- get_eurostat(
    id = "namq_10_gdp",
    filters = list(
        geo = c("EA", "DE", "IT", "FR"),
        untilTimePeriod = 2024,
        na_item = "B1GQ",
        unit = "CLV_I10",
        s_adj = "SCA"
    ),
    cache = FALSE
)

# Citation: R-documentation of the package "eurostat"
