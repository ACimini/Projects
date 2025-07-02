packages = c(
    "dplyr",
    "readxl",
    "quantreg"
)

installed = rownames(installed.packages())
to_install = setdiff(packages, installed)

if (length(to_install) > 0) {
    install.packages(to_install)
}

library(readxl)
library(dplyr)

collapsed = read.csv("collapsed.csv")
collapsed$colors = NA
collapsed$colors[collapsed$sic_industry=="Construction"] = "yellow"
collapsed$colors[collapsed$sic_industry=="Finance, Insurance and Real estate"] = "green"
collapsed$colors[collapsed$sic_industry=="Retail Trade"] = "black"
collapsed$colors[collapsed$sic_industry=="Manufacturing"] = "red"
collapsed$colors[collapsed$sic_industry=="Wholesale Trade"] = "skyblue"
collapsed$colors[collapsed$sic_industry=="Transportation, Communications, Electric, Gas and Sanitary service"] = "orange"
collapsed$colors[collapsed$sic_industry=="Services"] = "blue"
collapsed$colors[collapsed$sic_industry=="Agriculture, Forestry and Fishing"] = "brown"
collapsed$colors[collapsed$sic_industry=="Public administration"] = "purple"
collapsed$colors[collapsed$sic_industry=="Mining"] = "grey"
collapsed$midpoint = collapsed$attachment + collapsed$limit / 2

expanded = read.csv("expanded.csv")

collapsed_prediction = collapsed[collapsed$layers_in_tower > 3,]
collapsed_pred_distr <- collapsed %>%
    group_by(midpoint) %>%
    summarize(
        median = median(lead_ratio, na.rm = TRUE),
        mean = mean(lead_ratio, na.rm = TRUE),
        p25 = quantile(lead_ratio, 0.25, na.rm = TRUE),
        p75 = quantile(lead_ratio, 0.75, na.rm = TRUE),
        p10 = quantile(lead_ratio, 0.1, na.rm = TRUE),
        p90 = quantile(lead_ratio, 0.9, na.rm = TRUE),
        p5 = quantile(lead_ratio, 0.05, na.rm = TRUE),
        p95 = quantile(lead_ratio, 0.95, na.rm = TRUE),
        count = n(),
        .groups = "drop"
    )

write.csv(collapsed_pred_distr, "lead_distr.csv")




