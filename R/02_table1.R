# Table I: summary statistics

source("R/00_setup.R")
dat <- readRDS("data/derived/analysis.rds")

# buyuk is used as a control but is not in the paper's Table I
labels <- c(hischshr1520f = "Women 15-20 with high school, 2000",
            hischshr1520m = "Men 15-20 with high school, 2000",
            vshr_islam1994 = "Islamic vote share 1994",
            partycount = "Number of parties 1994",
            lpop1994 = "Log population 1994",
            ageshr19 = "Population share below 19",
            ageshr60 = "Population share above 60",
            sexr = "Gender ratio",
            shhs = "Household size",
            merkezi = "District center",
            merkezp = "Province center",
            subbuyuk = "Sub-metro center")

# the difference has the classical standard error (t-test with equal variances), as in the paper
table1 <- dat |>
  pivot_longer(all_of(names(labels)), names_to = "variable") |>
  mutate(variable = factor(variable, levels = names(labels))) |>
  group_by(variable) |>
  summarise(mean = mean(value), sd = sd(value),
            mean_islamic = mean(value[i94 == 1]), sd_islamic = sd(value[i94 == 1]),
            mean_secular = mean(value[i94 == 0]), sd_secular = sd(value[i94 == 0]),
            difference = mean_islamic - mean_secular,
            se = t.test(value[i94 == 1], value[i94 == 0], var.equal = TRUE)$stderr) |>
  mutate(variable = labels[as.character(variable)])

print(table1, width = Inf)
cat("N =", nrow(dat), "of which", sum(dat$i94), "with an Islamic mayor\n")

kable(table1, format = "latex", booktabs = TRUE, digits = 3, linesep = "",
      col.names = c("", "All", "(SD)", "Islamic", "(SD)", "Secular", "(SD)", "Difference", "(SE)")) |>
  writeLines("output/tables/table1.tex")
