# Figures 1-4. bins and windows are the same as in graphs_R4_final.do

source("R/00_setup.R")
dat <- readRDS("data/derived/analysis.rds")
# the do-file uses the whole data file for figures 2-4, not only the 2629 sample
raw <- read_dta("data/regdata0.dta") |> zap_labels() |> filter(!is.na(iwm94))


# Figure 1
d1 <- dat |>
  filter(vshr_islam1994 < 0.75) |>
  mutate(close = abs(iwm94) < 0.02) |>
  arrange(close)

fig1 <- ggplot(d1, aes(vshr_islam1994, iwm94, colour = close)) +
  geom_point(size = 0.5) +
  geom_abline(intercept = -1, slope = 2, colour = "grey40", linewidth = 0.3) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.2) +
  scale_colour_manual(values = c(`FALSE` = "grey65", `TRUE` = "black")) +
  labs(x = "Islamic vote share", y = "Islamic win margin") +
  theme_paper()
save_fig(fig1, "figure1", 4.5, 3.5)
cat("Figure 1:", nrow(d1), "municipalities with vote share below 0.75,", sum(d1$close), "with |margin| < 0.02\n")


# Figure 2
fig2a <- ggplot(raw, aes(iwm94)) +
  geom_histogram(aes(y = after_stat(100 * count / sum(count))),
                 breaks = seq(min(raw$iwm94), max(raw$iwm94), length.out = 100),
                 fill = "grey60", colour = "grey20", linewidth = 0.1) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3) +
  labs(title = "(a) Histogram", x = "Islamic win margin", y = "Percent") +
  theme_paper()

# the paper uses the McCrary test, I use rddensity from the course instead (same idea, but the test statistic is not the same as in the paper)
rdd <- rddensity(X = dat$iwm94, c = 0)
fig2b <- rdplotdensity(rdd, X = dat$iwm94, noPlot = TRUE)$Estplot +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3) +
  labs(title = "(b) Density test (rddensity)", x = "Islamic win margin", y = "Density") +
  theme_paper()
save_fig(fig2a + fig2b, "figure2", 7, 3)
cat("Figure 2 (a):", nrow(raw), "municipalities with a win margin\n")
summary(rdd)


# RD plot for figures 3 and 4: bin means + linear fit with 95% CI on each side
rd_plot <- function(data, v, bin_width, xlim, title = NULL) {
  d <- data |>
    filter(!is.na(.data[[v]]), abs(iwm94) <= xlim) |>
    mutate(x = iwm94, outcome = .data[[v]], right = x >= 0, bin = floor(x / bin_width))
  bins <- d |>
    group_by(bin) |>
    summarise(x = mean(x), outcome = mean(outcome))
  ggplot(d, aes(x, outcome, group = right)) +
    geom_smooth(method = "lm", formula = y ~ x, colour = "black", fill = "grey80", linewidth = 0.5) +
    geom_point(data = bins, aes(x, outcome), size = 0.9, inherit.aes = FALSE) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3) +
    labs(title = title, x = "Islamic win margin 1994", y = NULL) +
    theme_paper()
}


# Figure 3: covariates at the cutoff
covs3 <- c("vshr_islam1994", "ageshr60", "ageshr19", "lpop1994", "sexr", "anyc", "partycount", "shhs")
names3 <- c("Islamic vote share", "Age 60+", "Age 19-", "Log population", "Gender ratio",
            "Any center municipality", "Number of parties", "Household size")

plots3 <- list()
for (i in 1:8) {
  plots3[[i]] <- rd_plot(raw, covs3[i], bin_width = 0.08, xlim = 0.3,
                         title = paste0("(", letters[i], ") ", names3[i]))
}
save_fig(wrap_plots(plots3, ncol = 4), "figure3", 10, 5)

# balance check: RD with each covariate as outcome, women's bandwidth, no controls
balance <- list()
for (i in 1:8) {
  balance[[i]] <- rd_reg(dat, covs3[i], h_women)
}
etable(balance, keep = "%^i94$")


# Figure 4: outcomes in 2000 and 1990
fig4 <- (rd_plot(raw, "hischshr1520f", 0.04, 0.5, "A. Women in 2000") +
           rd_plot(raw, "hischshr1520m", 0.04, 0.5, "B. Men in 2000") +
           rd_plot(raw, "c90hischshr1520f", 0.04, 0.5, "C. Women in 1990") +
           rd_plot(raw, "c90hischshr1520m", 0.04, 0.5, "D. Men in 1990")) +
  plot_layout(ncol = 2) +
  plot_annotation(caption = "Share of 15-20 year-olds with high school degree")
save_fig(fig4, "figure4", 7, 5.5)
cat("Figure 4:", sum(!is.na(raw$hischshr1520f)), "municipalities with the 2000 outcome,",
    sum(!is.na(raw$c90hischshr1520f)), "with the 1990 outcome\n")
