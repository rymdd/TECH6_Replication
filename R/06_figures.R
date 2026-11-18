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


# McCrary (2008) density test, follows DCdensity.ado from the replication package (rdd, the R version of it, is not on CRAN anymore).
# histogram of x with a bin edge at the cutoff, then a weighted line through the bin heights on each side, compared at the cutoff
mccrary_test <- function(x, cutoff = 0) {
  n <- length(x)
  b <- 2 * sd(x) / sqrt(n)  # McCrary's default bin size

  # bin k goes from cutoff + k*b to cutoff + (k+1)*b, empty bins are kept with height 0
  bin <- floor((x - cutoff) / b)
  k <- min(bin):max(bin)
  bins <- tibble(mid = cutoff + (k + 0.5) * b,
                 height = tabulate(bin - min(bin) + 1, nbins = length(k)) / (n * b))

  # default bandwidth: 4th order polynomial through the bin heights on each side, h from its 2nd derivative, average of the two sides
  h_side <- c()
  for (right in c(FALSE, TRUE)) {
    d <- bins |> filter((mid >= cutoff) == right)
    fit <- lm(height ~ poly(mid, 4, raw = TRUE), data = d)
    beta <- coef(fit)
    f2 <- 2 * beta[3] + 6 * beta[4] * d$mid + 12 * beta[5] * d$mid^2
    h_side <- c(h_side, 3.348 * (sigma(fit)^2 * max(abs(d$mid - cutoff)) / sum(f2^2))^(1 / 5))
  }
  h <- mean(h_side)
  # the ado file also adds empty bins beyond the data, they only get weight if the data ends inside the bandwidth
  stopifnot(min(x) < cutoff - h, max(x) > cutoff + h)

  # local linear fit with triangular weights on each side, the intercept is the density at the cutoff
  fhat <- c()
  for (right in c(FALSE, TRUE)) {
    d <- bins |> filter((mid >= cutoff) == right, abs(mid - cutoff) < h)
    fit <- lm(height ~ I(mid - cutoff), data = d, weights = 1 - abs(mid - cutoff) / h)
    fhat <- c(fhat, coef(fit)[[1]])
  }

  # theta is the log difference of the two densities at the cutoff, se as in McCrary (2008)
  theta <- log(fhat[2]) - log(fhat[1])
  se <- sqrt(1 / (n * h) * 24 / 5 * (1 / fhat[1] + 1 / fhat[2]))
  list(bins = bins, b = b, h = h, theta = theta, se = se, p = 2 * pnorm(-abs(theta / se)))
}


# Figure 2
fig2a <- ggplot(raw, aes(iwm94)) +
  geom_histogram(aes(y = after_stat(100 * count / sum(count))),
                 breaks = seq(min(raw$iwm94), max(raw$iwm94), length.out = 100),
                 fill = "grey60", colour = "grey20", linewidth = 0.1) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3) +
  labs(title = "(a) Histogram", x = "Islamic win margin", y = "Percent") +
  theme_paper()

# McCrary test with the sample of the do-file: municipalities with the women's outcome and |margin| < 0.6, default bin size and bandwidth
x2 <- raw |>
  filter(!is.na(hischshr1520f), abs(iwm94) < 0.6) |>
  pull(iwm94)
mccrary <- mccrary_test(x2)
cat("McCrary test: bin size", fmt(mccrary$b, 4), "bandwidth", fmt(mccrary$h), "estimate", fmt(mccrary$theta),
    "s.e.", fmt(mccrary$se), "p", fmt(mccrary$p), "\n")

# the paper draws a smoother over the whole range, I only draw the two lines that the test compares at zero
bins2 <- mccrary$bins |> filter(abs(mid) < 0.5)
lines2 <- bins2 |>
  filter(abs(mid) < mccrary$h) |>
  mutate(right = mid >= 0, w = 1 - abs(mid) / mccrary$h)
fig2b <- ggplot(bins2, aes(mid, height)) +
  geom_point(colour = "grey50", size = 0.6) +
  geom_smooth(data = lines2, aes(weight = w, group = right), method = "lm", formula = y ~ x,
              se = FALSE, colour = "black", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3) +
  labs(title = "(b) McCrary density test", x = "Islamic win margin", y = "Density",
       caption = paste0("Discontinuity est. = ", fmt(mccrary$theta), ", s.e. = ", fmt(mccrary$se))) +
  theme_paper()
save_fig(fig2a + fig2b, "figure2", 7, 3)
cat("Figure 2 (a):", nrow(raw), "municipalities with a win margin, (b):", length(x2),
    "with the women's outcome and |margin| < 0.6\n")


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
            "Any centre municipality", "Number of parties", "Household size")

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
