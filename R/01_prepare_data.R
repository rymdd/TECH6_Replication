# Prepare the estimation samples from regdata0.dta

source("R/00_setup.R")

raw <- read_dta("data/regdata0.dta") |> zap_labels() |> zap_formats() |> zap_label()

# the data has 81 province dummies (pd_1 ... pd_81), make one province variable
prov <- as.matrix(raw[, paste0("pd_", 1:81)])
prov[is.na(prov)] <- 0
stopifnot(rowSums(prov) %in% c(0, 1))
raw$prov81 <- ifelse(rowSums(prov) == 1, max.col(prov, ties.method = "first"), NA)

# main sample: complete cases on the two outcomes, the election variables and the controls
dat <- raw |>
  drop_na(all_of(c("hischshr1520f", "hischshr1520m", "i94", "iwm94", "pcode", controls, "prov81"))) |>
  mutate(prov81 = factor(prov81))

# the paper has 2,629 municipalities, 315 with an Islamic mayor, 67 provinces
cat("Main sample:", nrow(dat), "municipalities,", sum(dat$i94), "with an Islamic mayor,",
    n_distinct(dat$pcode), "provinces\n")
# sharp RD: i94 is 1 exactly when the margin is >= 0
print(table(islamic_mayor = dat$i94, margin_positive = dat$iwm94 >= 0))
saveRDS(dat, "data/derived/analysis.rds")

# sample for Table VII (2011 outcomes). the 2000 outcomes are not required here, with them N is 2337 and the paper has 2338
y_2011 <- c("ahischshr15_19f", "ahischshr15_19m", "ahischshr15_29f",
            "ahischshr15_29m", "nevm15_19f", "nevm15_19m")
dat11 <- raw |>
  drop_na(all_of(c(y_2011, "i94", "iwm94", "pcode", controls, "prov81"))) |>
  mutate(prov81 = factor(prov81))
cat("2011 sample:", nrow(dat11), "municipalities (paper: 2,338)\n")
saveRDS(dat11, "data/derived/analysis_2011.rds")
