# tidyILD

Tidyverse-native toolkit for **intensive longitudinal data (ILD)**.

## Install

```r
remotes::install_github("alitovchenko/tidyILD")
```

## Quick start

```r
library(tidyILD)

# Prepare: validate time structure, add .ild_* columns and metadata
d <- data.frame(
  id = rep(1:3, each = 5),
  time = rep(as.POSIXct(0:4 * 3600, origin = "1970-01-01"), 3),
  mood = rnorm(15)
)
x <- ild_prepare(d, id = "id", time = "time", gap_threshold = 7200)

# Inspect
ild_summary(x)

# Within-between decomposition
x <- ild_center(x, mood)

# Spacing-aware lags
x <- ild_lag(x, mood, mode = "gap_aware", max_gap = 7200)
```

## Pipeline

- `ild_prepare()` — encode longitudinal structure, spacing, gaps
- `ild_summary()` — one-shot summary
- `ild_center()` — person-mean centering (WP/BP)
- `ild_lag()` — index or gap-aware lags
- `ild_spacing_class()` — regular-ish vs irregular-ish
- `ild_missing_pattern()` — missingness by person/variable
- `ild_lme()` — mixed-effects model (lmer or nlme with AR1/CAR1)
- `ild_diagnostics()` — residual ACF, residuals vs fitted/time
- `ild_plot()` — trajectory, heatmap, gaps, fitted vs observed, residual ACF
- `ild_simulate()` — simple simulated ILD for examples
- `ild_check_lags()` — audit lag columns (valid vs invalid)
- `ema_example` — built-in dataset (`data(ema_example)`)
- **broom**: Load `broom.mixed` for `tidy(fit)` and `augment(fit)` on `ild_lme` fits.

## Vignettes

- **From raw data to model**: full pipeline with `ild_prepare()` through `ild_lme()` and `ild_plot()`.
- **Within-between decomposition and irregular spacing**: centering and gap-aware lags.
- **Glossary and quick-start**: function glossary and checklist.

## License

MIT.
