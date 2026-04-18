## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)
has_tsibble <- requireNamespace("tsibble", quietly = TRUE)

## ----ingest, eval = has_tsibble-----------------------------------------------
suppressPackageStartupMessages({
  library(tidyILD)
  library(tsibble)
})
t <- seq.POSIXt(as.POSIXct("2020-01-01", tz = "UTC"), by = "hour", length.out = 4)
d <- tsibble(
  id = rep(1L, 4),
  t = t,
  y = 1:4,
  key = id,
  index = t
)
x <- ild_prepare(d)
class(x)

## ----meta, eval = has_tsibble-------------------------------------------------
str(ild_tsibble_meta(x), max.level = 1)

## ----spacing-bridge, eval = has_tsibble---------------------------------------
sp <- ild_spacing(x)
names(sp$tsibble)

## ----summary-ts, eval = has_tsibble-------------------------------------------
names(ild_summary(x))
ild_summary(x)$tsibble[c("key_vars", "index_var", "interval_format", "is_regular")]

## ----roundtrip, eval = has_tsibble--------------------------------------------
d2 <- ild_as_tsibble(x)
tsibble::interval(d2)
identical(tsibble::interval(d), tsibble::interval(d2))

