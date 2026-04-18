## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
md_path <- if (file.exists("../inst/dev/DEVELOPER_CONTRACTS.md")) {
  "../inst/dev/DEVELOPER_CONTRACTS.md"
} else {
  system.file("dev", "DEVELOPER_CONTRACTS.md", package = "tidyILD")
}

## ----contracts, results='asis'------------------------------------------------
if (!nzchar(md_path) || !file.exists(md_path)) {
  stop("DEVELOPER_CONTRACTS.md not found; expected inst/dev/DEVELOPER_CONTRACTS.md", call. = FALSE)
}
cat(knitr::knit_child(md_path, quiet = TRUE))

