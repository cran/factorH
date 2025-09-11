## ---- echo=FALSE--------------------------------------------------------------
pth <- system.file("partials/syntax.md", package = "factorH")
stopifnot(nzchar(pth))
txt <- readLines(pth, encoding = "UTF-8", warn = FALSE)
knitr::asis_output(paste0(paste(txt, collapse = "\n"), "\n\n"))

