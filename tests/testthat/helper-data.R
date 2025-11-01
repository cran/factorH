# używamy wbudowanego datasetu pakietu
if (!exists("mimicry")) {
  data("mimicry", package = "factorH")
}
testthat::skip_if_not(exists("mimicry"), "Dataset 'mimicry' not found")
