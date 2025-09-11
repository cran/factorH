#' Write full SRH pipeline result to a TSV file
#'
#' Exports the result of \code{\link{srh.kway.full}} into a single,
#' tab-separated text file, in the order:
#' \emph{ANOVA} > \emph{SUMMARY} > \emph{POSTHOC CELLS} > \emph{SIMPLE EFFECTS} > \emph{META}.
#' Supports choosing the decimal mark for numeric values.
#'
#' @param obj A list produced by \code{\link{srh.kway.full}}.
#' @param file Path to the output TSV file. Default \code{"srh_kway_full.tsv"}.
#' @param sep Field separator (default tab \code{"\\t"}).
#' @param na String to use for missing values (default empty string).
#' @param dec Decimal mark for numbers: dot \code{"."} (default) or comma \code{","}.
#'
#' @details
#' Each section is preceded by a header line (e.g., \code{## SRH: EFFECTS TABLE}).
#' For post hoc sections, each effect/table is prefixed with a subheader
#' (e.g., \code{### posthoc_cells: gender:condition}). For simple-effect tables,
#' the attribute \code{"adjustment"} (if present) is written as a comment line
#' beginning with \code{"# "}.
#'
#' Components that are not applicable (e.g., simple effects in 1-factor designs)
#' or failed computations are written as literal one-line messages.
#'
#' @return (Invisibly) the normalized path to \code{file}.
#'
#' @examples
#' \donttest{
#' data(mimicry, package = "factorH")
#' res <- srh.kway.full(liking ~ gender + condition, data = mimicry)
#'
#' # Write to a temporary file (CRAN-safe)
#' f <- tempfile(fileext = ".tsv")
#' write.srh.kway.full.tsv(res, file = f, dec = ".")
#' file.exists(f)
#' }
#'
#' @export
write.srh.kway.full.tsv <- function(obj, file = "srh_kway_full.tsv",
                                    sep = "\t", na = "", dec = ".") {
  if (!is.list(obj) || is.null(obj$anova) || is.null(obj$summary) ||
      is.null(obj$posthoc_cells) || is.null(obj$posthoc_simple)) {
    stop("Object does not look like a result from srh.kway.full().")
  }
  if (!dec %in% c(".", ",")) stop("Argument 'dec' must be '.' or ','.")

  con <- file(file, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)

  write_hdr   <- function(title) cat("## ",  title, "\n", sep = "", file = con)
  write_sub   <- function(title) cat("### ", title, "\n", sep = "", file = con)
  write_blank <- function()      cat("\n", file = con)

  # format scalar with chosen decimal mark (META lines)
  fmt_num <- function(x) {
    if (is.na(x)) return(na)
    format(x, scientific = FALSE, trim = TRUE, decimal.mark = dec)
  }

write_df <- function(df) {
  # Ensure a plain data.frame
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)

  # 1) Numeric columns: format without scientific notation and with chosen decimal mark
  is_num <- vapply(df, is.numeric, logical(1))
  if (any(is_num)) {
    df[is_num] <- lapply(df[is_num], function(x)
      format(x, scientific = FALSE, trim = TRUE, decimal.mark = dec))
  }

  # 2) Character columns: if dec == "," replace only dots between digits with commas
  #    This catches pretty-printed tables (e.g., from srh.posthocs) that are character.
  if (dec == ",") {
    is_chr <- vapply(df, is.character, logical(1))
    if (any(is_chr)) {
      df[is_chr] <- lapply(df[is_chr], function(x) {
        ifelse(
          nzchar(x),
          gsub("(?<=\\d)\\.(?=\\d)", ",", x, perl = TRUE),
          x
        )
      })
    }
  }

  # Write the table (values are now strings for numerics too; NA uses 'na')
  utils::write.table(df, file = con, sep = sep, quote = FALSE,
                     row.names = TRUE, col.names = NA, na = na, dec = dec)
  write_blank()
}

  write_any <- function(x) {
    if (is.character(x) && length(x) == 1L && grepl("^\\[", x)) {
      # placeholders like "[not applicable]" or "[failed] <msg>"
      cat(x, "\n\n", file = con, sep = "")
    } else if (is.data.frame(x)) {
      write_df(x)
    } else if (is.list(x)) {
      # try to coerce lists/data-like objects
      write_df(as.data.frame(x))
    } else {
      write_df(as.data.frame(x))
    }
  }

  # 1) ANOVA / EFFECTS
  write_hdr("SRH: EFFECTS TABLE")
  write_any(obj$anova)

  # 2) SUMMARY STATS
  write_hdr("SUMMARY STATS (nonpar.datatable)")
  write_any(obj$summary)

  # 3) POSTHOC CELLS (srh.posthocs) - P.adj tables
  write_hdr("POSTHOC CELLS (srh.posthocs) - P.adj tables")
  pc <- obj$posthoc_cells
  if (is.list(pc) && length(pc)) {
    nms <- names(pc); if (is.null(nms)) nms <- paste0("Effect_", seq_along(pc))
    for (i in seq_along(pc)) {
      write_sub(paste0("posthoc_cells: ", nms[i]))
      write_any(pc[[i]])
    }
  } else {
    write_sub("No tables (empty list or failure)")
    write_blank()
  }

  # 4) SIMPLE EFFECTS (srh.simple.posthocs)
  write_hdr("SIMPLE EFFECTS (srh.simple.posthocs) - within-family Bonferroni")
  ps <- obj$posthoc_simple
  if (is.character(ps)) {
    # e.g., "[not applicable]" for 1-factor designs or "[failed] ..."
    write_sub(ps); write_blank()
  } else if (is.list(ps) && length(ps)) {
    nms <- names(ps); if (is.null(nms)) nms <- paste0("Simple_", seq_along(ps))
    for (i in seq_along(ps)) {
      write_sub(nms[i])
      adj_note <- attr(ps[[i]], "adjustment")
      if (!is.null(adj_note)) cat("# ", adj_note, "\n", file = con, sep = "")
      write_any(ps[[i]])
    }
  } else {
    write_sub("No tables (empty list or failure)")
    write_blank()
  }

  # 5) META
  write_hdr("META")
  meta <- obj$meta
  if (!is.null(meta$n)) {
    cat("n", sep, meta$n, "\n", file = con, sep = "")
  }
  if (!is.null(meta$levels)) {
    cat("levels", file = con)
    for (nm in names(meta$levels)) cat(sep, paste0(nm, "=", meta$levels[[nm]]), file = con, sep = "")
    cat("\n", file = con)
  }
  if (!is.null(meta$empty_cells)) {
    ec <- meta$empty_cells
    cat("empty_cells",
        sep, paste0("n_cells=", ec$n_cells),
        sep, paste0("n_empty=", ec$n_empty),
        sep, paste0("prop_empty=", fmt_num(ec$prop_empty)),
        "\n", file = con, sep = "")
  }
  if (!is.null(meta$call)) {
    cat("call", sep, paste(deparse(meta$call), collapse = ""), "\n", file = con, sep = "")
  }
  write_blank()

  invisible(normalizePath(file))
}
