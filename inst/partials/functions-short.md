## Function reference (short)

### srh.kway.full()
One-call pipeline: ANOVA on ranks, descriptives, post hocs, simple effects.

Syntax:
```r
srh.kway.full(y ~ A + B (+ C ...), data, max_levels = 30)
```
Returns list: anova, summary, posthoc_cells, posthoc_simple, meta.

### write.srh.kway.full.tsv()
Export full result to a single TSV.

Syntax:
```r
write.srh.kway.full.tsv(obj, file = "srh_kway_full.tsv", sep = "\t", na = "", dec = ".")
```
dec can be "." or ","; numeric output has no scientific notation; character tables are normalized for dec=",".

### srh.kway()
K-way SRH-style ANOVA on ranks (Type II SS), tie-corrected p-values. Reports Effect, Df, Sum Sq, H, Hadj, p.chisq, k, n, eta2H, eps2H.

### srh.effsize()
Two-factor SRH with effect sizes (eta2H, eps2H) from H.

### nonpar.datatable()
Compact descriptives (global rank means, medians, quartiles, IQR) for all main effects and interactions.

### srh.posthoc()
Dunn-Bonferroni pairwise matrices for a specified effect. Options: method, digits, triangular (lower/upper/full), numeric, sep.

### srh.posthocs()
Dunn-Bonferroni pairwise matrices for all effects. Returns a named list with P.adj tables for A, B, A:B, etc.

### srh.simple.posthoc()
Simple-effects post hocs within levels of conditioning factors. compare selects target factor; scope = "within" or "global".

### srh.simple.posthocs()
Enumerate all simple-effect configurations for a design. Returns a named list like "COMPARE(gender) | BY(condition x age_cat)".

### normality.datatable
Shapiro–Wilk normality per subgroup for all factor combinations; returns W, p, OK/NOT OK.

### residuals.normality.datatable
Shapiro–Wilk on global residuals from an ANOVA fitted to the selected factors; one test per model.

### residuals.cellwise.normality.datatable
Shapiro–Wilk on model residuals within each cell (matches ANOVA per-cell error normality).

### balance.chisq.datatable
Count-balance checks: homogeneity (1 factor), independence (2 factors), log-linear independence (3+); ChiSq, df, p, OK/NOT OK.

### levene.plan.datatable
Levene/Brown–Forsythe for full-plan cells (highest-order interaction); F, df, p, OK/NOT OK.

### plan.diagnostics
One-call diagnostics: raw normality, residuals cellwise normality, Levene (median), balance chi-square; prints overall summary and returns full tables.