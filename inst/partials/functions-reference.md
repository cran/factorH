## Function reference

This document collects **call patterns** and **options** for each public function.  All formulas follow response ~ A + B (+ C ...) with **numeric** response and **factor** predictors.

**srh.kway.full()**

**Purpose:** one-call pipeline: ANOVA on ranks + descriptives + post hocs + simple effects.  
**Syntax:** srh.kway.full(y ~ A + B (+ C ...), data, max_levels = 30)

- Automatically chooses the ANOVA engine:
  - 1 factor: srh.kway()
  - 2 factors: srh.effsize()
  - 3+ factors: srh.kway()

- Returns a list: anova, summary, posthoc_cells, posthoc_simple, meta.

- Placeholders:
  - *not applicable* when a component does not apply (e.g., simple effects with 1 factor),
  - *failed...* when a sub-step errors out (keeps the pipeline alive).

Example:

    res <- srh.kway.full(liking ~ gender + condition + age_cat, data = mimicry)
    names(res)
    res$anova[1:3]
    head(res$summary)
    names(res$posthoc_cells)
    names(res$posthoc_simple)[1:3]
    res$meta

**Notes:**

- Predictors are coerced to factor internally; levels must be 2..max_levels.
- Missing values are removed **pairwise** on the variables in the formula.

**write.srh.kway.full.tsv()**

**Purpose:** export the srh.kway.full() result into a single TSV file for fast formatting.  
**Syntax:**  write.srh.kway.full.tsv(obj, file = "srh_kway_full.tsv", sep = "\t", na = "", dec = ".")

- dec = "." or "," controls the decimal mark.
- Numeric fields are written without scientific notation.
- Pretty-printed character tables (e.g., from post hocs) are normalized so that dec="," also affects numbers embedded in strings.

Example:

    # you can of course provide your own path to the file outside the temporary folder
	f <- file.path(tempdir(), "result.tsv")
    write.srh.kway.full.tsv(res, file = f, dec = ",")
    file.exists(f)

**srh.kway()**

**Purpose:** general k-way SRH-style ANOVA on ranks (Type II SS), tie-corrected p-values.
**Syntax:** srh.kway(y ~ A + B (+ C ...), data, clamp0 = TRUE, force_factors = TRUE, type = 2, ...)

- Reports: Effect, Df, Sum Sq, H, Hadj (tie correction), p.chisq, k, n, eta2H, eps2H.
- eta2H and eps2H are computed from **unadjusted H** (classical SRH practice).
- force_factors = TRUE coerces predictors to factor (recommended).
- type controls sums of squares. Default type = 2 (Type II SS). Set type = 3 for Type III SS (internally uses sum-to-zero contrasts; no global options changed).

Example:

    k3 <- srh.kway(liking ~ gender + condition + age_cat, data = mimicry)
    k3

One-factor check (KW-like):

    k1 <- srh.kway(liking ~ condition, data = mimicry)
    k1

Two factor (Type III SS):

    k3_ss3 <- srh.kway(liking ~ gender + condition, data = mimicry, type = 3)
    k3_ss3

**srh.effsize()**

**Purpose:** 2-way SRH table with effect sizes from H.  
**Syntax:** srh.effsize(y ~ A + B, data, clamp0 = TRUE, ...)

- Same columns as above but tailored to 2-way SRH.
- clamp0 = TRUE clamps small negatives to 0 for effect sizes.

Example:

    e2 <- srh.effsize(liking ~ gender + condition, data = mimicry)
    e2

**nonpar.datatable()**

**Purpose:** compact descriptive tables (APA-style), with **global rank means**, medians, quartiles, IQR.  
**Syntax:** nonpar.datatable(y ~ A + B (+ C ...), data, force_factors = TRUE)

- Returns rows for all **main effects** and all **interaction cells**  (constructed internally).
- Rank means are computed on **global ranks** (all observations ranked together), which matches how rank-based ANOVA effects are formed.

Example:

    dt <- nonpar.datatable(liking ~ gender + condition, data = mimicry)
    head(dt)

**srh.posthoc()**

**Purpose:** Dunn–Bonferroni **pairwise comparison matrix** for a specified effect.  
**Syntax:**  srh.posthoc(y ~ A (+ B + ...), data, method = "bonferroni", digits = 3, triangular = c("lower","upper","full"), numeric = FALSE, force_factors = TRUE, sep = ".")

- Builds a single grouping variable (cells) from the RHS factors and
  runs FSA::dunnTest.
- Returns a list of three matrices (as data.frames): Z, P.unadj, P.adj.
- triangular = "lower" (default) shows only the lower triangle; diagonal
  and upper triangle are blank.
- numeric = FALSE returns pretty-printed character tables; set TRUE to
  get numeric.

Example:

    ph <- srh.posthoc(liking ~ condition, data = mimicry)

**srh.posthocs()**

**Purpose:** Dunn–Bonferroni **pairwise matrices for all effects** (main and interactions).  
**Syntax:** srh.posthocs(y ~ A + B (+ C ...), data, ...)

- Iterates srh.posthoc over: A, B, C, A:B, A:C, B:C, A:B:C, ...
- Returns a named list: names are "A", "B", "A:B", etc.; each value is a
  P.adj matrix.

Example:

    phs <- srh.posthocs(liking ~ gender + condition + age_cat, data = mimicry)
    names(phs)
    phs[["gender:condition"]][1:5, 1:5]

**srh.simple.posthoc()**

**Purpose:** **Simple-effects** post hocs (pairwise comparisons **within** levels of conditioning factors).  
**Syntax:**  srh.simple.posthoc(y ~ A + B (+ C ...), data, compare = NULL, scope = c("within","global"), digits = 3)

- compare selects the target factor for pairwise comparisons (default:
  first RHS factor).
- **Scope**:
  - "within" (default): Bonferroni **within each by-table** (SPSS-like).
  - "global": one Bonferroni across **all** tests from all by-tables combined.
- Returns a data.frame with conditioning columns (BY), Comparison, Z, P.unadj, P.adj, m.tests, adj.note. An "adjustment" attribute describes the correction.

Example:

    simp <- srh.simple.posthoc(liking ~ gender + condition + age_cat, data = mimicry, compare = "gender", scope = "within")
    head(simp)

**srh.simple.posthocs()**

**Purpose:** enumerate **all simple-effect configurations** for a given design.  
**Syntax:** srh.simple.posthocs(y ~ A + B (+ C ...), data)

- For each target factor and each non-empty combination of the remaining factors as BY, runs srh.simple.posthoc(..., scope = "within").
- Returns a named list, names like COMPARE(gender) \| BY(condition x
  age_cat).

Example:

    sps <- srh.simple.posthocs(liking ~ gender + condition + age_cat, data = mimicry)
    head(names(sps), 6)
	
**normality.datatable**

**Purpose:** Shapiro–Wilk normality tests for the raw response within each subgroup for all non-empty combinations of RHS factors (main effects and interaction cells). 
**Syntax:** normality.datatable(y ~ A + B (+ C ...), data, force_factors = TRUE)  

- Returns Effect, factor columns, count, W, p.shapiro (fixed-format to 4 decimals, no scientific notation), and OK/NOT OK (p < 0.05 => NOT OK).  

Example:

    normality.datatable(liking ~ gender + condition + age_cat, data = mimicry)

**residuals.normality.datatable**

**Purpose:** Shapiro–Wilk normality tests on residuals from a classical ANOVA model fitted to the selected RHS factors (full factorial for those factors), one test per model (global residuals). 
**Syntax:** residuals.normality.datatable(y ~ A + B (+ C ...), data, force_factors = TRUE)

- Returns one row per Effect (A, B, A:B, …), with count, W, p.shapiro (4 decimals), OK/NOT OK. Use the cellwise variant below for the strict per-cell assumption.
- We have retained this feature for the purpose of recording older versions of the software, but according to the newer statistical literature it should not be used to determine the validity of a research plan.

Example:

    residuals.normality.datatable(liking ~ gender + condition + age_cat, data = mimicry)

**residuals.cellwise.normality.datatable**

**Purpose:** Shapiro–Wilk tests of residuals from an ANOVA model fitted to the selected RHS factors (full factorial), but tested separately within each cell defined by those factors. 
**Syntax:** residuals.cellwise.normality.datatable(y ~ A + B (+ C ...), data, force_factors = TRUE)  

- This matches the classical ANOVA assumption of normal errors per cell. Returns rows for every cell across all Effects, with count, W, p.shapiro (4 decimals), OK/NOT OK.  

Example:

    residuals.cellwise.normality.datatable(liking ~ gender + condition + age_cat, data = mimicry)

**balance.chisq.datatable**

**Purpose:** Count-balance diagnostics across design factors. 
**Syntax:** balance.chisq.datatable(y ~ A + B (+ C ...), data, force_factors = TRUE)  

- For one factor: chi-square test of homogeneity vs equal proportions. For two factors: chi-square test of independence on the contingency table. For three or more: log-linear independence model (Poisson; main effects only) assessed via deviance and df. Returns Effect, n, ChiSq (4 decimals), df, p.chisq (4 decimals), OK/NOT OK (p < 0.05 => NOT OK).  
- Note: The response is ignored; only RHS factors are used to build the tables.  

Example:

    balance.chisq.datatable(liking ~ gender + condition + age_cat, data = mimicry)

**levene.plan.datatable**

**Purpose:** Levene/Brown–Forsythe test for homogeneity of variances across the full-plan cells (highest-order interaction of RHS factors). 
**Syntax:** levene.plan.datatable(y ~ A + B (+ C ...), data, center = "median", force_factors = TRUE)  

- This is the primary variance-equality diagnostic for factorial ANOVA. Returns F, df.num, df.den, p (4 decimals), and OK/NOT OK (p < 0.05 => NOT OK).  

Examples:

    levene.plan.datatable(liking ~ gender + condition + age_cat, data = mimicry)  
    levene.plan.datatable(liking ~ gender + condition, data = mimicry, center = "mean")

**plan.diagnostics**

**Purpose:** Orchestrates all diagnostics in one call. 
**Syntax:** plan.diagnostics(y ~ A + B (+ C ...), data, force_factors = TRUE)  

- Runs raw normality (cellwise on the response), residuals cellwise normality, Levene/Brown–Forsythe for the full plan (median by default), and balance chi-square tests for all factor combinations. 
- Prints a concise console summary and returns full tables in a list.  
- Console summary: prints overall share of OK and overall status (OK only if 100% OK).  

Returned list:  

    $summary: percent_ok, ok_count, total, overall, plus per-type percentages:  
    percent_ok_normality_raw, percent_ok_residuals_cellwise, percent_ok_balance_chisq, percent_ok_levene_full_plan.  
    $results: normality_raw, residuals_cellwise_normality, levene_full_plan, balance_chisq.  

Examples:

    diag_out <- plan.diagnostics(liking ~ gender + condition + age_cat, data = mimicry)  
    diag_out$results$normality_raw  
    diag_out$results$residuals_cellwise_normality  
    diag_out$results$levene_full_plan  
    diag_out$results$balance_chisq  
    diag_out$summary


**Formula tips and pitfalls**

- Do **not** write A:B or A\*B. Use A + B (+ C ...); the package computes all necessary interaction structures internally.
- Response must be **numeric**. For Likert data, keep it numeric 1..k.
- Predictors should be **factors**. If they are not, they will be coerced.
- Coerce predictors to factor explicitly if needed

Example:

    #coercing
    mimicry$gender <- factor(mimicry$gender)
    mimicry$condition <- factor(mimicry$condition)

**Performance and reproducibility**

- Functions use ranks and Type II sums of squares (via car::Anova under the hood) and Dunn tests (FSA::dunnTest).
- P-values apply a standard tie correction factor for ranks; effect sizes are derived from unadjusted H (classical SRH practice).
- All outputs are plain data.frames and lists, easy to save and post-process.