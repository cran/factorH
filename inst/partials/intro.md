## What this package does (and why)

*factorH* provides a **simple, single-call workflow** for **multifactor
nonparametric, rank-based ANOVA** and publication-ready outputs:

- ANOVA-like table based on `ranks` (rooted in **Kruskal-Wallis H** and the 2x2 **Scheirer-Ray-Hare** extension),
- **effect sizes** computed directly from H
- **Dunn–Bonferroni** post hoc **comparison matrices**
- `simple-effects` post hocs (pairwise comparisons **within levels**
  of conditioning factors),

- compact **descriptive tables** and a **TSV writer** for quick formatting in Excel or a manuscript.

Why? Popular GUI stats tools do not offer a ready-made, **user-friendly multifactor rank-based** pipeline that mirrors standard H / SRH analyses in a way that is easy for beginners. factorH aims to fill that gap with **clear, R-like formula syntax** and a `one-command` report function.

The package is intentionally small: most users will only ever need:

- srh.kway.full(...) to compute everything
- write.srh.kway.full.tsv(...) to export the results into a single tab-separated file.

## Formula syntax at a glance

All high-level functions use standard R **model formulas**:  

    response ~ factorA + factorB + factorC

lists **main effects** - Interactions are handled internally. You do `not` need to write A:B  or A\*B. The `response` (left of ~) must be `numeric` (e.g., a Likert score coded as 1..5 stored as numeric).

Examples below use the included dataset mimicry.

    library(factorH)
    data(mimicry, package = "factorH")
    str(mimicry)

Predictors should be factors. If not, functions will coerce them.

**What is allowed?**

    # One factor (KW-style):  
      liking ~ condition
    
    # Two factors (SRH-style):  
      liking ~ gender + condition
    
    # Three or more factors (k-way):  
      liking ~ gender + condition + age_cat

You do `not` need to write gender:condition or gender\*condition. The
package will build all needed interactions internally when relevant.

## Numeric response (Likert note)

The response must be `numeric`. For Likert-type items (e.g., 1 = strongly disagree ... 5 = strongly agree), keep them numeric; rank-based tests are robust for such `ordinal-like` data.

If your Likert is accidentally a `factor` or `character`, coerce
safely:

    # if stored as character "1","2",...:
    mimicry$liking <- as.numeric(mimicry$liking)
    # if stored as factor with labels "1","2",...:
    mimicry$liking <- as.numeric(as.character(mimicry$liking))

## The one-call pipeline

The main function srh.kway.full() runs:

1.  ANOVA-like table on ranks
2.  descriptive summary
3.  post hoc matrices (Dunn–Bonferroni; P.adj)
4.  simple-effects post hocs (within-family Bonferroni).

For 2 factors:
    
    res2 <- srh.kway.full(liking ~ gender + condition, data = mimicry)
    names(res2)
    res2$anova
    head(res2$summary)
    names(res2$posthoc_cells)
    names(res2$posthoc_simple)[1:4]
    
 For 3 factors:
 
    res3 <- srh.kway.full(liking ~ gender + condition + age_cat, data =
    mimicry)
    res3$anova

**Export to a tab-separated file**

    f <- tempfile(fileext = ".tsv")
    write.srh.kway.full.tsv(res3, file = f, dec = ".") # decimal dot
    file.exists(f)

If you need `comma` as decimal mark:

    f2 <- tempfile(fileext = ".tsv")
    write.srh.kway.full.tsv(res3, file = f2, dec = ",") # decimal comma
    file.exists(f2)

The TSV contains clearly separated sections:  
\## SRH: EFFECTS TABLE, \## SUMMARY STATS, \## POSTHOC CELLS, \## SIMPLE EFFECTS, \## META.

## What is in the example dataset?

mimicry is a real study on the **chameleon effect (Trzmielewska, Duras, Juchacz & Rak, 2025)**: how `mimicry` vs other **movement conditions** affect `liking` of an interlocutor. Potential moderators include `gender` and `age` (with dichotomized age_cat, and a 3-level age_cat2). This makes it a natural playground for `multifactor` rank-based analyses. 

    table(mimicry\$condition)
    table(mimicry\$gender)
    table(mimicry\$age_cat)

## What the functions compute (high level)

- **srh.kway()**: rank-based k-way ANOVA table using Type II SS on ranks; p-values are tie-corrected; H is reported with and without the correction factor; effect sizes from unadjusted H.
- **srh.effsize()**: 2-way SRH table with effect sizes (eta2H, eps2H) computed from H.
- **nonpar.datatable()**: compact descriptive tables with **global ranks** (means of ranks per cell), medians, quartiles, IQR, etc., for all main effects and interactions.
- **srh.posthocs()**: Dunn–Bonferroni **pairwise matrices** (P.adj) for **all effects** (main and interactions).
- **srh.simple.posthoc() / srh.simple.posthocs()**: `simple-effects` pairwise comparisons **within levels** of conditioning factors (SPSS-like "within" scope by default).
- **srh.kway.full()**: orchestrates all of the above. 
- **write.srh.kway.full.tsv()**: exports everything into one TSV (with dot or comma decimal mark).

That is it. For most users, the intro ends here: **use srh.kway.full()** and export with **write.srh.kway.full.tsv()**.
