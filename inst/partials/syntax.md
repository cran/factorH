## Formula syntax at a glance

All high-level functions use standard R **model formulas**:  
response ~ factorA + factorB + factorC

- \+ lists **main effects** - Interactions are handled internally. You do `not` need to write A:B  or A\*B.
- The `response` (left of ~) must be `numeric` (e.g., a Likert score coded as 1..5 stored as numeric).

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