## What is in the example dataset?

mimicry is a real study on the **chameleon effect** by Trzmielewska et al. (2025) \\doi{10.18290/rpsych2024.0019} about how `mimicry` vs other movement conditions affect `liking` of an interlocutor. Potential moderators include `gender` and `age` (with dichotomized age_cat, and a 3-level age_cat2). This makes it a natural playground for `multifactor` rank-based analyses. 

    table(mimicry$condition)
    table(mimicry$gender)
    table(mimicry$age_cat)
