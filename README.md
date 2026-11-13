# Extracurricular Sports Training, Future Confidence, and Depression among Chinese Adolescents

R code for the analyses in the manuscript "Extracurricular Sports Training, Future Confidence, and Depression among Chinese Adolescents: Evidence from Matching and Panel Fixed-Effects Models".

## Data

The analyses use the China Education Panel Survey (CEPS), 2013–2014 baseline and 2014–2015 follow-up student files:

- `cepsw1studentCN.dta`
- `cepsw2studentCN.dta`

CEPS is conducted by the National Survey Research Center at Renmin University of China. The data are **not** included in this repository. They are available upon registration and application through the Renmin University of China Social Science Data Resource Platform (CNSDA). After obtaining the data, place both files in the working directory set at the top of each script.

## Software

R (version 4.6.2 used) with the packages `haven`, `dplyr`, `fixest`, and `MatchIt`.

## Scripts (run in this order)

| Script | Content | Output in manuscript |
|---|---|---|
| `step1_ceps.R` | Builds variables; descriptive statistics; school fixed-effects models | Table 1, Table 2 |
| `step2_psm.R` | Propensity score matching (1:1 nearest neighbour, with replacement, caliper 0.2); subgroup matching by family economic status | Table 3, Table 4 (Panel B) |
| `step3_interaction.R` | Sports training × low economic status interactions | Table 4 (Panel A) |
| `step4_panel.R` | Two-wave individual fixed-effects models (Grade 7 cohort) | Table 5 |
| `step5_mediation.R` | Parallel mediation with school-level cluster bootstrap (500 replications) | Table 6 |
| `step6_figure2.R` | Propensity score distributions before and after matching | Figure 2 |
| `step7_weights.R` | Models weighted by CEPS sampling weights | Table A1 |
| `step8_lag.R` | Lagged-outcome models | Section 4.5 |
| `step9_robust.R` | Matching with school fixed effects; within-school matching; Benjamini–Hochberg correction | Table A2 |
| `step10_round4.R` | Matching with cognitive ability; starting vs. stopping; gender interactions; attrition | Table A3 |

`step1` must be run first; `step3`–`step10` use objects created by `step1`, `step2`, and (for `step10`) `step4`.

## Contact

   Yufeng Kang, yufengkang070@gmail.com
