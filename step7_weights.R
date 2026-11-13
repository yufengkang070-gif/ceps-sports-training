# =========================================================
# 第 7 步：稳健性检验——使用 CEPS 抽样权重（sweight）
# 前提：已经运行过 step1_ceps (1).R（需要其中生成的数据 d 和 df）
# =========================================================
library(fixest)

d$w <- df$sweight   # 学生个体权数

f <- function(y) as.formula(paste(y, "~ sport + acad +", controls, "| schids"))
wt <- lapply(c("conf", "dep", "score", "cog", "peer"), function(y)
  feols(f(y), data = d, weights = ~w, cluster = ~schids))

etable(wt, digits = 3, keep = c("sport", "acad"),
       headers = c("Confidence", "Depression", "Score", "Cognition", "Peer"))
