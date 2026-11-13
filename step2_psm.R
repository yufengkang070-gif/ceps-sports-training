# =========================================================
# 第 2 步：倾向得分匹配（PSM）
# 前提：已经运行过 step1_ceps (1).R（需要其中生成的数据 d）
# 方法：1:1 最近邻匹配，有放回，卡尺 = 0.2 倍 logit 倾向得分标准差
# =========================================================

# ---- 0. 第一次使用时安装软件包（只需运行一次）----
if (!requireNamespace("MatchIt", quietly = TRUE)) install.packages("MatchIt")
library(MatchIt)
library(fixest)

covs <- "female + age + grade9 + rural + ses + pedu + only + board + acad"

# ---- 1. 匹配（全样本）----
s <- subset(d, !is.na(sport) & complete.cases(d[, c("female","age","grade9","rural",
                                                     "ses","pedu","only","board","acad")]))
m <- matchit(as.formula(paste("sport ~", covs)), data = s,
             method = "nearest", distance = "glm", link = "linear.logit",
             caliper = 0.2, replace = TRUE, ratio = 1)

# ---- 2. 平衡性检验：匹配后 Std. Mean Diff 绝对值应小于 0.1 ----
summary(m)

# ---- 3. 匹配后的平均处理效应（ATT），标准误按学校聚类 ----
md <- match.data(m)
att <- lapply(c("dep","score","cog","conf","peer"), function(y)
  feols(as.formula(paste(y, "~ sport")), data = md, weights = ~weights, cluster = ~schids))
etable(att, digits = 3, headers = c("Depression","Score","Cognition","Confidence","Peer"))

# ---- 4. 分组：家庭经济困难 vs 中等及以上 ----
psm_group <- function(sub) {
  mm <- matchit(as.formula(paste("sport ~", covs)), data = sub,
                method = "nearest", distance = "glm", link = "linear.logit",
                caliper = 0.2, replace = TRUE, ratio = 1)
  dd <- match.data(mm)
  lapply(c("dep","conf"), function(y)
    feols(as.formula(paste(y, "~ sport")), data = dd, weights = ~weights, cluster = ~schids))
}
low  <- psm_group(subset(s, ses <= 2))
high <- psm_group(subset(s, ses >= 3))
etable(c(low, high), digits = 3,
       headers = c("Dep_LowSES","Conf_LowSES","Dep_MidHigh","Conf_MidHigh"))
