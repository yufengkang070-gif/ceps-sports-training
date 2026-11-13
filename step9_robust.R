# =========================================================
# 第 9 步：稳健性检验
# (a) 匹配后样本 + 学校固定效应
# (b) 学校内部匹配（exact = ~schids）
# (c) 多重检验校正（Benjamini-Hochberg）
# 前提：已经运行过 step1_ceps (1).R 和 step2_psm.R（需要 s、m、md、att、covs）
# =========================================================
library(MatchIt)
library(fixest)

ys <- c("conf", "dep", "score", "cog", "peer")

# ---- (a) 匹配后样本 + 学校固定效应 ----
ra <- lapply(ys, function(y) feols(as.formula(paste(y, "~ sport | schids")),
                                   data = md, weights = ~weights, cluster = ~schids))
etable(ra, digits = 3, keep = "sport", headers = ys)

# ---- (b) 学校内部匹配 ----
mw <- matchit(as.formula(paste("sport ~", covs)), data = s, exact = ~schids,
              method = "nearest", distance = "glm", link = "linear.logit",
              caliper = 0.2, replace = TRUE, ratio = 1)
summary(mw)$nn          # 匹配成功的人数
mdw <- match.data(mw)
rb <- lapply(ys, function(y) feols(as.formula(paste(y, "~ sport")),
                                   data = mdw, weights = ~weights, cluster = ~schids))
etable(rb, digits = 3, keep = "sport", headers = ys)

# ---- (c) 多重检验校正：主要结果的 12 个检验 ----
# p 值来自 Table 2（m1-m5 等）、Table 3（att）和 Table 5（个体固定效应）
# 把你电脑上 R 结果的系数和标准误填进下面两列
b  <- c(0.106, -0.030, -2.433, -0.143, 0.042, 0.064, 0.010, -2.640, -0.146, 0.011, 0.064, -0.078)
se <- c(0.021,  0.020,  0.301,  0.025, 0.021, 0.028, 0.029,  0.390,  0.037, 0.028, 0.025,  0.035)
lab <- c("OLS conf","OLS dep","OLS score","OLS cog","OLS peer",
         "PSM conf","PSM dep","PSM score","PSM cog","PSM peer","FE conf","FE dep")
p <- 2 * pnorm(-abs(b / se))
data.frame(test = lab, p = round(p, 4), q_BH = round(p.adjust(p, "BH"), 4))
