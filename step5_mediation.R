# =========================================================
# 第 5 步：中介分析——体育培训 → 信心 / 同伴关系 → 抑郁
# 前提：已经运行过 step1_ceps (1).R（需要其中生成的数据 d）
# 方法：学校固定效应模型；间接效应 = a × b；
#       按学校整群抽样 Bootstrap 500 次，得到 95% 置信区间
# 运行时间：约 2-5 分钟，请耐心等待
# =========================================================
library(fixest)

ctrl <- "acad + female + age + grade9 + rural + ses + pedu + only + board"
vars <- c("schids","dep","sport","acad","conf","peer",
          "female","age","grade9","rural","ses","pedu","only","board")
s <- na.omit(d[, vars])

est <- function(df) {
  a1 <- coef(feols(as.formula(paste("conf ~ sport +", ctrl, "| schids")), data = df))["sport"]
  a2 <- coef(feols(as.formula(paste("peer ~ sport +", ctrl, "| schids")), data = df))["sport"]
  m  <- coef(feols(as.formula(paste("dep ~ sport + conf + peer +", ctrl, "| schids")), data = df))
  tt <- coef(feols(as.formula(paste("dep ~ sport +", ctrl, "| schids")), data = df))["sport"]
  c(ind_conf = unname(a1 * m["conf"]), ind_peer = unname(a2 * m["peer"]),
    direct = unname(m["sport"]), total = unname(tt))
}

point <- est(s)

set.seed(1)
sch <- unique(s$schids)
boot <- t(replicate(500, {
  pick <- sample(sch, length(sch), replace = TRUE)
  bs <- do.call(rbind, lapply(seq_along(pick), function(i) {
    x <- s[s$schids == pick[i], ]; x$schids <- i; x }))
  est(bs)
}))

result <- data.frame(
  effect   = names(point),
  estimate = round(point, 4),
  ci_low   = round(apply(boot, 2, quantile, 0.025), 4),
  ci_high  = round(apply(boot, 2, quantile, 0.975), 4))
print(result, row.names = FALSE)
cat("通过信心解释的比例：", round(point["ind_conf"] / point["total"], 2), "\n")
