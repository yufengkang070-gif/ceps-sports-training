# =========================================================
# 第 1 步：读取 CEPS 基线数据，生成变量，做 Table 1 和 Table 2
# 数据：CEPS 2013-2014 基线学生数据 cepsw1studentCN.dta
# 运行方法：在 RStudio 中打开本文件，点右上角 "Source"
# =========================================================

# ---- 0. 第一次使用时安装软件包（只需运行一次，之后可以删掉这三行）----
if (!requireNamespace("haven", quietly = TRUE))  install.packages("haven")
if (!requireNamespace("fixest", quietly = TRUE)) install.packages("fixest")
if (!requireNamespace("dplyr", quietly = TRUE))  install.packages("dplyr")

library(haven)
library(fixest)
library(dplyr)

# ---- 1. 设置文件夹（改成你电脑上放数据的位置，路径里用 / 不要用 \）----
setwd("C:/ceps/w1")
df <- read_dta("cepsw1studentCN.dta")
df <- zap_labels(df)   # 去掉标签，变成普通数字

# ---- 2. 生成变量 ----
d <- df %>%
  transmute(
    schids, ids,
    # 自变量与对照
    sport  = b1910,                                         # 体育类兴趣班（1=参加）
    acad   = ifelse(is.na(b1910), NA,
                    pmax(b1901, b1902, b1903, b1904, na.rm = TRUE)),  # 学科补习
    # 因变量
    dep    = rowMeans(cbind(a1801, a1802, a1803, a1804, a1805)),     # 抑郁 1-5，越高越抑郁
    score  = rowMeans(cbind(stdchn, stdmat, stdeng)),                # 语数英标准化成绩
    cog    = cog3pl,                                                 # 认知能力
    # 中介变量
    peer   = rowMeans(cbind(c1706, c1707)),                          # 同伴关系 1-4
    conf   = c25,                                                    # 对未来的信心 1-4
    # 控制变量
    female = 1 - stsex,
    age    = 2013 - a02a,
    grade9,
    rural  = sthktype,
    ses    = steco_5c,
    pedu   = stprhedu,
    only   = as.numeric(stonly == 1),
    board  = stbrd
  )

controls <- "female + age + grade9 + rural + ses + pedu + only + board"

# ---- 3. Table 1：描述性统计 ----
vars <- c("dep","score","cog","sport","acad","peer","conf",
          "female","age","grade9","rural","ses","pedu","only","board")
table1 <- data.frame(
  Variable   = vars,
  N          = sapply(vars, function(v) sum(!is.na(d[[v]]))),
  Mean       = sapply(vars, function(v) mean(d[[v]], na.rm = TRUE)),
  SD         = sapply(vars, function(v) sd(d[[v]], na.rm = TRUE)),
  Mean_sport = sapply(vars, function(v) mean(d[[v]][d$sport == 1], na.rm = TRUE)),
  Mean_none  = sapply(vars, function(v) mean(d[[v]][d$sport == 0], na.rm = TRUE))
)
print(round(table1[,-1], 3))
write.csv(table1, "table1_descriptives_R.csv", row.names = FALSE)

# ---- 4. Table 2：学校固定效应 + 学校聚类标准误 ----
f <- function(y, x) as.formula(paste(y, "~", x, "+", controls, "| schids"))
m1 <- feols(f("dep",   "sport"),        data = d, cluster = ~schids)
m2 <- feols(f("dep",   "sport + acad"), data = d, cluster = ~schids)
m3 <- feols(f("score", "sport"),        data = d, cluster = ~schids)
m4 <- feols(f("score", "sport + acad"), data = d, cluster = ~schids)
m5 <- feols(f("cog",   "sport + acad"), data = d, cluster = ~schids)
etable(m1, m2, m3, m4, m5, digits = 3)

# ---- 5. 机制（中介路径）初步检验 ----
p1 <- feols(f("peer", "sport + acad"), data = d, cluster = ~schids)
p2 <- feols(f("conf", "sport + acad"), data = d, cluster = ~schids)
p3 <- feols(f("dep",  "sport + acad + peer + conf"), data = d, cluster = ~schids)
etable(p1, p2, p3, digits = 3)

# ---- 6. 异质性：家庭经济困难 vs 中等及以上 ----
d$lowses <- ifelse(is.na(d$ses), NA, as.numeric(d$ses <= 2))
h1 <- feols(f("dep", "sport + acad"), data = subset(d, lowses == 1), cluster = ~schids)
h2 <- feols(f("dep", "sport + acad"), data = subset(d, lowses == 0), cluster = ~schids)
etable(h1, h2, digits = 3, headers = c("Low SES", "Mid/High SES"))
