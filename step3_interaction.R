# =========================================================
# 第 3 步：交互项检验——体育培训的效果在困难家庭是否显著更大
# 前提：已经运行过 step1_ceps (1).R（需要其中生成的数据 d）
# 模型：学校固定效应 + 学校聚类标准误，加入 sport × lowses 交互项
# =========================================================
library(fixest)

# 家庭经济困难 = 1（非常困难 / 比较困难），中等及以上 = 0
d$lowses <- ifelse(is.na(d$ses), NA, as.numeric(d$ses <= 2))

# 控制变量（不再放 ses，因为 lowses 已经由它生成）
ctrl <- "acad + female + age + grade9 + rural + pedu + only + board"

i_conf <- feols(as.formula(paste("conf ~ sport * lowses +", ctrl, "| schids")),
                data = d, cluster = ~schids)
i_dep  <- feols(as.formula(paste("dep  ~ sport * lowses +", ctrl, "| schids")),
                data = d, cluster = ~schids)

# 重点看 sport:lowses 这一行
etable(i_conf, i_dep, digits = 3, headers = c("Confidence", "Depression"),
       keep = c("sport", "lowses"))
