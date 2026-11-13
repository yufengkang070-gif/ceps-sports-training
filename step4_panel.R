# =========================================================
# 第 4 步：两轮追踪数据的个体固定效应模型（七年级队列）
# 思路：同一个学生，从"不参加"变成"参加"体育培训（或相反）时，
#       他自己的信心和抑郁是否随之变化。个体固定效应去掉了所有
#       不随时间变化的个人和家庭特征（性别、家庭背景、性格等）。
# 需要两个数据文件放在 C:/ceps/w1：
#   cepsw1studentCN.dta（基线）和 cepsw2studentCN.dta（追访）
# =========================================================
library(haven)
library(dplyr)
library(fixest)

setwd("C:/ceps/w1")
w1 <- zap_labels(read_dta("cepsw1studentCN.dta"))
w2 <- zap_labels(read_dta("cepsw2studentCN.dta"))

# 两轮都有的 4 道抑郁题：沮丧、不快乐、生活没意思、悲伤（1-5 分）
a <- w1 %>% transmute(
  ids, schids, grade9, ses = steco_5c, wave = 0,
  sport = b1910,
  acad  = ifelse(is.na(b1910), NA, pmax(b1901, b1902, b1903, b1904, na.rm = TRUE)),
  dep4  = rowMeans(cbind(a1801, a1803, a1804, a1805)),
  conf  = c25)

b <- w2 %>% transmute(
  ids, wave = 1,
  sport = w2b1110,
  acad  = ifelse(is.na(w2b1110), NA, pmax(w2b1101, w2b1102, w2b1103, w2b1104, na.rm = TRUE)),
  dep4  = rowMeans(cbind(w2c2501, w2c2503, w2c2504, w2c2506)),
  conf  = w2b21) %>%
  left_join(a %>% select(ids, schids, grade9, ses), by = "ids")

p <- bind_rows(filter(a, ids %in% b$ids), b) %>%
  filter(grade9 == 0) %>%
  group_by(ids) %>% mutate(low = as.numeric(first(ses) <= 2)) %>% ungroup()

# 有多少学生在两轮之间改变了参加状态
sw <- p %>% group_by(ids) %>% summarise(s0 = sport[wave == 0][1], s1 = sport[wave == 1][1])
table(start = sw$s0, end = sw$s1)

# ---- 主模型：个体固定效应 + 时间（wave）效应，标准误按学校聚类 ----
fe_conf <- feols(conf ~ sport + acad + wave | ids, data = p, cluster = ~schids)
fe_dep  <- feols(dep4 ~ sport + acad + wave | ids, data = p, cluster = ~schids)

# ---- 异质性：效果在困难家庭是否更大 ----
fe_conf_i <- feols(conf ~ sport + sport:low + acad + wave + wave:low | ids, data = p, cluster = ~schids)
fe_dep_i  <- feols(dep4 ~ sport + sport:low + acad + wave + wave:low | ids, data = p, cluster = ~schids)

etable(fe_conf, fe_dep, fe_conf_i, fe_dep_i, digits = 3,
       headers = c("Conf", "Dep", "Conf x Low", "Dep x Low"))
