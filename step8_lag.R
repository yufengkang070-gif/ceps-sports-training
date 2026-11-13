# =========================================================
# 第 8 步：滞后模型——基线体育培训 → 一年后的信心和抑郁（控制基线水平）
# 前提：已经运行过 step1_ceps (1).R（需要其中的 d 和 controls）
# 需要 cepsw2studentCN.dta 也在 C:/ceps/w1
# =========================================================
library(haven)
library(dplyr)
library(fixest)

w1 <- zap_labels(read_dta("cepsw1studentCN.dta"))
w2 <- zap_labels(read_dta("cepsw2studentCN.dta"))

d$dep4 <- rowMeans(cbind(w1$a1801, w1$a1803, w1$a1804, w1$a1805))
nx <- w2 %>% transmute(ids,
                       dep4_w2 = rowMeans(cbind(w2c2501, w2c2503, w2c2504, w2c2506)),
                       conf_w2 = w2b21)
m <- d %>% inner_join(nx, by = "ids") %>% filter(grade9 == 0)

ctrl <- gsub("\\+ grade9 ", "", controls)   # 只有七年级队列，去掉年级
l_conf <- feols(as.formula(paste("conf_w2 ~ sport + conf + acad +", ctrl, "| schids")),
                data = m, cluster = ~schids)
l_dep  <- feols(as.formula(paste("dep4_w2 ~ sport + dep4 + acad +", ctrl, "| schids")),
                data = m, cluster = ~schids)
etable(l_conf, l_dep, digits = 3, keep = "sport", headers = c("Confidence W2", "Depression W2"))
