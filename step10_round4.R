# =========================================================
# 第 10 步：第四轮审稿补充分析
# 运行顺序：先跑 step1，再跑 step2，再跑 step4，最后跑本文件
# （需要 step1 的 d、controls，step2 的 s、covs，step4 的 p、w1、w2）
# =========================================================
library(MatchIt); library(fixest); library(dplyr)

# ---- (1) 把认知能力加进匹配变量 ----
covs2 <- paste(covs, "+ cog")
s2 <- subset(s, !is.na(cog))
m2 <- matchit(as.formula(paste("sport ~", covs2)), data = s2,
              method = "nearest", distance = "glm", link = "linear.logit",
              caliper = 0.2, replace = TRUE, ratio = 1)
summary(m2)$sum.matched[, "Std. Mean Diff."]      # 匹配后平衡，绝对值应 < 0.1
md2 <- match.data(m2)
att2 <- lapply(c("conf","dep","score","peer"), function(y)
  feols(as.formula(paste(y, "~ sport")), data = md2, weights = ~weights, cluster = ~schids))
etable(att2, digits = 3, keep = "sport", headers = c("Conf","Dep","Score","Peer"))

# 学校固定效应回归里再控制认知能力
ols_cog <- lapply(c("conf","dep","peer","score"), function(y)
  feols(as.formula(paste(y, "~ sport + acad + cog + female + age + grade9 + rural + ses + pedu + only + board | schids")),
        data = d, cluster = ~schids))
etable(ols_cog, digits = 3, keep = "sport", headers = c("Conf","Dep","Peer","Score"))

# ---- (2) 开始培训 vs 停止培训（一阶差分）----
wide <- p %>% group_by(ids) %>%
  summarise(schids = first(schids),
            s0 = sport[wave == 0][1], s1 = sport[wave == 1][1],
            a0 = acad[wave == 0][1],  a1 = acad[wave == 1][1],
            c0 = conf[wave == 0][1],  c1 = conf[wave == 1][1],
            d0 = dep4[wave == 0][1],  d1 = dep4[wave == 1][1]) %>%
  mutate(start = as.numeric(s0 == 0 & s1 == 1), quit = as.numeric(s0 == 1 & s1 == 0),
         dacad = a1 - a0, dconf = c1 - c0, ddep = d1 - d0)
asym <- list(feols(dconf ~ start + quit + dacad, data = wide, cluster = ~schids),
             feols(ddep  ~ start + quit + dacad, data = wide, cluster = ~schids))
etable(asym, digits = 3, headers = c("ΔConf","ΔDep"))

# ---- (3) 性别差异 ----
g1 <- lapply(c("conf","dep"), function(y)
  feols(as.formula(paste(y, "~ sport * female + acad + age + grade9 + rural + ses + pedu + only + board | schids")),
        data = d, cluster = ~schids))
etable(g1, digits = 3, keep = c("sport","female"), headers = c("Conf","Dep"))
p <- p %>% left_join(w1 %>% transmute(ids, female = 1 - stsex), by = "ids")
g2 <- list(feols(conf ~ sport + sport:female + acad + wave | ids, data = p, cluster = ~schids),
           feols(dep4 ~ sport + sport:female + acad + wave | ids, data = p, cluster = ~schids))
etable(g2, digits = 3, headers = c("Conf FE","Dep FE"))

# ---- (4) 追踪样本流失（七年级队列）----
g7 <- w1 %>% filter(grade9 == 0) %>%
  left_join(w2 %>% select(ids, w2status), by = "ids") %>%
  mutate(stay = as.numeric(w2status == 1),
         dep4 = rowMeans(cbind(a1801, a1803, a1804, a1805)),
         score = rowMeans(cbind(stdchn, stdmat, stdeng)))
mean(g7$stay)                                         # 追访成功率
g7 %>% group_by(stay) %>%
  summarise(sport = mean(b1910, na.rm = TRUE), conf = mean(c25, na.rm = TRUE),
            dep4 = mean(dep4, na.rm = TRUE), rural = mean(sthktype, na.rm = TRUE),
            cog = mean(cog3pl, na.rm = TRUE), score = mean(score, na.rm = TRUE))
