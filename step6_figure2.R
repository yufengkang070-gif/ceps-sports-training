# =========================================================
# 第 6 步：Figure 2 —— PSM 匹配前后的倾向得分分布图
# 前提：已经运行过 step1 和 step2_psm.R（需要其中的 s 和 m）
# 输出：C:/ceps/w1/figure2_psm.png（600 dpi，可直接用于投稿）
# =========================================================
library(MatchIt)

to_p <- function(x) 1 / (1 + exp(-x))   # linear.logit 转回概率
s$ps <- to_p(m$distance)

# 有放回匹配：用 get_matches() 展开配对，重复使用的对照组会重复出现
gm <- get_matches(m)
gm$ps <- to_p(gm$distance)

png("figure2_psm.png", width = 9, height = 3.8, units = "in", res = 600)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 2.5, 1), las = 1, bty = "l")
br   <- seq(0, 1, by = 0.01)
blue <- rgb(47, 111, 214, 120, maxColorValue = 255)
grey <- rgb(154, 165, 177, 120, maxColorValue = 255)

hist(s$ps[s$sport == 0], breaks = br, freq = FALSE, col = grey, border = NA,
     xlim = c(0, 0.45), main = "Before matching", xlab = "Propensity score", ylab = "Density")
hist(s$ps[s$sport == 1], breaks = br, freq = FALSE, col = blue, border = NA, add = TRUE)

hist(gm$ps[gm$sport == 0], breaks = br, freq = FALSE, col = grey, border = NA,
     xlim = c(0, 0.45), main = "After matching", xlab = "Propensity score", ylab = "")
hist(gm$ps[gm$sport == 1], breaks = br, freq = FALSE, col = blue, border = NA, add = TRUE)
legend("topright", c("No sports training", "Sports training"),
       fill = c(grey, blue), border = NA, bty = "n", cex = 0.9)
dev.off()
cat("已保存：", file.path(getwd(), "figure2_psm.png"), "\n")
