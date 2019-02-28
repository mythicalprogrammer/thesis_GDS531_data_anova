grep("33273_f_at", colnames(GDS)) # 3303
grep("33500_i_at", colnames(GDS)) # 3533
grep("33501_r_at", colnames(GDS)) # 3534

# find which row are NA in each column
which(is.na(GDS$`33273_f_at`))# 30
which(is.na(GDS$`33500_i_at`))# 13  20  31  65 108 125 127 160
which(is.na(GDS$`33501_r_at`)) # 5

res.aov <- aov(GDS$`33273_f_at` ~ state, data = GDS)
result <- summary(res.aov)
result[[1]]$`Pr(>F)`[[1]]

res.aov <- aov(GDS$`33500_i_at` ~ state, data = GDS)
result <- summary(res.aov)
result[[1]]$`Pr(>F)`[[1]]

res.aov <- aov(GDS$`33501_r_at` ~ state, data = GDS)
result <- summary(res.aov)
result[[1]]$`Pr(>F)`[[1]]

# Missing values in all columns are not sig to care about
