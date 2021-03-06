grep("33273_f_at", colnames(GDS)) # 3303
grep("33500_i_at", colnames(GDS)) # 3533
grep("33501_r_at", colnames(GDS)) # 3534

# find which row are NA in each column
which(is.na(GDS$`33273_f_at`))# 30
which(is.na(GDS$`33500_i_at`))# 13  20  31  65 108 125 127 160
which(is.na(GDS$`33501_r_at`)) # 5


GDS_train <- GDS[
  -c(30, 13,  20,  31,  65, 108, 125, 127, 160, 5 ),]


fit <- ranger(
  dependent.variable.name = "33273_f_at",
  data = GDS_train,
  seed = 1030)

# GDS[30,3303] <- 12041.77 "33273_f_at"
predict(fit, GDS[30, -c(3303)])$predictions # 12041.77

fit <- ranger(
  dependent.variable.name = "33500_i_at",
  data = GDS_train, seed = 1030)

# 13  20  31  65 108 125 127 160
predict(fit, GDS[13,-c(3533)])$predictions # 16530.52
predict(fit, GDS[20,-c(3533)])$predictions # 24152.1
predict(fit, GDS[31,-c(3533)])$predictions # 14354.39
predict(fit, GDS[65,-c(3533)])$predictions # 15006.2
predict(fit, GDS[108,-c(3533)])$predictions # 10676.98
# GDS[13,3533] <- 10667.03
# GDS[20,3533] <- 24152.1
# GDS[31,3533] <- 14354.39
# GDS[65,3533] <- 15006.2
# GDS[108,3533] <- 10676.98
# 13  20  31  65 108 125 127 160
predict(fit, GDS[125,-c(3533)])$predictions # 10676.98

# Imputation gives such bad values let's try to use ANOVA
# and T-test to see if it's even worth it
