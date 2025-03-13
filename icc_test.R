r1 <- round(rnorm(1, 10, 4))
r2 <- round(r1 + 10 + rnorm(1, 0, 2))
r3 <- round(r1 + 20 + rnorm(1, 0, 2))
irr::icc(cbind(r1, r2, r3), type = "agreement")

df = data.frame(Rater = c(1,2,3), Score = c(10,10,10), Object = "EXP10")
df = rbind(
  data.frame(Rater = c(1,2,3), Score = c(10,10,10), Object = "EXP10"),
  data.frame(Rater = c(1,2,3), Score = c(200,200,200), Object = "EXP20")
)
df
agreement::dim_icc(df, type = "agreement", unit = "average")
