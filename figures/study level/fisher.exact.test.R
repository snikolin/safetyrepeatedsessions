
contingency <-
matrix(c(34, 19, 9, 24),
       nrow = 2,
       dimnames = list(AE = c("Active", "Sham"),
                       Truth = c("AE", "No AE")))
# AAE, SAE, AnoAE, SnoAE
fisher.test(contingency, alternative = "t",conf.int = TRUE, conf.level = 0.95)

