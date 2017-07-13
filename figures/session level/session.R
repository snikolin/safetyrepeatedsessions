library(xlsx)
library(metafor)
library(RColorBrewer)

# enter first: 
sheet <- 8
sessiondata <- read.xlsx("H:/tDCS Safety/rsessionlvl.xlsx",sheet)
sessiondata
dat1 <- escalc(measure="OR",ai=apos,bi=aneg,ci=spos,di=sneg,data=sessiondata)
res <- rma(yi, vi, mods = ~ccharge, data = dat1)
res

preds <- predict(res,newmods=c(0:75),transf=exp)
wi <- 1/sqrt(dat1$vi)
size <- 2 + 5.0 * (wi - min(wi))/(max(wi) - min(wi))
cols <- brewer.pal(n = 7, name = "Dark2")
colours <- cols[dat1$colid]

par(mar=c(5,5,1,2))
plot(dat1$ccharge, exp(dat1$yi), pch=19, cex=size, 
col = colours, xlab="Cumulative Charge", ylab="Odds Ratio",
las=1, bty="l", log="y",ylim = c(0.4,10),xlim=c(5,75))

# Boundaries and absolute line at OR = 1 
lines(0:75, preds$pred)
lines(0:75, preds$ci.lb, lty="dashed")
lines(0:75, preds$ci.ub, lty="dashed")
abline(h=1, lty = "dotted")

# Legend
grpcol <- cols[dat1$colid]
leg.txt <- dat1$author
legend(55,5, leg.txt, pch = 19, col = grpcol, bty = "n", cex = 1, pt.cex = 2, xjust = 0, title = "Headache",title.adj=0)

# Size = 600 x 600
# Discomfort(S5) - ylim = c(0.1,10); xlim = (5,75); legend (50,6.5)
# Erythema (S2) - ylim = c(0.1,100), xlim = (5,40); legend(25,40)
# Fatigue(S6) - ylim = c(0.1,10), xlim = (5,40); legend(25,10)
# Headache(S3) - ylim = c(0.4,10); xlim = (5,75);legend(55,5)
# Paraesthesia(S8) - ylim = c(0.4,10); xlim = (5,75);



# colours <- cols[dat1$type]
# leg.txt <- c("Tingling", "Itching", "Burning")
# legend(50,10, leg.txt, pch = 19, col = grpcol, bty = "n", cex = 1, pt.cex = 2, xjust = 0, title = "Paraesthesia")

