library(xlsx)
library(metafor)
library(RColorBrewer) # display.brewer.all()

# enter first: sheet <- 
sheet <- 7
participantdata <- read.xlsx("H:/tDCS Safety/rparticipantlvl.xlsx",sheet)
participantdata
dat1 <- escalc(measure="OR",ai=apos,bi=aneg,ci=spos,di=sneg,data=participantdata)
res <- rma(yi, vi, mods = ~ ccharge, data = dat1)
res
		
# res <- rma(yi, vi, random = ~ ccharge, data = dat1)
preds <- predict(res,newmods=c(0:72),transf=exp)
wi <- 1/sqrt(dat1$vi)
size <- 2 + 5.0 * (wi - min(wi))/(max(wi) - min(wi))
cols <- brewer.pal(n = 6, name = "Dark2") # dev.new(); display.brewer.all()
colours <- cols[dat1$group]

par(mar=c(5,5,1,2))
plot(dat1$ccharge, exp(dat1$yi), pch=19, cex=size, 
col = colours, xlab="Cumulative Charge", ylab="Odds Ratio",
las=1, bty="l", log="y",ylim = c(0.1,10),xlim=c(2,47))

# plot(dat1$ccharge, exp(dat1$yi), pch=19, cex=size, col = colours, xlab="Cumulative Charge", ylab="Odd's Ratio",las=1, bty="l", log="y", ylim = c(0.1,6), xlim = c(1:40))
lines(0:72, preds$pred)
lines(0:72, preds$ci.lb, lty="dashed")
lines(0:72, preds$ci.ub, lty="dashed")
abline(h=1, lty = "dotted")

# text above the bubble plots
ids <- 1:30
pos <- dat1$pos #1=below,2=left,3=above,4=right
#offset <- 
text(dat1$ccharge[ids],exp(dat1$yi)[ids],dat1$author[ids],cex =.7, pos = pos)

# legend values
leg.txt <- c("Healthy", "Pain Disorder", "Stroke", "Neurocognitive Disorder", "Neuropsychiatric Disorder", "Other")
legend(23,0.6, leg.txt, pch = 19, col = cols, bty = "n", cex = 1, pt.cex = 2, xjust = 0, title = "Headache",title.adj=0)

# # Size = 600 x 600
# Discomfort(S4) - ylim = c(0.15,22); legend (22,21)
# Fatigue(S10) - ylim = c(0.3,10); legend(24,12)
# Headache(S7) - ylim = c(0.1,10); legend(23,0.6), xlim=c(2,47)
# Paraesthesia(S12) - ylim = c(0.2,20); 

# paraesthesia specific
# colours <- cols[dat1$type]
# leg.txt <- c("Tingling", "Itching", "Burning")
# legend(55,20, leg.txt, pch = 19, col = cols, bty = "n", cex = 1, pt.cex = 2, xjust = 0, title = "Paraesthesia",title.adj=0)


# headache(25,0.5)
# tinlging(43,10)
# fatigue(25,12)
# paraesthesia(50,18)
# burning(

# plot.new() - clears the image
# dev.new() - creates a new frame
# dev.set() - chooses which frame is active at the moment