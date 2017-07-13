# plot in one script

library(xlsx)
library(metafor)
library(RColorBrewer)

par(mfrow = c(2,2))

# Discomfort
sheet <- 4
participantdata <- read.xlsx("H:/tDCS Safety/rparticipantlvl.xlsx",sheet)
participantdata
dat1 <- escalc(measure="OR",ai=apos,bi=aneg,ci=spos,di=sneg,data=participantdata)
res <- rma(yi, vi, mods = ~ ccharge, data = dat1)

preds <- predict(res,newmods=c(0:72),transf=exp)
wi <- 1/sqrt(dat1$vi)
size <- 2 + 5.0 * (wi - min(wi))/(max(wi) - min(wi))
cols <- brewer.pal(n = 6, name = "Dark2")
colours <- cols[dat1$group]

par(mar=c(5,5,1,2))
plot(dat1$ccharge, exp(dat1$yi), pch=19, cex=size, 
     col = colours, xlab="Cumulative Charge", ylab="Odds Ratio",
     las=1, bty="l", log="y",ylim = c(0.15,21))

lines(0:72, preds$pred)
lines(0:72, preds$ci.lb, lty="dashed")
lines(0:72, preds$ci.ub, lty="dashed")
abline(h=1, lty = "dotted")

ids <- 1:30
pos <- dat1$pos
text(dat1$ccharge[ids],exp(dat1$yi)[ids],dat1$author[ids],cex =0.7, pos = pos)

leg.txt <- c("Healthy", "Pain Disorder", "Stroke", "Neurocognitive Disorder", "Neuropsychiatric Disorder", "Other")
legend(22,22, leg.txt, pch = 19, col = cols, bty = "n", cex = 1, pt.cex = 2, xjust = 0, title = "Discomfort",title.adj=0)
title("A")

# Dizziness
sheet <- 5
participantdata <- read.xlsx("H:/tDCS Safety/rparticipantlvl.xlsx",sheet)
participantdata
dat1 <- escalc(measure="OR",ai=apos,bi=aneg,ci=spos,di=sneg,data=participantdata)
res <- rma(yi, vi, mods = ~ ccharge, data = dat1)

preds <- predict(res,newmods=c(0:72),transf=exp)
wi <- 1/sqrt(dat1$vi)
size <- 2 + 5.0 * (wi - min(wi))/(max(wi) - min(wi))
cols <- brewer.pal(n = 6, name = "Dark2")
colours <- cols[dat1$group]

par(mar=c(5,5,1,2))
plot(dat1$ccharge, exp(dat1$yi), pch=19, cex=size, 
     col = colours, xlab="Cumulative Charge", ylab="Odds Ratio",
     las=1, bty="l", log="y",ylim = c(0.15,21),xlim=c(2,40))

lines(0:72, preds$pred)
lines(0:72, preds$ci.lb, lty="dashed")
lines(0:72, preds$ci.ub, lty="dashed")
abline(h=1, lty = "dotted")

ids <- 1:30
pos <- dat1$pos
text(dat1$ccharge[ids],exp(dat1$yi)[ids],dat1$author[ids],cex =0.7, pos = pos)

leg.txt <- c("Healthy", "Pain Disorder", "Stroke", "Neurocognitive Disorder", "Neuropsychiatric Disorder", "Other")
legend(22,21, leg.txt, pch = 19, col = cols, bty = "n", cex = 1, pt.cex = 2, xjust = 0, title = "Dizziness",title.adj=0)
title("B")

# Erythema
sheet <- 6
participantdata <- read.xlsx("H:/tDCS Safety/rparticipantlvl.xlsx",sheet)
participantdata
dat1 <- escalc(measure="OR",ai=apos,bi=aneg,ci=spos,di=sneg,data=participantdata)
res <- rma(yi, vi, mods = ~ ccharge, data = dat1)

preds <- predict(res,newmods=c(0:72),transf=exp)
wi <- 1/sqrt(dat1$vi)
size <- 2 + 5.0 * (wi - min(wi))/(max(wi) - min(wi))
cols <- brewer.pal(n = 6, name = "Dark2")
colours <- cols[dat1$group]

par(mar=c(5,5,1,2))
plot(dat1$ccharge, exp(dat1$yi), pch=19, cex=size, 
     col = colours, xlab="Cumulative Charge", ylab="Odds Ratio",
     las=1, bty="l", log="y",ylim = c(0.1,50),xlim=c(2,50))

lines(0:72, preds$pred)
lines(0:72, preds$ci.lb, lty="dashed")
lines(0:72, preds$ci.ub, lty="dashed")
abline(h=1, lty = "dotted")

ids <- 1:30
pos <- dat1$pos
text(dat1$ccharge[ids],exp(dat1$yi)[ids],dat1$author[ids],cex =0.7, pos = pos)

leg.txt <- c("Healthy", "Pain Disorder", "Stroke", "Neurocognitive Disorder", "Neuropsychiatric Disorder", "Other")
legend(30,50, leg.txt, pch = 19, col = cols, bty = "n", cex = 1, pt.cex = 2, xjust = 0, title = "Erythema",title.adj=0)
title("C")

# Fatigue
sheet <- 10
participantdata <- read.xlsx("H:/tDCS Safety/rparticipantlvl.xlsx",sheet)
participantdata
dat1 <- escalc(measure="OR",ai=apos,bi=aneg,ci=spos,di=sneg,data=participantdata)
res <- rma(yi, vi, mods = ~ ccharge, data = dat1)

preds <- predict(res,newmods=c(0:72),transf=exp)
wi <- 1/sqrt(dat1$vi)
size <- 2 + 5.0 * (wi - min(wi))/(max(wi) - min(wi))
cols <- brewer.pal(n = 6, name = "Dark2")
colours <- cols[dat1$group]

par(mar=c(5,5,1,2))
plot(dat1$ccharge, exp(dat1$yi), pch=19, cex=size, 
     col = colours, xlab="Cumulative Charge", ylab="Odds Ratio",
     las=1, bty="l", log="y",ylim = c(0.3,10),xlim = c(2,45))

lines(0:72, preds$pred)
lines(0:72, preds$ci.lb, lty="dashed")
lines(0:72, preds$ci.ub, lty="dashed")
abline(h=1, lty = "dotted")

ids <- 1:30
pos <- dat1$pos
text(dat1$ccharge[ids],exp(dat1$yi)[ids],dat1$author[ids],cex =0.7, pos = pos)

leg.txt <- c("Healthy", "Pain Disorder", "Stroke", "Neurocognitive Disorder", "Neuropsychiatric Disorder", "Other")
legend(26,12, leg.txt, pch = 19, col = cols, bty = "n", cex = 1, pt.cex = 2, xjust = 0, title = "Fatigue",title.adj=0)
title("D")

# Headache
sheet <- 7
participantdata <- read.xlsx("H:/tDCS Safety/rparticipantlvl.xlsx",sheet)
participantdata
dat1 <- escalc(measure="OR",ai=apos,bi=aneg,ci=spos,di=sneg,data=participantdata)
res <- rma(yi, vi, mods = ~ ccharge, data = dat1)

preds <- predict(res,newmods=c(0:72),transf=exp)
wi <- 1/sqrt(dat1$vi)
size <- 2 + 5.0 * (wi - min(wi))/(max(wi) - min(wi))
cols <- brewer.pal(n = 6, name = "Dark2")
colours <- cols[dat1$group]

par(mar=c(5,5,1,2))
plot(dat1$ccharge, exp(dat1$yi), pch=19, cex=size, 
     col = colours, xlab="Cumulative Charge", ylab="Odds Ratio",
     las=1, bty="l", log="y",ylim = c(0.1,10),xlim=c(2,47)) 

lines(0:72, preds$pred)
lines(0:72, preds$ci.lb, lty="dashed")
lines(0:72, preds$ci.ub, lty="dashed")
abline(h=1, lty = "dotted")

ids <- 1:30
pos <- dat1$pos
text(dat1$ccharge[ids],exp(dat1$yi)[ids],dat1$author[ids],cex =0.7, pos = pos)

leg.txt <- c("Healthy", "Pain Disorder", "Stroke", "Neurocognitive Disorder", "Neuropsychiatric Disorder", "Other")
legend(23,0.6, leg.txt, pch = 19, col = cols, bty = "n", cex = 1, pt.cex = 2, xjust = 0, title = "Headache",title.adj=0)
title("E")

# Paraesthesiaylim = c(0.2,20); 
sheet <- 12
participantdata <- read.xlsx("H:/tDCS Safety/rparticipantlvl.xlsx",sheet)
participantdata
dat1 <- escalc(measure="OR",ai=apos,bi=aneg,ci=spos,di=sneg,data=participantdata)
res <- rma(yi, vi, mods = ~ ccharge, data = dat1)

preds <- predict(res,newmods=c(0:72),transf=exp)
wi <- 1/sqrt(dat1$vi)
size <- 2 + 5.0 * (wi - min(wi))/(max(wi) - min(wi))
cols <- brewer.pal(n = 6, name = "Dark2") # dev.new(); display.brewer.all()
colours <- cols[dat1$type]

par(mar=c(5,5,1,2))
plot(dat1$ccharge, exp(dat1$yi), pch=19, cex=size, 
     col = colours, xlab="Cumulative Charge", ylab="Odds Ratio",
     las=1, bty="l", log="y",ylim = c(0.2,20))

lines(0:72, preds$pred)
lines(0:72, preds$ci.lb, lty="dashed")
lines(0:72, preds$ci.ub, lty="dashed")
abline(h=1, lty = "dotted")

leg.txt <- c("Tingling", "Itching", "Burning")
legend(55,20, leg.txt, pch = 19, col = cols, bty = "n", cex = 1, pt.cex = 2, xjust = 0, title = "Paraesthesia",title.adj=0)
title("F")
