dat <- data.frame(ccharge = factor(c("Low","Medium","High"),
levels=c("Low","Medium","High")))

# Normal
dat$OR = c(1.544236,1.143023,1.642897)
dat$CILB <- c(0.1633985,0.3654732,0.5515256)
dat$CIUB <- c(19.8400365,3.6045734,5.0478014)

par(mar=c(5,5,1,2))
plot(c(1:3),dat$OR,pch = 19, cex = 2,col="black",
     las = 1,log="y",ylim = c(0.1,30), xlim = c(0.5,3.5),
     xaxt = 'n',ann=FALSE,bty='o')
abline(h=1, lty = "dotted")
axis(1, at = 1:3, lab=dat$ccharge, xlab = "Cumulative Charge")
arrows(c(1:3),dat$CILB,c(1:3),dat$CIUB,length=0.05,angle=90,code=3)
title(xlab = "Cumulative Charge", ylab = "Odds Ratio")


# Conservative
dat$OR <- c(4.111946,1.712852,4.678545)
dat$CILB <- c(0.6981593,0.6238405,1.688652)
dat$CIUB <- c(44.0443481,4.8254789,13.977431)

par(mar=c(5,5,1,2))
plot(c(1:3),dat$OR,pch = 19, cex = 2,col="black",
     las = 1,log="y",ylim = c(0.35,55), xlim = c(0.5,3.5),
     xaxt = 'n',ann=FALSE,bty='o')
abline(h=1, lty = "dotted")
axis(1, at = 1:3, lab=dat$ccharge, xlab = "Cumulative Charge")
arrows(c(1:3),dat$CILB,c(1:3),dat$CIUB,length=0.05,angle=90,code=3)
text(3,25,"*",cex = 2)
title(xlab = "Cumulative Charge", ylab = "Odds Ratio")