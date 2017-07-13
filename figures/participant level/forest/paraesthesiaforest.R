# Paraesthesia Forest Plots

library(xlsx)
library(metafor)

### load data for paraesthesia
sheet <- 12
participantdata <- read.xlsx("H:/tDCS Safety/rparticipantlvl.xlsx",sheet)

### decrease margins so the full space is used
par(mar=c(4,4,1,2), cex = 1)

### calculate (log) odds ratios and corresponding sampling variances
res <- rma(ai=apos, bi=aneg, ci=spos, di=sneg, data=participantdata, measure="OR",
           slab=paste(author), method="REML")

### fit random-effects models
forest(res, xlim=c(-28, 13), at=log(c(.01, .1, 1, 10, 100,1000)), atransf=exp,
       ilab=cbind(participantdata$apos, participantdata$aneg, 
		participantdata$spos, participantdata$sneg),
       ilab.xpos=c(-12,-10,-8,-6), cex=.75, ylim=c(-1, 61),
	rows=c(3:22,27:42,47:57),     
	xlab="Odds Ratio", mlab="", psize=1)

### add text with Q-value, dfs, p-value, and I^2 statistic
text(-28, -1, pos=4, cex=0.75, bquote(paste("RE Model for Paraesthesia (Q = ",
     .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
     ", p = ", .(formatC(res$pval, digits=3, format="f")), ", ", I^2, " = ",
     .(formatC(res$I2, digits=1, format="f")), "%)")))
 
### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=4)

### add text for the subgroups
text(-23, c(58,43,23), pos=4, c("Burning",
                               "Itching",
                               "Tingling"))

## switch to bold font
par(font=2, ps = 12, cex = 0.8, cex.main = 1)
 
### add column headings to the plot
text(c(-12,-10,-8,-6),   60, c("AE+", "AE-", "AE+", "AE-"))
text(c(-11,-7),     	   61, c("Active", "Sham"))
text(-28,                60, "Author(s) and Year",  pos=4)
text(13,                 60, "Odds Ratio [95% CI]", pos=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
res.b <- rma(ai=apos, bi=aneg, ci=spos, di=sneg, data=participantdata, measure="OR",
             subset=(type==3), method="REML")
res.i <- rma(ai=apos, bi=aneg, ci=spos, di=sneg, data=participantdata, measure="OR",
             subset=(type==2), method="REML")
res.t <- rma(ai=apos, bi=aneg, ci=spos, di=sneg, data=participantdata, measure="OR",
             subset=(type==1), method="REML")
 
### add summary polygons for the three subgroups
addpoly(res.b, row= 45.5, cex=0.75, atransf=exp, mlab="")
addpoly(res.i, row= 25.5, cex=0.75, atransf=exp, mlab="")
addpoly(res.t, row= 1.50, cex=0.75, atransf=exp, mlab="")

### add text with Q-value, dfs, p-value, and I^2 statistic for subgroups
text(-28, 45.5, pos=4, cex=0.75, bquote(paste("RE Model for Burning (Q = ",
     .(formatC(res.b$QE, digits=2, format="f")), ", df = ", .(res.b$k - res.b$p),
     ", p = ", .(formatC(res.b$pval, digits=3, format="f")), "; ", I^2, " = ",
     .(formatC(res.b$I2, digits=1, format="f")), "%)")))
text(-28, 25.5, pos=4, cex=0.75, bquote(paste("RE Model for Itching (Q = ",
     .(formatC(res.i$QE, digits=2, format="f")), ", df = ", .(res.i$k - res.i$p),
     ", p = ", .(formatC(res.i$pval, digits=3, format="f")), "; ", I^2, " = ",
     .(formatC(res.i$I2, digits=1, format="f")), "%)")))
text(-28, 1.5, pos=4, cex=0.75, bquote(paste("RE Model for Tingling (Q = ",
     .(formatC(res.t$QE, digits=2, format="f")), ", df = ", .(res.t$k - res.t$p),
     ", p = ", .(formatC(res.t$pval, digits=3, format="f")), "; ", I^2, " = ",
     .(formatC(res.t$I2, digits=1, format="f")), "%)")))
 

