# Erythema Forest Plots

library(xlsx)
library(metafor)

### load data for erythema
sheet <- 6
participantdata <- read.xlsx("H:/tDCS Safety/rparticipantlvl.xlsx",sheet)

### decrease margins so the full space is used
par(mar=c(4,4,1,2), cex = 1)

### calculate (log) odds ratios and corresponding sampling variances
res <- rma(ai=apos, bi=aneg, ci=spos, di=sneg, data=participantdata, measure="OR",
           slab=paste(author), method="REML")

### replace in ylim=c(-1,11) later on
rowsnum <- dim(participantdata)[2] 

### fit random-effects models
forest(res, xlim=c(-23, 12), at=log(c(.01, .1, 1, 10, 100,1000)), atransf=exp,
       ilab=cbind(participantdata$apos, participantdata$aneg, 
		participantdata$spos, participantdata$sneg),
       ilab.xpos=c(-11,-9,-7,-5), cex=.75, ylim=c(-1, 16),     
       xlab="Odds Ratio", mlab="", psize=1)

text(-23, -1, pos=4, cex=0.75, bquote(paste("RE Model (Q = ",
     .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
     ", p-value = ", .(formatC(res$pval, digits=3, format="f")), ", ", I^2, " = ",
     .(formatC(res$I2, digits=1, format="f")), "%)")))

## switch to bold font
par(font=2, ps = 12, cex = 0.8, cex.main = 1)
 
### add column headings to the plot
text(c(-11,-9,-7,-5),    15, c("AE+", "AE-", "AE+", "AE-"))
text(c(-10,-6),           16, c("Active", "Sham"))
text(-23,                15, "Author(s) and Year",  pos=4)
text(12,                 15, "Odds Ratio [95% CI]", pos=2)
