  tDCS.rep.safety = function(filename,AE.type,level){
  # filename must be entered as a directory to an excel spreadsheet such as the one provided: "AE master sheet.xlsx"
  # AE.type can take on the values: "burning","discomfort","erythema","fatigue",
  #    "headache","itching","tingling","burn","concentration","dizziness","mania",
  #     multiple AEs can be entered at once (e.g. for paraesthesia enter: level <- c("itching","tingling","burning"))
  # level can take on the values: 1,2,3
  #     1:session level; 2:incidence rates; 3:study level
  #     multiple numbers can be entered at once (e.g. level <- c(1,3))
  #
  # Example of usage
  # filename <- "H:/tDCS Safety/AE master sheet.xlsx"
  # AE.type <- c("itching","tingling","burning")
  # level <- 1
  # Use as: res <- tDCS.rep.safety(filename,AE.type,level)
  # where res$session, res$incidence, res$forest and res$study are the outputs for each analysis
  
  # Check function inputs are present
  if(missing(AE.type))
    stop("Need to specify adverse event e.g. headache")
  
  if(missing(level))
    stop("Need to specify level of analysis. 1:session level; 2:incidence rates; 3:study level")
  
  # Check that required packages are installed and loaded
  CBREW <- "RColorBrewer"
  if(!require(CBREW,character.only=TRUE)){
    install.packages(CBREW,dep=TRUE,repos="https://cran.ms.unimelb.edu.au/")
  }
  
  MFOR <- "metafor"
  if(!require(MFOR,character.only=TRUE)){
    install.packages(MFOR,dep=TRUE,repos="https://cran.ms.unimelb.edu.au/")
  }
  
  XREAD <- "xlsx"
  if(!require(XREAD,character.only=TRUE)){
    install.packages(XREAD,dep=TRUE,repos="https://cran.ms.unimelb.edu.au/")
  }
  
  # Call required library packages
  require(RColorBrewer)
  require(metafor)
  require(xlsx)
  
  # Read file containing all AE data
  master.dat <- read.xlsx(filename,1)
  
  # Initialise data frames
  session.res <- data.frame()
  incidence.res <- data.frame()
  forest.res <- data.frame()
  study.res <- data.frame()
  
  # Session level analysis and plotting
  if(is.element(1,level)){
    AE.type <- tolower(AE.type)
    index <- master.dat$level == 1 & is.element(master.dat$AE,AE.type)
    
    # Simple univariate meta-regression
    metareg.dat <- escalc(measure="OR",ai=apos,bi=aneg,ci=spos,di=sneg,data=master.dat[index,])
    session.res <- rma(yi, vi, mods = ~ccharge, data = metareg.dat)
    
    # Bubble Plot
    ccharge.max <- max(metareg.dat$ccharge) + 5
    preds <- predict(session.res,newmods=c(0:ccharge.max),transf=exp)
    wi <- 1/sqrt(metareg.dat$vi)
    size <- 2 + 5.0 * (wi - min(wi))/(max(wi) - min(wi))
    colours <- brewer.pal(n = 7, name = "Dark2")
    
    ymin <- floor(exp(min(metareg.dat$yi))*10)/10
    ymax <- ceiling(max(exp(metareg.dat$yi))/10)*10
    yabs <- max(c(abs(ymin),abs(ymax)))
    
    dev.new()
    par(mar=c(5,5,1,2))
    plot(metareg.dat$ccharge, exp(metareg.dat$yi), pch=19, cex=size, 
         col = colours, xlab="Cumulative Charge", ylab="Odds Ratio",
         las=1, bty="l", log="y",ylim = c(1/yabs,yabs), xlim = c(0,(ccharge.max)))
    
    lines(0:ccharge.max, preds$pred)
    lines(0:ccharge.max, preds$ci.lb, lty="dashed")
    lines(0:ccharge.max, preds$ci.ub, lty="dashed")
    abline(h=1, lty = "dotted")
    
    grpcol <- colours
    leg.txt <- metareg.dat$author
    legend("topright",legend = leg.txt, pch = 19, col = grpcol, bty = "n", cex = 1, 
           pt.cex = 2, xjust = 1, title.adj = 0, 
           title = paste0(toupper(substr(AE.type, 1, 1)), substr(AE.type, 2, nchar(AE.type))))
    title("Session Level Bubble Plot")
  }
  
  # Incidence rate analysis and plotting
  if(is.element(2,level)){
    AE.type <- tolower(AE.type)
    index <- master.dat$level == 2 & is.element(master.dat$AE,AE.type)
    
    # Forest and Funnel plots
    forest.dat <- escalc(measure="OR",method="RE",ai=apos,bi=aneg,ci=spos,di=sneg,data=master.dat[index,])
    forest.res <- rma(yi,vi,data = forest.dat,slab = forest.dat$author)
    forest.res$egger <- regtest.rma(forest.res,model="rma",predictor="sei",ret.fit=FALSE)["pval"]

    # Funnel Plot
    dev.new()
    funnel(forest.res)
    
    # Forest Plot
    rowsnum <- dim(forest.dat)[1] 
    dev.new()
    par(mar=c(4,4,1,2), cex = 1)
    forest(forest.res, xlim=c(-23, 12), at=log(c(.01, .1, 1, 10, 100,1000)), atransf=exp,
           ilab=cbind(forest.dat$apos, forest.dat$aneg, 
                      forest.dat$spos, forest.dat$sneg),
           ilab.xpos=c(-11,-9,-7,-5), cex=.75, ylim=c(-1, rowsnum + 3),     
           xlab="Odds Ratio", mlab="", psize=1)
    
    text(-23, -1, pos=4, cex=0.75, bquote(paste("RE Model (Q = ",
                                                .(formatC(forest.res$QE, digits=2, format="f")), ", df = ", .(forest.res$k - forest.res$p),
                                                ", p-value = ", .(formatC(forest.res$pval, digits=3, format="f")), ", ", I^2, " = ",
                                                .(formatC(forest.res$I2, digits=1, format="f")), "%)")))
    
    par(font=2, ps = 12, cex = 0.8, cex.main = 1)
    text(c(-11,-9,-7,-5),    (rowsnum+2), c("AE+", "AE-", "AE+", "AE-"))
    text(c(-10,-6),          (rowsnum+3), c("Active", "Sham"))
    text(-23,                (rowsnum+2), "Author(s) and Year",  pos=4)
    text(12,                 (rowsnum+2), "Odds Ratio [95% CI]", pos=2)
    
    # Simple univariate meta-regression
    metareg.dat <- escalc(measure="OR",ai=apos,bi=aneg,ci=spos,di=sneg,data=master.dat[index,])
    incidence.res <- rma(yi, vi, mods = ~ccharge, data = metareg.dat)
    
    # Bubble Plot
    ccharge.max <- max(metareg.dat$ccharge) + 5
    preds <- predict(incidence.res,newmods=c(0:ccharge.max),transf=exp)
    wi <- 1/sqrt(metareg.dat$vi)
    size <- 2 + 5.0 * (wi - min(wi))/(max(wi) - min(wi))
    cols <- brewer.pal(n = 6, name = "Dark2")
    colours <- cols[metareg.dat$group]
    
    ymin <- floor(exp(min(metareg.dat$yi))*10)/10
    ymax <- ceiling(max(exp(metareg.dat$yi))/10)*10
    yabs <- max(c(abs(ymin),abs(ymax)))
    
    dev.new()
    par(mar=c(5,5,1,2))
    plot(metareg.dat$ccharge, exp(metareg.dat$yi), pch=19, cex=size, 
         col = colours, xlab="Cumulative Charge", ylab="Odds Ratio",
         las=1, bty="l", log="y",ylim = c(1/yabs,yabs), xlim = c(0,(ccharge.max)))
    
    lines(0:ccharge.max, preds$pred)
    lines(0:ccharge.max, preds$ci.lb, lty="dashed")
    lines(0:ccharge.max, preds$ci.ub, lty="dashed")
    abline(h=1, lty = "dotted")
    
    ids <- 1:length(metareg.dat)
    text(metareg.dat$ccharge[ids],exp(metareg.dat$yi)[ids],metareg.dat$author[ids],cex =.5, pos = rep(1,length(metareg.dat)))
    
    grpcol <- colours
    leg.txt <- c("Healthy", "Pain Disorder", "Stroke", "Neurocognitive Disorder", "Neuropsychiatric Disorder", "Other")
    legend("topright",legend = leg.txt, pch = 19, col = cols, bty = "n", cex = 1, 
           pt.cex = 2, xjust = 1, title.adj = 0, 
           title = paste0(toupper(substr(AE.type, 1, 1)), substr(AE.type, 2, nchar(AE.type))))
    title("Incidence Rates Bubble Plot")
  }
  
  # Study level analysis and plotting
  if(is.element(3,level)){
    
    # Fishers exact test
    tertile.levels <- c("low","medium","high")  
    study.res <- data.frame(ccharge = factor(c("low","medium","high"), levels=tertile.levels))
    
    for(a in 1:length(tertile.levels)){
      
      index <- master.dat$AE == tertile.levels[a]
      A.pos <- sum(master.dat$apos[index])
      S.pos <- sum(master.dat$spos[index])
      A.neg <- sum(master.dat$aneg[index])
      S.neg <- sum(master.dat$sneg[index])
    
      contingency.table <- matrix(c(A.pos,S.pos,A.neg,S.neg),
                    nrow = 2,
                    dimnames = list(AE = c("Active", "Sham"), Outcome = c("AE", "No AE")))
    
      tempdat <- fisher.test(contingency.table, alternative = "t",conf.int = TRUE, conf.level = 0.95)
    
      study.res$OR[a] <- tempdat$estimate
      study.res$CILB[a] <- tempdat$conf.int[1]
      study.res$CIUB[a] <- tempdat$conf.int[2]
      study.res$p.value[a] <- tempdat$p.value
    }
  
    # Tertile plot
    ymin <- floor(min(study.res$CILB)*10)/10
    ymax <- ceiling(max(study.res$CIUB)/10)*10
    
    dev.new()
    par(mar=c(5,5,1,2))
    plot(c(1:3),study.res$OR,pch = 19, cex = 2,col="black",
         las = 1,log="y",ylim = c(ymin,ymax), xlim = c(0.5,3.5),
         xaxt = 'n',ann=FALSE,bty='o')
    abline(h=1, lty = "dotted")
    axis(1, at = 1:3, lab=study.res$ccharge, xlab = "Cumulative Charge")
    arrows(c(1:3),study.res$CILB,c(1:3),study.res$CIUB,length=0.05,angle=90,code=3)
    title("Study Level Tertile Plot", xlab = "Cumulative Charge", ylab = "Odds Ratio")
  }
  
  result <- list(session = session.res,incidence = incidence.res,forest = forest.res, study = study.res)
  return(result)
  }
  