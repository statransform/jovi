# This code was downloaded from http://www.uni-koeln.de/~luepsen/R/
# As we only call the method with pseudo = F, the pseudorank library is not loaded.
# Note that this library was removed from CRAN but can be obtained from the archive: https://cran.r-project.org/web/packages/pseudorank/index.html

np.anova <- function(formel,data,method=0,compact=T,pseudo=F)
{                        # data  :  dataframe 
                         # formel:  anova-Modell
                         # method:  0: generalized Kruskal-Wallis-Friedman tests
                         #             including Iman & Davenport F-tests
                         #          1: generalized van der Waerden tests
                         #          2: Puri & Sen tests
                         #          3: Puri & Sen tests with INT
                         # compact: T: all tests in one table
                         #          F: seperate tables for each error term (repeated measures only)
                         # pseudo   T: use pseudoranks instead of ordinary ranks (for unequal ni)

if (pseudo) library(pseudorank) # See: https://cran.r-project.org/web/packages/pseudorank/index.html
                                               # check formula
if (mode(formel)!="call") stop("invalid formula")
if (class(data[,as.character(formel)[2]])=="factor") stop("invalid type of dependent variable")
                                               # perform standard aov and check model
aov.1 <- aov(formel,data)
if (class(aov.1)[1]=="aov")     repm<-0
if (class(aov.1)[1]=="aovlist") repm<-1
                                               # extract varnames
vars  <- all.vars(formel)
vdep  <- vars[1]                               # dependent var
nfact <- length(vars)-1
if (repm==1) nfact <- length(vars)-2
nfgrp <- nfact
if (repm==1)                                   # params for repeated
 { nam.aov  <- names(aov.1)
   l.aov    <- length(nam.aov)
   if (nam.aov[l.aov]=="Within") nam.aov <- nam.aov[-l.aov]
   nerrterm <- length(nam.aov)-2               # n of error terms for repeated
   nfrep    <- log2(length(nam.aov)-1)         # n of repeated measures factors
   nfgrp    <- nfact-nfrep }                   # n of grouping factors

if (repm==1) 
  {vpn      <- vars[length(vars)]                               # case id
   nvar     <- length(data[,vpn])/length(levels(data[,vpn])) }  # n dependent vars
  else
   nvar <- 1

if (pseudo)                                    # make cell factor for pseudoranks
  {nilist <- list(data[,vars[2]])
   ncell  <- length(table(data[,vars[2]]))
   if (nfgrp>1) 
     for (i in 2:nfgrp) 
       {nilist[[i]]<-data[,vars[i+1]]
        ncell <- ncell*length(table(data[,vars[i+1]])) }
   ni <- factor(rep(1:ncell,as.vector(table(nilist))/nvar))
  }
                                               # ranking procedure
                                               #   repeated measures
if (repm==1) {
  cfrep    <- unlist(strsplit(names(aov.1),":")[3:(nfrep+2)])[seq(2,2*nfrep,2)] # names
  nrep     <-1
  for (i in 1:nfrep) nrep <- nrep*length(levels(data[,cfrep[i]])) # n repeated measures
  nvar     <- length(data[,vpn])/length(levels(data[,vpn]))       # n dependent vars
                                               
  if (method<=1)                               # combined case and Friedman ranking
    {                                          # rank Vpn
     ysum <- ave(data[,vdep],data[,vpn],FUN=sum)
     if (pseudo) rsum     <-(pseudorank(ysum,ni)+(nvar-1)/2)/nvar
       else      rsum     <-(      rank(ysum)   +(nvar-1)/2)/nvar
                         
     if (method==1)                            #   normal scores for van der Waerden tests
       {  ncases <- sum(summary(aov.1[[2]])[[1]]$"Df")+1
          if (pseudo) ncases<-ncases*nvar
          rsum   <- qnorm(rsum/(ncases+1)) }
                                               #   rank repeated
     zzz_ry <- ave(data[,vdep], data[,vpn], FUN=rank) 
     if (method==1) zzz_ry <- qnorm(zzz_ry/(nvar+1))
     zzz_rx <- zzz_ry
     if (nfgrp>0)                              #   combine ranks
       {if (method==0)      zzz_rx <- (rsum-1)*nrep + zzz_ry
        else if (method==1) zzz_rx <- rsum+zzz_ry }
    }
  else                                         # Puri & Sen: simple ranking over cases and rep measures
    {if (pseudo) zzz_rx <- pseudorank(data[,vdep],ni)
       else      zzz_rx <-       rank(data[,vdep])
     if (method==3)                            #   normal scores
       {len<-length(data[,vdep])
        if (pseudo) len<-len*nvar
        zzz_rx <- qnorm(zzz_rx/(len+1)) } }
                                               
  if (nfgrp>1)                                 #   get grouping factors
     {ig <- 0 ; ifgrp <- 0
      for (i in 1:nfact)
         {if (length(grep(vars[i+1],names(aov.1)))==0)
             { ig <- ig+1 ; ifgrp[ig] <- vars[i+1] }
     } } }
else                                           # ranking procedure: only grouping factors
   { ncases <- sum(anova(aov.1)[,1])+1
     if (pseudo) zzz_rx     <- pseudorank(data[,vdep],ni)
       else      zzz_rx     <-       rank(data[,vdep])
                                               # normal scores for van der Waerden tests
     if (method==1) zzz_rx <- qnorm(zzz_rx/(ncases+1))
     }
                                               # perform anova on ranks
cformel <- as.character(formel)
formel1 <- as.formula(paste("zzz_rx ~",cformel[3]))
aov.2   <- aov(formel1,data)
                                               # perform puri & sen-tests
                                               # repeated measures
if (repm==1) {
   aov.21 <- summary(aov.2[[2]])[[1]]          # vpn error part (including grouping effects)
   df     <- aov.21$"Df"
   ncases <- sum(df)+1
   nerg   <- 0                                   # n of items in erglist
   ngeffects <- 0                                # n of grouping effects
   if (nfgrp>0)                                                                                          
     {ngeffects <- dim(aov.21)[1]-1
      mstotal   <- sum(aov.21$"Sum Sq")/sum(df)
                                                 # more grouping effects
      if (nfgrp>1)
        {                                        # type III ssq necessary
                                                 # separate anova for grouping effects
         formel2 <- paste(c("zzz_rx",paste(ifgrp,collapse="*")),collapse="~")
         formel3 <- paste(c("zzz_rx",paste(c(vpn,ifgrp),collapse="*")),collapse="~")
         data1   <- aggregate(as.formula(formel3),data=data,FUN=mean)
         aov.31  <- drop1(aov(as.formula(formel2),data1), ~. , test="F")
         aov.21$"Sum Sq"[1:(ngeffects-1)] <- nrep*aov.31$"Sum of Sq"[2:ngeffects]
        }
                                                 # prepare cols for Iman & Davenport
      if (method==0)
         {aov.21[[6]] <- aov.21[[5]]
          aov.21[[5]] <- aov.21[[4]]
          names(aov.21)[5:6] <- names(aov.21)[4:5] }
                                                 # chisquare tests
      names(aov.21)[5]   <- " "
      names(aov.21)[3:4] <-c("Chi Sq","Pr(>Chi)")
      chisq       <- aov.21$"Sum Sq"/mstotal
      aov.21[[3]] <- chisq
      aov.21[[4]] <- 1-pchisq(chisq,df)
      aov.21[ngeffects+1,3:4] <- NA

      erglist     <- list(aov.21)
      names(erglist)[1] <- names(aov.1)[2]
      nerg        <- 1
     }
   for (errterm in 3:(2+nerrterm))
      {                                          # for each trial error term...
      aov.22   <- summary(aov.2[[errterm]])[[1]] #   trial part of anova on ranks
      neffects <- dim(aov.22)[1]-1
      if (method==1) names(aov.22)[5] <- c(" ")
                                                 # prepare cols for Iman-Davenport F
      if (method==0)
         {aov.22[[6]] <- aov.22[[5]]
          aov.22[[5]] <- aov.22[[4]]
          names(aov.22)[5:6] <- names(aov.22)[4:5] }
      names(aov.22)[3:4] <- c("Chisq","Pr(>Chi)")
                                                 # chisquare tests
      df      <- aov.22$"Df"
      sumdf   <- sum(df)
      mstotal <- sum(aov.22$"Sum Sq")/sumdf
      chisq   <-aov.22$"Sum Sq"/mstotal
      aov.22[[3]] <- chisq
      aov.22[[4]] <- 1-pchisq(chisq,df)
      aov.22[neffects+1,3:4] <- NA
                                                 # Iman-Davenport F main trial effect
      if (method==0)
         {aov.22[[5]][1] <- (ncases-1)*chisq[1]/(sumdf-chisq[1])
          aov.22[[6]][1] <- 1-pf(aov.22[[5]][1],df[1],df[neffects+1]) 
          aov.22[neffects+1,5:6] <- NA }
                                                 # type of tables
      if (compact)
         {if (errterm==3) 
             {aov.3     <- aov.22
              nteffects <- neffects+1
              row.names(aov.3)[nteffects] <- paste("Residuals",row.names(aov.3)[nteffects-ngeffects-1])
              if (nfgrp>0)
                 {nteffects <- nteffects+ngeffects+1
                  aov.3[(ngeffects+2):nteffects,] <- aov.22[1:(neffects+1),]
                  aov.3[1:(ngeffects+1),]         <- aov.21[1:(ngeffects+1),]
                  nd<- dim(aov.21)[1]
                  row.names(aov.21)[nd]<-"Residuals Btw.Subj"        
                  row.names(aov.3)[1:(ngeffects+1)]         <- row.names(aov.21)
                  row.names(aov.3)[(ngeffects+2):nteffects] <- row.names(aov.22) 
                  row.names(aov.3)[nteffects] <- paste("Residuals",row.names(aov.3)[nteffects-ngeffects-1]) }
              }
              
          else
             {aov.3[(nteffects+1):(nteffects+neffects+1),] <- aov.22[1:(neffects+1),]
              nteffects <- nteffects+neffects+1
              row.names(aov.3)[nteffects] <- paste("Residuals",row.names(aov.3)[nteffects-1]) }
         }
      else
         {nerg <- nerg+1
          if (nerg==1) erglist         <- list(aov.22)
          if (nerg >1) erglist[[nerg]] <- aov.22 }
      }
   if (compact) 
       erglist <- aov.3

   else
      {if (nfgrp==0) names(erglist) <- names(aov.1)[3:(nerrterm+2)]
       if (nfgrp >0) names(erglist) <- names(aov.1)[2:(nerrterm+2)] }
   if (method>0) erglist <- erglist[,-5]
   }
else                                             # only grouping effects
  { aov.3 <- drop1(aov.2, ~. , test="F")         # SS Type III
    aov.2 <- anova(aov.2)                        # as dataframe
    names(aov.2)[3:5] <- c("Chi Sq","Pr(>Chi)"," ")
                                                 # Chisquare tests
    neffects <- dim(aov.2)[1]-1
    mstotal  <- sum(aov.2[,2])/sum(aov.2[,1])
    aov.2[1:(neffects),2] <- aov.3[2:(neffects+1),2]
    aov.2[1:neffects,3]     <- aov.3[2:(neffects+1),2]/mstotal
    aov.2[,4]               <- 1-pchisq(aov.2[,3],aov.2[,1])
    aov.2[neffects+1,3:4]   <- NA
    aov.2                   <- aov.2[,-5]
    erglist  <- aov.2
  }

if (method==0) attr(erglist,"heading") <- "generalized Kruskal-Wallis/Friedman tests including Iman & Davenport F-tests"
if (method==1) attr(erglist,"heading") <- "generalized van der Waerden tests"
if (method==2) attr(erglist,"heading") <- "Puri & Sen tests"
if (method==3) attr(erglist,"heading") <- "Puri & Sen with inverse normal transformation"
if (pseudo) attr(erglist,"heading") <- paste(attr(erglist,"heading"),"on pseudo ranks")
erglist
}
