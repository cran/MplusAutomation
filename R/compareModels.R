#TODO: Make work with standardized parameter estimates
compareModels <- function(m1, m2, show="all", equalityMargin=c(param=0.0001, pvalue=0.0001), sort="none", showFixed=FALSE, showNS=TRUE, diffTest=FALSE) {
  #options for show:
  # -all: print equal params, unequal params, and unique params
  # -unique: print only unique params
  # -diff: print parameters that differ and unique params
  # -pdiff: print parameters where pvalues differ and unique params
  # -equal: print parameters that are equal
  # -summaries: print comparison of summaries
  # -allsummaries  
  
  #defaults to show equal and unequal params
  require(reshape)
  require(plyr)
  
  #if a single scalar is passed in with no name, then use that value for param and pvalue diffs
  if (length(equalityMargin) == 1) {
    if (is.null(names(equalityMargin)))
      equalityMargin <- c(param=equalityMargin, pvalue=equalityMargin)
    else {
      if (!"param" %in% names(equalityMargin))
        equalityMargin["param"] <- .0001
      if (!"pvalue" %in% names(equalityMargin))
        equalityMargin["pvalue"] <- .0001
    }
  }
  
  cat("\n==============\n\nMplus model comparison\n----------------------\n\n")
  
  if (!is.null(fn <- attr(m1, "filename")))
    cat("------\nModel 1: ", fn, "\n")
  
  if (!is.null(fn <- attr(m2, "filename")))
    cat("Model 2: ", fn, "\n------\n")
  
  if (inherits(m1, c("mplus.summaries", "mplus.model")) &&
      inherits(m2, c("mplus.summaries", "mplus.model")) && any(c("all", "allsummaries", "summaries") %in% show)) {
    
    #compare summaries
    if (inherits(m1, "mplus.model")) m1Summaries <- m1$summaries
    else m1Summaries <- m1 #just copy summaries as is
    
    if (inherits(m2, "mplus.model")) m2Summaries <- m2$summaries
    else m2Summaries <- m2 #just copy summaries as is
    
    cat("\nModel Summary Comparison\n------------------------\n\n")
    
    #compare basic stuff
    
    sumCombined <- rbind.fill(m1Summaries, m2Summaries)
    
    if (!"allsummaries" %in% show) {
      comparators <- c("Title", "Observations", "Estimator", "Parameters", "LL", "AIC", "BIC", "ChiSqM_Value", "ChiSqM_DF",
          "CFI", "TLI", "RMSEA", "SRMR", "WRMR")
      
      sumCombined <- sumCombined[,intersect(comparators, names(sumCombined))]      
    }

    #manual loop seems easier here (tried apply, sapply combos... too painful)
    sumCombinedL <- as.list(sumCombined)
    nameList <- names(sumCombinedL)
    totalPad <- 0
    
    for (var in 1:length(sumCombinedL)) {
      sumCombinedL[[var]] <- lapply(sumCombinedL[[var]], strwrap, width=40, exdent=2)
      len1 <- length(sumCombinedL[[var]][[1]])
      len2 <- length(sumCombinedL[[var]][[2]])
      if (len1 > len2) sumCombinedL[[var]][[2]] <- c(sumCombinedL[[var]][[2]], rep("", len1-len2))
      else if (len2 > len1) sumCombinedL[[var]][[1]] <- c(sumCombinedL[[var]][[1]], rep("", len2-len1))
      padLen <- max(len2,len1) - 1
      if ((var+totalPad+1) <= length(nameList))
        nameList <- c(nameList[1:(var+totalPad)], rep("", padLen), nameList[(var+totalPad+1):length(nameList)])
      else
        nameList <- c(nameList[1:(var+totalPad)], rep("", padLen))
      
      totalPad <- totalPad + padLen      
    }

    wrappedOutput <- cbind(m1=do.call("c", lapply(sumCombinedL, "[[", 1)),
        m2=do.call("c", lapply(sumCombinedL, "[[", 2)))
    
    nameList <- encodeString(nameList, justify="left", width=NA)
    rownames(wrappedOutput) <- nameList
    
    print(wrappedOutput, quote=FALSE)
    
    #chi-square difference testing
    if (diffTest == TRUE && m1Summaries$Estimator %in% c("ML", "MLM", "MLR", "WLS", "WLSM") && m1Summaries$Parameters != m2Summaries$Parameters) {
      if (m1Summaries$Estimator == m2Summaries$Estimator ) {
        if (m1Summaries$Parameters < m2Summaries$Parameters) H0m <- m1Summaries
        else H0m <- m2Summaries
        
        if (m1Summaries$Parameters > m2Summaries$Parameters) H1m <- m1Summaries
        else H1m <- m2Summaries
        
        if (m1Summaries$Estimator == "MLR") {
          #Chi-square difference test for MLR models based on loglikelihood
          cd <- (H0m$Parameters*H0m$LLCorrectionFactor - H1m$Parameters*H1m$LLCorrectionFactor)/(H0m$Parameters - H1m$Parameters)
          
          ChiSqDiff <- -2*(H0m$LL - H1m$LL)/cd
          dfDiff <- (H0m$ChiSqM_DF - H1m$ChiSqM_DF)
          
          cat("\n  MLR Chi-Square Difference test for nested models based on loglikelihood\n  -----------------------------------------------------------------------\n\n")
          cat("  Difference Test Scaling Correction: ", cd, "\n")
          cat("  Chi-square difference: ", round(ChiSqDiff, 4), "\n")
          cat("  Diff degrees of freedom: ", dfDiff, "\n")
          cat("  P-value: ", round(pchisq(ChiSqDiff, dfDiff, lower.tail=FALSE), 4), "\n")
          cat("\n  Note: The chi-square difference test assumes that these models are nested.\n  It is up to you to verify this assumption.\n")
        }
        if (m1Summaries$Estimator %in% c("ML", "MLR", "MLM", "WLSM")) {
          if (m1Summaries$Estimator %in% c("ML", "WLS"))
            #for ML and WLS,  no need to correct chi-square with scaling factors
            ChiSqDiff <- H0m$ChiSqM_Value - H1m$ChiSqM_Value
          else {
            cd <- (H0m$ChiSqM_DF*H0m$ChiSqM_ScalingCorrection - H1m$ChiSqM_DF*H1m$ChiSqM_ScalingCorrection)/(H0m$ChiSqM_DF - H1m$ChiSqM_DF)
            ChiSqDiff <- (H0m$ChiSqM_Value*H0m$ChiSqM_ScalingCorrection - H1m$ChiSqM_Value*H1m$ChiSqM_ScalingCorrection)/cd            
          }
          
          dfDiff <- (H0m$ChiSqM_DF - H1m$ChiSqM_DF)
          
          cat("\n  ", m1Summaries$Estimator, " Chi-Square Difference test for nested models\n  --------------------------------------------\n\n", sep="")
          if (! m1Summaries$Estimator %in% c("WLS", "ML")) cat("  Difference Test Scaling Correction:", cd, "\n")
          cat("  Chi-square difference:", round(ChiSqDiff, 4), "\n")
          cat("  Diff degrees of freedom:", dfDiff, "\n")
          cat("  P-value:", round(pchisq(ChiSqDiff, dfDiff, lower.tail=FALSE), 4), "\n")
          cat("\nNote: The chi-square difference test assumes that these models are nested.\n  It is up to you to verify this assumption.\n")          
        }
      }
      else
        warning("Cannot compute chi-square difference test because different estimators used: ", m1Summaries$Estimator, " and ", m2Summaries$Estimator)
    }
  }
  
  #will blow up in cases where std output with no est_se or pval
  
  #fuzzy equality
  testEquality <- function(v1, v2, margin=0.0000) {
    if (length(v1) != length(v2)) stop ("v1 length != v2 length")
    
    retVec <- aaply(cbind(v1, v2), 1, function(row) {
          if(abs(row["v1"]-row["v2"]) <= margin) return(TRUE)
          else return(FALSE)
        })
    
    return(retVec)
  }
  
  if (inherits(m1, c("mplus.model", "mplus.params")) &&
      inherits(m2, c("mplus.model", "mplus.params")) &&
      any(c("all", "equal", "diff", "pdiff", "unique") %in% show)) {
    
    if (inherits(m1, "mplus.model")) m1Params <- m1$parameters$unstandardized
    else m1Params <- m1
    
    if (inherits(m2, "mplus.model")) m2Params <- m2$parameters$unstandardized
    else m2Params <- m2
    
    #match up paramheader and param combinations
    m1Params$paramCombined <- paste(m1Params$paramHeader, m1Params$param, sep=".")
    m2Params$paramCombined <- paste(m2Params$paramHeader, m2Params$param, sep=".")
    
    #identify columns present in both data.frames
    matchCols <- intersect(m1Params$paramCombined, m2Params$paramCombined)
    m1Only <- setdiff(m1Params$paramCombined, m2Params$paramCombined)
    m2Only <- setdiff(m2Params$paramCombined, m1Params$paramCombined)
    
    m1Params <- reshape::rename(m1Params, c(est="m1_est", se="m1_se", est_se="m1_est_se", pval="m1_pval"))
    m2Params <- reshape::rename(m2Params, c(est="m2_est", se="m2_se", est_se="m2_est_se", pval="m2_pval"))
    
    matchDF <- merge(m1Params[m1Params$paramCombined %in% matchCols,], m2Params[m2Params$paramCombined %in% matchCols,], by=c("paramHeader", "param"), all.x=TRUE, all.y=TRUE)
    matchDF <- subset(matchDF, select=c(-paramCombined.x, -paramCombined.y)) #just retain paramHeader and param
    
    #remove fixed parameters (only if fixed in both models)
    if (!showFixed) {
      matchDF <- subset(matchDF, !(m1_est_se == 999.000 & m2_est_se == 999.000))
      #N.B. I'm dubious about removing fixed from m1/m2 params because then the "unique" output omits these
      m1Params <- subset(m1Params, !m1_est_se == 999.000)
      m2Params <- subset(m2Params, !m2_est_se == 999.000)
    }
    
    #remove non-significant effects, if requested
    if (is.logical(showNS) && showNS==FALSE) showNS <- .05 #default alpha for filtering N.S. results
    if (!showNS==TRUE) {
      matchDF <- subset(matchDF, !(m1_pval > showNS & m2_pval > showNS))
      m1Params <- subset(m1Params, !(m1_pval > showNS))
      m2Params <- subset(m2Params, !(m2_pval > showNS))
    }
    
    if (sort=="type") {
      #add a leading . to the replacement to filter on, by, and with to the top of the results output 
      paramType <- sapply(matchDF$paramHeader, sub, pattern=".*\\.(ON|WITH|BY)$", replacement=".\\1", ignore.case=TRUE, perl=TRUE)
      matchDF <- matchDF[order(paramType, matchDF$paramHeader, matchDF$param),]
    }
    else if (sort=="alphabetical") {
      matchDF <- matchDF[order(matchDF$paramHeader, matchDF$param),]
    }
    
    #order by each parameter match (could make optional)
    matchDF <- matchDF[,c("paramHeader", "param", "m1_est", "m2_est", "m1_se", "m2_se", "m1_est_se", "m2_est_se", "m1_pval", "m2_pval")]
    matchDF <- cbind(matchDF[,1:4], .=rep("|", nrow(matchDF)), matchDF[,5:6], .=rep("|", nrow(matchDF)), matchDF[,7:8], .=rep("|", nrow(matchDF)), matchDF[,9:10])
    
    cat("\n=========\n\nModel parameter comparison\n--------------------------\n")
    paramsEqual <- with(matchDF, testEquality(m1_est, m2_est, equalityMargin["param"]))
    pvalsEqual <- with(matchDF, testEquality(m1_pval, m2_pval, equalityMargin["pvalue"]))
    
    if (any(c("all", "equal") %in% show)) {
      cat("  Parameters present in both models\n=========\n\n")
      cat("  Equal in both models (param. est. diff <= ", equalityMargin["param"], ")\n\n")
      if (any(paramsEqual))
        print(matchDF[paramsEqual==TRUE, ], row.names=FALSE)
      else
        cat("None\n")
    }
    
    if (any(c("all", "diff") %in% show)) {
      cat("\n\n  Parameter estimates that differ between models (param. est. diff > ", equalityMargin["param"], ")\n  ----------------------------------------------\n", sep="")
      if (any(!paramsEqual)) {
        diffDF <- matchDF[paramsEqual==FALSE,]
        if (sort=="maxDiff") {
          paramDiff <- abs(diffDF$m1_est - diffDF$m2_est)
          diffDF <- diffDF[order(paramDiff, decreasing=TRUE),]
        }
        print(diffDF, row.names=FALSE)    
      }
      else
        cat("None\n")
    }
    
    if (any(c("all", "pdiff") %in% show)) {
      cat("\n\n  P-values that differ between models (p-value diff > ", equalityMargin["pvalue"], ")\n  -----------------------------------\n", sep="")
      if (any(!pvalsEqual)) {
        diffDF <- matchDF[pvalsEqual==FALSE,]
        if (sort=="maxDiff") {
          paramDiff <- abs(diffDF$m1_pval - diffDF$m2_pval)
          diffDF <- diffDF[order(paramDiff, decreasing=TRUE),]
        }
        print(diffDF, row.names=FALSE)      
      }
      else
        cat("None\n")
    }
    
    #sort doesn't affect this because it uses matchDF... sort earlier?
    if (any(c("unique", "all") %in% show)) {
      cat("\n\n  Parameters unique to model 1: ", length(m1Only), "\n  -----------------------------\n\n", sep="")
      if (length(m1Only) > 0) {
        m1Remaining <- sum(m1Params$paramCombined %in% m1Only)
        nFiltered <- length(m1Only) - m1Remaining
        
        if (m1Remaining > 0)
          print(m1Params[m1Params$paramCombined %in% m1Only,!names(m1Params)=="paramCombined"], row.names=FALSE)
        
        if (nFiltered > 0) {
          cat("\n\n  ", nFiltered, " filtered from output (fixed and/or n.s.)\n\n", sep="")
          filtered <- setdiff(m1Only, m1Params$paramCombined)
          cat(paste(strwrap(paste(filtered, collapse=", "), width=80, indent=4, exdent=4), collapse="\n"), "\n\n")
        }
      }
      else
        cat("  None\n\n")
      
      cat("\n  Parameters unique to model 2: ", length(m2Only), "\n  -----------------------------\n\n", sep="")
      if (length(m2Only) > 0) {
        m2Remaining <- sum(m2Params$paramCombined %in% m2Only)
        nFiltered <- length(m2Only) - m2Remaining
        
        if (m2Remaining > 0)
          print(m2Params[m2Params$paramCombined %in% m2Only,!names(m2Params)=="paramCombined"], row.names=FALSE)
        
        if (nFiltered > 0) {
          cat("\n\n  ", nFiltered, " filtered from output (fixed and/or n.s.)\n\n", sep="")
          filtered <- setdiff(m2Only, m2Params$paramCombined)
          cat(paste(strwrap(paste(filtered, collapse=", "), width=80, indent=4, exdent=4), collapse="\n"), "\n\n")
        }
        
      }
      else
        cat(" None\n\n")  
    }
    
  }
  
  cat("\n==============\n")
}


#detritus from debacle of printing summaries side-by-side

#doeesn't work with print at the moment
#    if ("Title" %in% names(sumCombined))
#      print(cbind(sapply(sumCombined$Title, function(title) {
#            return(strwrap(title, width=25, exdent=2))
#          }, USE.NAMES=FALSE)), quote=FALSE)
#      sumCombined$Title <- sapply(sumCombined$Title, function(title) {
#            return(paste(strwrap(title, width=25, exdent=2), collapse="\n"))
#          })

#      
#    m1Printout <- capture.output(print(t(sumCombined[1,]), quote=FALSE))
#    m2Printout <- capture.output(print(t(sumCombined[1,]), quote=FALSE))
#      
#    printout <- capture.output(print(t(sumCombined), quote=FALSE, width=10000))
#    printout <- strwrap(encodeString(t(sumCombined), width=5000), width=30, exdent=2)
#    
#convert from two rows to two columns    
#sumCombined[1,"Title"] <- "short title"

#detritus from earlier effort
#    m2Wrapped <- lapply(sumCombinedL, "[[", 2)
#    
#    sumCombined <- apply(sumCombined, c(1,2), strwrap, width=30, exdent=2)
#    
#    #this will pad lists where one is longer than the other
#    sumCombined2 <- aaply(sumCombined, 2, function(col) {
#          len1 <- length(col[[1]])
#          len2 <- length(col[[2]])
#          if (len1 > len2) col[[2]] <- c(col[[2]], rep("", len1-len2))
#          else if (len2 > len1) col[[1]] <- c(col[[1]], rep("", len2-len1))
#          return(lapply(col, cbind))
#        })
#    
#    
#    
#    lapply(sumCombined2, function(el) {
#          browser()
#        })
#    do.call("rbind", lapply(sumCombined2, "rbind"))
#    #the above call converts to a list of the same dimension, but with varying length depending on wrap
#    
#    sumCombinedL <- lapply(sumCombined, function(col) {
#          browser()
#          len1 <- length(col[1])
#          len2 <- length(col[2])
#          if (len1 > len2) col[2] <- c(col[2], rep("", len1-len2))
#          else if (len2 > len1) col[1] <- c(col[1], rep("", len2-len1))
#          
#        })
#    sumCombined <- t(sumCombined)
#    
#    print(t(sumCombined[,!names(sumCombined)=="Title"]), quote=FALSE)
#    
#    cat(strwrap(capture.output(print(t(sumCombined), quote=FALSE)), width=30, exdent=2), sep="\n")
