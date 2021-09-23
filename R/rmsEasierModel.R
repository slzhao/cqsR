#library(data.table)

#' @export
#'
nonLinearTest <- function(rawData, outVars, xVars, modelType = "lrm", uniqueSampleSize = 6,returnKable=FALSE) {
  modelType <- match.arg(modelType, c("lrm", "cph", "ols"))
  modelFun <- get(modelType)

  resultOut <- NULL
  if (length(outVars) == length(xVars)) { # One outVar to one XVar
    for (i in 1:length(outVars)) {
      outVarOne <- outVars[[i]]
      xVarOne <- xVars[i]

      if (class(rawData[, xVarOne]) == "numeric" | class(rawData[, xVarOne]) == "integer") {
        if (length(unique(rawData[, xVarOne])) >= uniqueSampleSize) {
          if (modelType == "cph") {
            formulaForModel <- as.formula(paste0("Surv(", outVarOne[1], ", ", outVarOne[2], ")", "~rcs(", xVarOne, ",3)"))
          } else {
            formulaForModel <- as.formula(paste0(outVarOne, "~rcs(", xVarOne, ",3)"))
          }
          #browser()
          #formulaForModel <- as.formula(paste0(outVarOne, "~rcs(", xVarOne, ",3)"))
          modelResult <- modelFun(formulaForModel, data = rawData)
          modelResultAnova <- anova(modelResult)
          resultOne <- c(paste(outVarOne,collapse=","), xVarOne, paste0(as.expression(formulaForModel)), as.vector(modelResultAnova[, 3]))
          resultOut <- rbind(resultOut, resultOne)
        }

      }

    }
  } else {
    for (outVarOne in outVars) { # Loop all outVars and XVars
      for (xVarOne in xVars) {
        if ("numeric" %in% class(rawData[, xVarOne]) | "integer" %in% class(rawData[, xVarOne])) {
          if (length(unique(rawData[, xVarOne])) >= uniqueSampleSize) {
            if (modelType == "cph") {
              formulaForModel <- as.formula(paste0("Surv(", outVarOne[1], ", ", outVarOne[2], ")", "~rcs(", xVarOne, ",3)"))
            } else {
              formulaForModel <- as.formula(paste0(outVarOne, "~rcs(", xVarOne, ",3)"))
            }
            #browser()
            modelResult <- modelFun(formulaForModel, data = rawData)
            modelResultAnova <- anova(modelResult)
            resultOne <- c(paste(outVarOne,collapse=","), xVarOne, paste0(as.expression(formulaForModel)), showP(modelResultAnova[1:3, "P"], 3, text = ""))
            resultOut <- rbind(resultOut, resultOne)
          }
        }

      }
    }
  }
#  browser()
  if (!is.null(resultOut) && nrow(resultOut)>0) {
    row.names(resultOut) <- NULL
    colnames(resultOut) <- c("Outcome", "X", "Formula", "P (Variable)", paste0("P (",row.names(modelResultAnova)[2:3],")"))
    if (returnKable) {
      #    temp <- apply(resultOut, 2, function(x) all(x == "")) # remove spaces
      #    kable(resultOut[, which(!temp)],caption ="Non-linear Test")
      kable(resultOut,caption ="Non-linear Test for continuous variables")
    } else {
      return(resultOut)
    }
  } else {
    return(resultOut)
  }
}

#export p and coef from modelResult
#varOne is interested Vars
#' @export
#'
exportModelResult=function(modelResult, varOne,extractStats=NULL,reportAnovaP=TRUE) {
  supportedModelTypes=c("lrm", "ols", "cph")
  modelType=intersect(class(modelResult),supportedModelTypes)[1]
  if (length(modelType)==0) {
    stop("Can't find modelType. Now only supports ",paste(supportedModelTypes,collapse=";"))
  }

  modelResultOut=NULL

  ######################
  #get p value
  ######################
  for (i in 1:length(varOne)) {
    varOneToExtract <- varOne[i]
    varOneInd <- grep(varOneToExtract, names(modelResult$coefficients))
    varOneToExtractType=modelResult$Design$assume[which(modelResult$Design$name==varOneToExtract)]

    if (length(varOneInd) > 0) {
      if (reportAnovaP && (varOneToExtractType=="rcspline" | varOneToExtractType=="polynomial")) { #for continuous variables and with non-linear term only
        pValueOne=anova(modelResult)[varOneToExtract,"P"]
      } else {
        if (modelType=="ols") { #ols, linear regression
          pValueOne=summary.lm(modelResult)$coefficients[varOneInd,"Pr(>|t|)"]
        } else { #lrm or cph, wald Z test to get p value
          pValueOne <- (pnorm(abs(modelResult$coef / sqrt(diag(modelResult$var))), lower.tail = F) * 2)[varOneInd]
        }
      }
      pValueOne <- showP(pValueOne, text = "", digits = 4)

    } else {
      warning(paste0("Can't find interested var name in model result: ", paste(varOneToExtract, collapse = ", ")))
      next
    }

    ######################
    #get coef/effect
    ######################

    ##get data limits and type
    #varLimitsTable=get(options("datadist")[[1]])[["limits"]][,varOneToExtract,drop=FALSE]
    #modelResult$Design

    if (varOneToExtractType=="rcspline") { #non linear effect for continuous variable. May have more than one p values
      pValueOne <- paste(pValueOne, collapse = "; ")
    }

    varOneToExtractLimits=modelResult$Design$limits[,which(modelResult$Design$name==varOneToExtract),drop=FALSE]
    varOneRef=varOneToExtractLimits["Adjust to",]


    if (varOneToExtractType=="category") { # interested var is factor
      summaryArgList <- list(quote(modelResult), varOneRef, est.all = FALSE)
      names(summaryArgList)[2] <- varOneToExtract
      modelResultSummary <- round(do.call(summary, summaryArgList), 3)
      #browser()
      varOneInd <- grep(varOneToExtract, row.names(modelResultSummary))
      if (modelType == "ols") { #one row, no odds ratio
        varOneOut <- data.frame(row.names(modelResultSummary)[varOneInd], pValueOne, matrix(modelResultSummary[varOneInd, c(4, 6, 7)], ncol = 3), matrix("", ncol = 6, nrow = length(varOneInd)), stringsAsFactors = FALSE)
      } else { #two rows, second row odds ratio
        varOneOut <- data.frame(row.names(modelResultSummary)[varOneInd], modelResultSummary[varOneInd, c(4)], pValueOne, matrix(modelResultSummary[varOneInd + 1, c(4, 6, 7)], ncol = 3), matrix("", ncol = 6, nrow = length(varOneInd)), stringsAsFactors = FALSE)
      }
    } else { # interested var is continous, need both +1 effect and 25%-75% quantile change effect
      #varOneRef is median value
      summaryArgList <- list(quote(modelResult), c(varOneRef, varOneRef + 1), est.all = FALSE)
      names(summaryArgList)[2] <- varOneToExtract
      #print(summaryArgList)
      modelResultSummaryUnit <- round(do.call(summary, summaryArgList), 3) # Value of One Unit Change (from median+1 to median)
      summaryArgList <- list(quote(modelResult), varOneToExtract, est.all = FALSE)
      #print(summaryArgList)
      modelResultSummary <- round(do.call(summary, summaryArgList), 3) # Value at 75% Quantile to 25% Quantile
      # varOneOut=c(coefficientOne,pValueOne,modelResultSummaryUnit[2,c(4,6,7)],modelResultSummary[2,c(1,2,3,4,6,7)])
      varOneInd <- grep(varOneToExtract, row.names(modelResultSummary))
      #browser()
      if (modelType == "ols") { #one row, no odds ratio
        varOneOut <- data.frame(row.names(modelResultSummary)[varOneInd], pValueOne, matrix(modelResultSummaryUnit[varOneInd , c(4, 6, 7)], ncol = 3), matrix(modelResultSummary[varOneInd, c(1, 2, 3, 4, 6, 7)], ncol = 6), stringsAsFactors = FALSE)
      } else {
        varOneOut <- data.frame(row.names(modelResultSummary)[varOneInd], modelResultSummaryUnit[varOneInd, c(4)], pValueOne, matrix(modelResultSummaryUnit[varOneInd + 1, c(4, 6, 7)], ncol = 3), matrix(modelResultSummary[varOneInd + 1, c(1, 2, 3, 4, 6, 7)], ncol = 6), stringsAsFactors = FALSE)
      }
    }

    if (modelType == "ols") { #linear regression no odds ratio
      colnames(varOneOut) <- c(
        "InterestedVar", "P", "Effect (One Unit)", "Effect (Lower 95%)", "Effect (Upper 95%)",
        "Value (25% Quantile)", "Value (75% Quantile)", "Value Diff (75%-25%)", "Effect (Diff: 75%-25%)", "Effect (Diff, Lower 95%)", "Effect (Diff, Upper 95%)"
      )
    } else if (modelType == "cph") { #hazard ratio
      colnames(varOneOut) <- c(
        "InterestedVar", "Effect (One Unit)", "P", "Hazard Ratio (One Unit)", "HR (Lower 95%)", "HR (Upper 95%)",
        "Value (25% Quantile)", "Value (75% Quantile)", "Value Diff (75%-25%)", "Hazard Ratio (Diff: 75%-25%)", "HR (Diff, Lower 95%)", "HR (Diff, Upper 95%)"
      )
    } else {
      colnames(varOneOut) <- c(
        "InterestedVar", "Effect (One Unit)", "P", "Odds Ratio (One Unit)", "OR (Lower 95%)", "OR (Upper 95%)",
        "Value (25% Quantile)", "Value (75% Quantile)", "Value Diff (75%-25%)", "Odds Ratio (Diff: 75%-25%)", "OR (Diff, Lower 95%)", "OR (Diff, Upper 95%)"
      )
    }

    #recored event level as sometimes event is factor and need to know which level is event (1)
    if (modelType == "lrm") {
      outVarEvent=paste(paste0(rev(names(modelResult$freq)),"(",rev((modelResult$freq)),")"),collapse=" : ")
      varOneOut <- data.frame(Event=outVarEvent,varOneOut, stringsAsFactors = FALSE, check.names = FALSE)
    }

    varOneOut <- data.frame(Formula = paste0(modelType, " (", as.character(as.expression(modelResult$sformula)), ")"),varOneOut, stringsAsFactors = FALSE, check.names = FALSE)

    if (!is.null(extractStats)) {
      varOneOut <- c(varOneOut, round(modelResult$stats[extractStats], 3))
    }
    modelResultOut <- rbind(modelResultOut, varOneOut)
  }

  return(modelResultOut)

}




# make report table for multipl logistic regression models
## outVars should be list if doing survival model
#' @export
#'
modelTable <- function(dataForModelAll, outVars, interestedVars, adjVars = NULL,
                       nonLinearVars = NULL, nonLinearFunName="rcs",nonLinearFunPar=3,
                       extractStats = NULL,modelType = "lrm", printModel = FALSE, printModelFigure = printModel,
                       returnKable = FALSE,returnModel = FALSE,uniqueSampleSize=5,
                       reportAnovaP=TRUE,adjto.cat='first') {
  modelType <- match.arg(modelType, c("lrm", "cph", "ols"))
  modelFun <- get(modelType)
  modelResultAll <- NULL
  modelAll <- list()

  for (outVar in outVars) {
    for (varOne in interestedVars) {
      varForModelOne <- c(varOne, adjVars)
      if (modelType == "cph") {
        formulaForModel <- paste("Surv(", outVar[1], ", ", outVar[2], ")", "~", paste0(varForModelOne, collapse = " + "), " ")
      } else {
        formulaForModel <- paste(outVar, "~", paste0(varForModelOne, collapse = " + "), " ")
      }
      if (!is.null(nonLinearVars)) {
        for (nonLinearVarOne in nonLinearVars) {
          formulaForModel <- gsub(paste0(" ", nonLinearVarOne, " "), paste0(" ",nonLinearFunName,"(", nonLinearVarOne, ",",nonLinearFunPar,") "), formulaForModel)
        }
      }
      formulaForModel <- as.formula(formulaForModel)

      dataForModel <- dataForModelAll[, c(outVar, varForModelOne)]
      for (temp in varForModelOne) { # change all numbers with only uniqueSampleSize values in dataForModel into factor
        if (length(unique(na.omit(dataForModel[, temp]))) <= uniqueSampleSize) {
          dataForModel[, temp] <- factor(dataForModel[, temp])
        }
      }

      ddist <<- datadist(dataForModel, n.unique = uniqueSampleSize,adjto.cat=adjto.cat)
      options(datadist = "ddist")
      modelResult <- modelFun(formulaForModel, data = dataForModel,x=TRUE,y=TRUE)
      if (printModel) {
        print(paste0("Model formula: ",as.character(as.expression(formulaForModel))))
        print(modelResult)
      }
      if (printModelFigure) {
        print(plot(Predict(modelResult),ylab=outVar))
      }
      if (returnModel) {
        modelAll=c(modelAll,list(modelResult))
      }

      # extract result, may have many variables in varOne
      modelResultOut=exportModelResult(modelResult,varOne,reportAnovaP = reportAnovaP)
      modelResultAll=rbind(modelResultAll,modelResultOut)
    }
  }
  row.names(modelResultAll) <- NULL

  if (returnKable) {
    temp <- apply(modelResultAll, 2, function(x) all(x == "")) # remove spaces
    print(kable(modelResultAll[, which(!temp)],caption ="Regression Model Result Summary"))
  }

  if (returnModel) {
    return(modelAll)
  } else {
    return(modelResultAll)
  }
}

#' summary/predict model result based on variable's value
#' @export
#'
easierSummaryByValue=function(modelResult,varOneToExtract,varOneToExtractValues) {
  if ("lrm" %in% class(modelResult)) {
    modelType="lrm"
  } else if ("ols" %in% class(modelResult)) {
    modelType="ols"
  } else {
    modelType="cph"
  }

  summaryArgList <- list(quote(modelResult), c((varOneToExtractValues[1]), (varOneToExtractValues[2])), est.all = FALSE)
  names(summaryArgList)[2] <- varOneToExtract
  #print(summaryArgList)
  #browser()
  modelResultSummary <- round(do.call(summary, summaryArgList), 3)
  varOneInd <- grep(varOneToExtract, row.names(modelResultSummary))
  #browser()
  if (modelType == "ols") { #one row, no odds ratio
    varOneOut <- data.frame(row.names(modelResultSummary)[varOneInd], matrix(modelResultSummary[varOneInd, c(1, 2, 3, 4, 6, 7)], ncol = 6), stringsAsFactors = FALSE)
  } else {
    varOneOut <- data.frame(row.names(modelResultSummary)[varOneInd], matrix(modelResultSummary[varOneInd + 1, c(1, 2, 3, 4, 6, 7)], ncol = 6), stringsAsFactors = FALSE)
  }

  modelFormula=paste0(modelType,"(",as.character(as.expression(modelResult$sformula)),")")
  varOneOut=c(modelFormula,varOneOut)

  if (modelType == "ols") { #linear regression no odds ratio
    names(varOneOut) <- c(
      "Formula","InterestedVar", "Value 1", "Value 2", "Value Diff", "Effect (Diff)", "Effect (Diff, Lower 95%)", "Effect (Diff, Upper 95%)"
    )
  } else if (modelType == "cph") { #hazard ratio
    names(varOneOut) <- c(
      "Formula","InterestedVar", "Value 1", "Value 2", "Value Diff", "Hazard Ratio (Diff)", "HR (Diff, Lower 95%)", "HR (Diff, Upper 95%)"
    )
  } else {
    names(varOneOut) <- c(
      "Formula","InterestedVar", "Value 1", "Value 2", "Value Diff", "Odds Ratio (Diff)", "OR (Diff, Lower 95%)", "OR (Diff, Upper 95%)"
    )
  }
  return(varOneOut)
}

