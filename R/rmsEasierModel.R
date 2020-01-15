#library(data.table)

#' @export
#'
nonLinearTest <- function(rawData, outVars, xVars, modelType = "lrm", uniqueSampleSize = 6) {
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
  }
  return(resultOut)
}



# make report table for multipl logistic regression models
## outVars should be list if doing survival model
#' @export
#'
modelTable <- function(dataForModelAll, outVars, interestedVars, adjVars = NULL, nonLinearVars = NULL, extractStats = NULL,
                       modelType = "lrm", printModel = FALSE, returnKable = TRUE) {
  modelType <- match.arg(modelType, c("lrm", "cph", "ols"))
  modelFun <- get(modelType)
  modelResultAll <- NULL




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
          formulaForModel <- gsub(paste0(" ", nonLinearVarOne, " "), paste0(" rcs(", nonLinearVarOne, ",3) "), formulaForModel)
        }
      }
      formulaForModel <- as.formula(formulaForModel)

      dataForModel <- dataForModelAll[, c(outVar, varForModelOne)]
      for (temp in varForModelOne) { # change all numbers with only two values in dataForModel into factor
        if (length(unique(dataForModel[, temp])) <= 2) {
          dataForModel[, temp] <- factor(dataForModel[, temp])
        }
      }

      ddist <<- datadist(dataForModel, n.unique = 5)
      options(datadist = "ddist")
      modelResult <<- modelFun(formulaForModel, data = dataForModel)
      if (printModel) {
        print(modelResult)
#        plot(Predict(modelResult))
      }


      # extract result, may have many variables in varOne
      for (i in 1:length(varOne)) {
        varOneToExtract <- varOne[i]
        varOneInd <- grep(varOneToExtract, names(modelResult$coefficients))

        if (length(varOneInd) > 0) {
          #        coefficientOne<-round(modelResult$coefficients[varOneInd],3)
          pValueOne <- (pnorm(abs(modelResult$coef / sqrt(diag(modelResult$var))), lower.tail = F) * 2)[varOneInd]
          pValueOne <- showP(pValueOne, text = "", digits = 4)

          #        coefficientOne=paste(coefficientOne,collapse="; ")
#          pValueOne <- paste(pValueOne, collapse = "; ") #for continus variable only, add this in next part
        } else {
          warning(paste0("Can't find interested var name in model result: ", paste(varOneToExtract, collapse = ", ")))
          next
        }

        if (is.factor(dataForModel[, varOneToExtract]) || is.character(dataForModel[, varOneToExtract])) { # interested var is factor
          if (is.factor(dataForModel[, varOneToExtract])) {
            varOneRef <- levels(dataForModel[, varOneToExtract])[1]
          } else { # character
            varOneRef <- unique(dataForModel[, varOneToExtract])[1]
          }
          summaryArgList <- list(modelResult, varOneRef, est.all = FALSE)
          names(summaryArgList)[2] <- varOneToExtract
          modelResultSummary <- round(do.call(summary, summaryArgList), 3)
          #browser()
          varOneInd <- grep(varOneToExtract, row.names(modelResultSummary))
          if (modelType == "ols") { #one row, no odds ratio
            varOneOut <- data.frame(row.names(modelResultSummary)[varOneInd], pValueOne, matrix(modelResultSummary[varOneInd, c(4, 6, 7)], ncol = 3), matrix("", ncol = 6, nrow = length(varOneInd)), stringsAsFactors = FALSE)
          } else { #two rows, second row odds ratio
            varOneOut <- data.frame(row.names(modelResultSummary)[varOneInd], modelResultSummary[varOneInd, c(4)], pValueOne, matrix(modelResultSummary[varOneInd + 1, c(4, 6, 7)], ncol = 3), matrix("", ncol = 6, nrow = length(varOneInd)), stringsAsFactors = FALSE)
          }

        } else { # interested var is continous
          pValueOne <- paste(pValueOne, collapse = "; ")
          varOneMedianValue <- median(dataForModel[, varOneToExtract], na.rm = TRUE)
          summaryArgList <- list(modelResult, c(varOneMedianValue, varOneMedianValue + 1), est.all = FALSE)
          names(summaryArgList)[2] <- varOneToExtract
          #print(summaryArgList)
          modelResultSummaryUnit <- round(do.call(summary, summaryArgList), 3) # Value of One Unit Change (from median+1 to median)
          summaryArgList <- list(modelResult, varOneToExtract, est.all = FALSE)
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
        if (modelType == "ols") { #no odds ratio
          colnames(varOneOut) <- c(
            "InterestedVar", "P", "Effect (One Unit)", "Effect (Lower 95%)", "Effect (Upper 95%)",
            "Value (25% Quantile)", "Value (75% Quantile)", "Value Diff (75%-25%)", "Effect (Diff: 75%-25%)", "Effect (Diff, Lower 95%)", "Effect (Diff, Upper 95%)"
          )
        } else {
          colnames(varOneOut) <- c(
            "InterestedVar", "Effect (One Unit)", "P", "Odds Ratio (One Unit)", "OR (Lower 95%)", "OR (Upper 95%)",
            "Value (25% Quantile)", "Value (75% Quantile)", "Value Diff (75%-25%)", "Odds Ratio (Diff: 75%-25%)", "OR (Diff, Lower 95%)", "OR (Diff, Upper 95%)"
          )
        }
        varOneOut <- data.frame(Formula = paste0(modelType, " (", as.character(as.expression(formulaForModel)), ")"), varOneOut, stringsAsFactors = FALSE, check.names = FALSE)

        if (!is.null(extractStats)) {
          varOneOut <- c(varOneOut, round(modelResult$stats[extractStats], 3))
        }
        modelResultAll <- rbind(modelResultAll, varOneOut)


      }
    }
  }
  row.names(modelResultAll) <- NULL
  if (returnKable) {
    temp <- apply(modelResultAll, 2, function(x) all(x == "")) # remove spaces
    kable(modelResultAll[, which(!temp)])
  } else {
    return(modelResultAll)
  }
}
