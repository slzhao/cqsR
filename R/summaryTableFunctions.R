# Use codes in reportExample.Rmd to generate data
# ## Make/Load Data
# rawData<-matrix(nrow=200,ncol=10,rnorm(200*10))
# row.names(rawData)<-paste0("Sample",1:200)
# colnames(rawData)<-paste0("Feature",1:10)
# rawData<-data.frame(rawData,FeatureYN1=sample(c(rep(0,100),rep(1,100)),200),FeatureYN2=as.character(sample(c(rep(0,100),rep(1,100)),200)),FeatureCategory1=sample(c("D","E"),200,replace = TRUE),FeatureCategory2=sample(c("A","B","C","D"),200,replace = TRUE),stringsAsFactors = TRUE)
#
# tableOut=summaryTable(rawData,groupCol="FeatureYN1",varCols=c("FeatureYN2"))
# tableOut=summaryTable(rawData,varCols=c("Feature6"),varColsPaired=c("Feature7"))
# tableOut=summaryTable(rawData,groupCol="FeatureYN1",varCols=c("FeatureYN2","Feature6"))
# printSummaryTable(tableOut)
# tableOut1=summaryTable(rawData,varCols=c("FeatureYN2","Feature6"),varColsPaired=c("FeatureYN1","Feature7"))
# tableOut2=summaryTable(rawData,varCols=c("FeatureYN2","Feature6"),varColsPaired=c("FeatureYN1","Feature7"),pairedTest=TRUE)
# printSummaryTable(tableOut1)
# printSummaryTable(tableOut2)
#
# tableOut=summaryTableContinus(rawData,variables=c("FeatureYN2","Feature6"),groupVariable="Feature7")
# printSummaryTableContinus(tableOut)

#require(htmlTable)
#library(Hmisc)

#' @export
#'
summaryTable<-function(rawData,groupCol=NULL,varCols,varColsPaired=NULL,groupColLabel=NULL,pairedTest=FALSE,minUnique=5,NotPairedCatTestFun=chisq.test) {
  if (is.null(groupCol) & is.null(varColsPaired)) { #at least one of groupCol or varColsPaired should be defined
    stop(pate0("at least one of groupCol or varColsPaired should be defined"))
  }
  if (is.null(varColsPaired)) {
    rawDataPairedOrderedGroup1=rawData[which(as.numeric(as.factor(rawData[,groupCol]))==1),]
    rawDataPairedOrderedGroup2=rawData[which(as.numeric(as.factor(rawData[,groupCol]))==2),]
    if (pairedTest) {
      message(paste0("Will perform paired test, Please confirm Samples were correctly matched/paired by groupCol."))
    }
    if (is.null(groupColLabel)) {
      groupColLabel=levels(as.factor(rawData[,groupCol]))
    }
    groupColCount=c()
  } else {
    rawDataPairedOrderedGroup1=rawData[,varCols,drop=FALSE]
    rawDataPairedOrderedGroup2=rawData[,varColsPaired,drop=FALSE]
    if (is.null(groupColLabel)) {
      if (pairedTest) {
        groupColLabel=c("Pre","Post")
      } else {
        groupColLabel=c("Group 1","Group 2")
      }
    }
  }
  groupColCount1=length(which(apply(rawDataPairedOrderedGroup1,1,function(x) !all(is.na(x)))))
  groupColCount2=length(which(apply(rawDataPairedOrderedGroup2,1,function(x) !all(is.na(x)))))
  groupColCount=c(groupColCount1,groupColCount2)

  tableAll=NULL
  for (i in 1:length(varCols)) {
    varCol=varCols[i]
    if (!is.null(varColsPaired)) {
      varColPaired=varColsPaired[i]
    } else {
      varColPaired=varCols[i]
    }
    if (varCol==varColPaired) {
      varColLabel=varCol
    } else {
      varColLabel=paste0(varCol," (",groupColLabel[1],") vs ",varColPaired," (",groupColLabel[2],")")
    }
    if (length(unique(rawData[,varCol]))<minUnique | is.factor(rawData[,varCol]) | is.character(rawData[,varCol])) { #categorical, or less than minUnique of unique numbers, make it a categorical data
      #make factor or numeric into character to match variables
      dataOneGroup1=as.character(rawDataPairedOrderedGroup1[,varCol])
      dataOneGroup2=as.character(rawDataPairedOrderedGroup2[,varColPaired])

      if (!pairedTest) { #Not Paired chisq.test
        dataForTable=data.frame(Var=c(dataOneGroup1,dataOneGroup2),
                                Group=c(rep(groupColLabel,c(nrow(rawDataPairedOrderedGroup1),nrow(rawDataPairedOrderedGroup2)))))
        matrixForTest=table(dataForTable)
        if (ncol(matrixForTest)==1 | nrow(matrixForTest)==1) {
          pValue=NA
          statistic=NA
        } else {
          testResult=NotPairedCatTestFun(matrixForTest)
          pValue=testResult$p.value
          if ("statistic" %in% names(testResult)) {
            statistic=testResult$statistic
          } else {
            statistic=""
          }

          dataOneVariableCountAll=rowSums(matrixForTest)
        }
        if (is.na(pValue)) {
          dataOneVariableTestResult=""
        } else {
          dataOneVariableTestResult=c(paste0(ifelse(statistic=="","",paste0(names(statistic),"=",round(statistic,2),"; ")),showP(pValue)),rep("",length(dataOneVariableCountAll)))
        }
        tableOneOut<-cbind(c(paste0('<p align="left"><b>',varColLabel,'</b></p>'),paste0(" ",names(dataOneVariableCountAll)," ")),
                           c("",countToPercent(dataOneVariableCountAll)),
                           rbind(c(rep("",ncol(matrixForTest))),countToPercent(matrixForTest)),
                           dataOneVariableTestResult
        )
      } else { #paired test mcnemar.test
        dataForTable=data.frame(Pre=dataOneGroup1,Post=dataOneGroup2)

        #Make sure dataOneVariableCount1 is the same as dataOneVariableCount2
        #dataOneVariableCount1=table(rawDataPairedOrderedGroup1[,varCol])
        #dataOneVariableCount2=table(rawDataPairedOrderedGroup2[,varCol])

        matrixForTest=table(dataForTable)
        if (ncol(matrixForTest)==1 & nrow(matrixForTest)==1) {
          pValue=NA
          statistic=NA
        } else {
          allLevels=unique(c(colnames(matrixForTest),row.names(matrixForTest)))
          colMissingLevel=setdiff(allLevels,colnames(matrixForTest))
          rowMissingLevel=setdiff(allLevels,row.names(matrixForTest))
          if (length(colMissingLevel)>0) {
            for (colMissingLevelOne in colMissingLevel) {
              matrixForTest<-cbind(matrixForTest,rep(0,length(allLevels)-length(rowMissingLevel)))
              colnames(matrixForTest)[ncol(matrixForTest)]=colMissingLevelOne
            }
            matrixForTest=matrixForTest[,allLevels]
          }
          if (length(rowMissingLevel)>0) {
            for (rowMissingLevelOne in rowMissingLevel) {
              matrixForTest<-rbind(matrixForTest,rep(0,length(allLevels)))
              row.names(matrixForTest)[nrow(matrixForTest)]=rowMissingLevelOne
            }
            matrixForTest=matrixForTest[allLevels,allLevels]
          }

          pValue=mcnemar.test(matrixForTest)$p.value
          statistic=mcnemar.test(matrixForTest)$statistic
          dataOneVariableCount1=rowSums(matrixForTest)
          dataOneVariableCount2=colSums(matrixForTest)
        }
        if (is.na(pValue)) {
          dataOneVariableTestResult=""
        } else {
          dataOneVariableTestResult=c(paste0(names(statistic),"=",round(statistic,2),"; ",showP(pValue)),rep("",length(dataOneVariableCount1)))
        }
        tableOneOut<-cbind(c(paste0('<p align="left"><b>',varColLabel,'</b></p>'),paste0(" ",names(dataOneVariableCount1)," ")),
                           c("",countToPercent(rowSums(cbind(dataOneVariableCount1,dataOneVariableCount2)))),
                           c("",countToPercent(dataOneVariableCount1)),
                           c("",countToPercent(dataOneVariableCount2)),
                           dataOneVariableTestResult
        )
      }
      tableAll<-rbind(tableAll,tableOneOut)
    } else {#continus, wilcox rank sum
      if (!pairedTest) { #Not Paired test
        testResult=wilcox.test(rawDataPairedOrderedGroup1[,varCol],rawDataPairedOrderedGroup2[,varColPaired],paired = FALSE)
      } else { #paired test
        testResult=wilcox.test(rawDataPairedOrderedGroup1[,varCol],rawDataPairedOrderedGroup2[,varColPaired],paired = TRUE)
      }
      pValue=testResult$p.value
      statistic=testResult$statistic
      if (is.na(pValue)) {
        dataOneVariableTestResult=""
      } else {
        dataOneVariableTestResult=paste(paste0(names(statistic),"=",statistic),"; ",showP(pValue),collapse="; ")
      }

      temp1=which(!is.na(rawDataPairedOrderedGroup1[,varCol]))
      temp2=which(!is.na(rawDataPairedOrderedGroup2[,varColPaired]))
      dataOneVariableCount=c(length(temp1),length(temp2))
      sampleInBothInd=intersect(temp1,temp2)
      if (pairedTest) { #paired test, only summary samples in both group
        group1Summary=summaryFun(rawDataPairedOrderedGroup1[sampleInBothInd,varCol])
        group2Summary=summaryFun(rawDataPairedOrderedGroup2[sampleInBothInd,varColPaired])
      } else {
        group1Summary=summaryFun(rawDataPairedOrderedGroup1[,varCol])
        group2Summary=summaryFun(rawDataPairedOrderedGroup2[,varColPaired])
      }

      if (varCol != varColPaired) {
        #dataOneVariableCount=paste0(c(paste0(c(varCol,varColPaired),"=")),dataOneVariableCount,collapse="; ")
        dataOneVariableCount=paste0(c(varCol,varColPaired),paste0(" (",dataOneVariableCount,")"),collapse="; ")
      } else {
        #dataOneVariableCount=paste0(c(paste0(groupColLabel,"=")),dataOneVariableCount,collapse="; ")
        dataOneVariableCount=paste0(groupColLabel,paste0(" (",dataOneVariableCount,")"),collapse="; ")
      }
      if (pairedTest) { #paired test, count N with value in both of two paired groups
        dataOneVariableCount=paste0(dataOneVariableCount,"; Both=",length(sampleInBothInd))
      }

      tableOneOut<-c(paste0('<p align="left"><b>',varColLabel,'</b></p>'),
                     dataOneVariableCount,
                     group1Summary,
                     group2Summary,
                     dataOneVariableTestResult
      )
      tableAll<-rbind(tableAll,tableOneOut)
    }

  }
  row.names(tableAll)<-NULL
  colnames(tableAll)=c("","N",paste0(groupColLabel," (",groupColCount,")"),"Test Statistic")
  return(tableAll)
}

#' @export
#'
printSummaryTable<-function(tableOut) {
  plSignLabel=markupSpecs[["html"]]$plminus
  summaryFootContent=""
  if (any(grepl("X-squared",tableOut[,"Test Statistic"])) | !all(grepl("^<p align=",tableOut[,1]))) { #have categorical variable
    summaryFootContent=paste0(summaryFootContent,"For categorical variable, numbers after proportions are counts; ")
  }
  if (any(grepl("V=|W=",tableOut[,"Test Statistic"]))) { #have continuous variable
    summaryFootContent=paste0(summaryFootContent, "For continuous variable, a b c (x",plSignLabel,"s). a b c represent the lower quartile a, the median b, and the upper quartile c in different categories. x",plSignLabel,"s represents Mean",plSignLabel,"SD.")
  }

  testFootContentMcNemar=ifelse(any(grepl("McNemar",tableOut[,"Test Statistic"])),"McNemar's chi-squared test for symmetry of rows and columns in a two-dimensional contingency table; ","")
  testFootContentChisq=ifelse(any(grepl("X-squared",tableOut[,"Test Statistic"])),"Chi-squared test for categorical variable; ","")
  if (testFootContentChisq=="" & !all(grepl("^<p align=",tableOut[,1]))) { #No Chi-squared test used and having categorical data in table
    testFootContentChisq="Fisher's exact test for categorical variable; "
  }
  testFootContentPairedWilcox=ifelse(any(grepl("V=",tableOut[,"Test Statistic"])),"Wilcoxon Signed Rank Test for continuous variable; ","")
  testFootContentNonPairedWilcox=ifelse(any(grepl("W=",tableOut[,"Test Statistic"])),"Non-Paired Wilcoxon Rank Sum Test for continuous variable; ","")
  if (testFootContentMcNemar!="" | testFootContentChisq!="" | testFootContentPairedWilcox!="" | testFootContentNonPairedWilcox!="") {
    testFootContentAll=paste0("
Tests used: ",testFootContentMcNemar,testFootContentChisq,testFootContentPairedWilcox,testFootContentNonPairedWilcox)
  } else {
    testFootContentAll=""
  }
  tfootContent=paste0(summaryFootContent,testFootContentAll)

  htmlTable(tableOut,
            css.cell = 'padding: 0px 10px 0px;',
            #			header =  c("","N",groupVariable,"Test Statistic"),
            header =  colnames(tableOut),
            tfoot=tfootContent
            #tfoot=paste0("a b c (x",plSignLabel,"s). a b c represent the lower quartile a, the median b, and the upper quartile c for continuous variable in different categories. x",plSignLabel,"s represents X",plSignLabel,"SD.
#Tests used: Wilcoxon Signed Rank Test for continuous variable; McNemar's chi-squared test for symmetry of rows and columns in a two-dimensional contingency table.")
  )
}


#' @export
#'
showP<-function(p,digits=3,text="P=",pCut=10^-digits) {
  if (length(text)==1) {
    text=rep(text,length(p))
  }
  if (any(is.na(p))) {
    naInd=which(is.na(p))
    p[naInd]=1
  } else {
    naInd=NULL
  }
  if (any(p<pCut)) {
    plargerInd<-which(p>=pCut)
    pLessInd<-which(p<pCut)
    p[plargerInd]<-round(p[plargerInd],digits)
    p[pLessInd]<-pCut
    text[pLessInd]<-gsub("=","<",text[pLessInd])
    if (any(text[pLessInd]=="")) {
      text[pLessInd][which(text[pLessInd]=="")]<-"<"
    }
  } else {
    p<-round(p,digits)
  }
  if (length(naInd)>0) {
    p[naInd]<-NA
  }
  return(paste0(text,p))
}

#' @export
#'
summaryFun<-function(x,digits=3) {
  plSignLabel=markupSpecs[["html"]]$plminus
  temp1<-round(quantile(x,c(0.25,0.5,0.75),na.rm=TRUE),digits=digits)
  temp2<-round(c(mean(x,na.rm=TRUE),sd(x,na.rm=TRUE)),digits=digits)
  result<-paste0(paste(temp1,collapse=" ")," (",paste(temp2,collapse=plSignLabel),")")
  return(result)
}



#' @export
#'
summaryTableContinus<-function(dataForTable,variables,groupVariable,digital=2,minUnique=5) {
  pDigital=max(c(digital,3))


  tableAll<-NULL
  for (i in 1:length(variables)) {
    dataOneVariable<-dataForTable[,variables[i]]

    dataOneVariableUniqueNum<-length(unique(na.omit(dataOneVariable)))
    dataType<-tail(class(dataOneVariable),1)
    dataForTableOne<-dataForTable

    tableOneOut<-NULL
    if (dataOneVariableUniqueNum<minUnique | dataType=="character" | dataType=="factor") {
      NaInd<-which(is.na(dataOneVariable))
      if (length(NaInd)>0) {
        dataOneVariable<-dataOneVariable[-NaInd]
        dataForTableOne<-dataForTableOne[-NaInd,]
      }
      dataOneVariableCount<-table(dataOneVariable)
      dataOneVariableSummary<-tapply(dataForTableOne[,groupVariable],dataOneVariable,function(x) summaryFun(x))
      #    temp<-wilcox.test(dataForTableOne[,groupVariable]~dataOneVariable)
      if (length(unique(dataOneVariable))>1) {
        temp<-kruskal.test(dataForTableOne[,groupVariable]~as.factor(dataOneVariable))
        dataOneVariableTestResult<-paste0(names(temp$statistic),"=",round(temp$statistic,digital),", ",showP(temp$p.value,pDigital))
        dataOneVariableTestResult<-makeTestNameForTable(dataOneVariableTestResult)
      } else {
        dataOneVariableTestResult=""
      }


      #			paste0(variables[i]," (",sum(dataOneVariableCount),")")
      #			paste(names(dataOneVariableCount)," (",dataOneVariableCount,")",sep="")
      tableOneOut<-cbind(c(paste0('<p align="left"><b>',variables[i],'</b></p>'),paste0(" ",names(dataOneVariableCount)," ")),
                         c(sum(dataOneVariableCount),dataOneVariableCount),
                         c("",dataOneVariableSummary),
                         c(dataOneVariableTestResult,rep("",dataOneVariableUniqueNum))
      )

    } else if (dataType=="numeric" | dataType=="integer") {
      NaInd<-which(is.na(dataOneVariable))
      if (length(NaInd)>0) {
        dataOneVariable<-dataOneVariable[-NaInd]
        dataForTableOne<-dataForTableOne[-NaInd,]
      }
      dataOneVariableCount<-length(dataOneVariable)
      temp<-cor.test(dataForTableOne[,groupVariable],dataOneVariable,method="sp")
      if ("conf.int" %in% names(temp)) {
        dataOneVariableSummary<-paste0(round(temp$estimate,digital),"(",paste0(round(temp$conf.int,digital),collapse="-"),")")
      } else {
        dataOneVariableSummary<-round(c(temp$estimate),digital)
      }
      dataOneVariableTestResult<-paste0(names(temp$statistic),"=",round(temp$statistic,digital),", ",showP(temp$p.value,pDigital))
      tableOneOut<-c(paste0('<p align="left"><b>',variables[i],'</b></p>'),
                     dataOneVariableCount,
                     dataOneVariableSummary,
                     dataOneVariableTestResult
      )

    } else {
      warning(paste0("Variable ",variables[i]," was not included in the table as its class (",dataType,") was not supported."))
    }
    tableAll<-rbind(tableAll,tableOneOut)
  }

  row.names(tableAll)<-NULL
  return(tableAll)
}

#' @export
#'
makeTestNameForTable<-function(x) {
  result<-gsub("Kruskal-Wallis chi-squared","X<sup>2</sup>",x)
  return(result)
}

#' @export
#'
countToPercent<-function(x) {
  #xTotal=sum(x,na.rm=TRUE)
  #xPercent=as.integer(x/xTotal*100)

  x=as.matrix(x)
  xTotal=colSums(x,na.rm=TRUE)
  xPercent=round(t(t(x)/xTotal)*100,0)
  xOUt=paste0(xPercent,"% (",x,")")
  dim(xOUt) <-dim(x)
  return(xOUt)
}

#' @export
#'
printSummaryTableContinus<-function(tableOut,groupVariable="Variable") {
  plSignLabel=markupSpecs[["html"]]$plminus
  htmlTable(tableOut,
            css.cell = 'padding: 0px 10px 0px;',
            header =  c("","N",groupVariable,"Test Statistic"),
            tfoot=paste0("One continuous and one categorical variables: a b c (x",plSignLabel,"s). a b c represent the lower quartile a, the median b, and the upper quartile c for continuous variable in different categories. x",plSignLabel,"s represents X",plSignLabel,"SD.
Two continuous variables: a. a represents the rho statistic of Spearman's correlation analysis.
Tests used: Kruskal-Wallis test for one continuous and one categorical variables; Spearman correlation for two continuous variables.")
  )
}
