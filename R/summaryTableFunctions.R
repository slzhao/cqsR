
## Make/Load Data
#rawData<-matrix(nrow=200,ncol=10,rnorm(200*10))
#row.names(rawData)<-paste0("Sample",1:200)
#colnames(rawData)<-paste0("Feature",1:10)
#rawData<-data.frame(rawData,FeatureYN1=sample(c(rep(0,100),rep(1,100)),200),FeatureYN2=as.character(sample(c(rep(0,100),rep(1,100)),200)),FeatureCategory1=sample(c("F","E"),200,replace = TRUE),FeatureCategory2=sample(c("A","B","C","D"),200,replace = TRUE),stringsAsFactors = TRUE)


#tableOut=summaryPairedTable(rawData,groupCol="FeatureYN1",varCols=c("FeatureYN2"))
#tableOut=summaryPairedTable(rawData,varCols=c("Feature6"),varColsPaired=c("Feature7"))
#tableOut=summaryPairedTable(rawData,groupCol="FeatureYN1",varCols=c("FeatureYN2","Feature6"))
#tableOut=summaryPairedTable(rawData,varCols=c("FeatureYN2","Feature6"),varColsPaired=c("FeatureYN1","Feature7"))
#printSummaryPairedTable(tableOut)

require(htmlTable)
library(Hmisc)
summaryPairedTable<-function(rawDataPairedOrdered,groupCol=NULL,varCols,varColsPaired=NULL,groupColLabel=NULL,nonPairedTest=FALSE) {
  if (is.null(groupCol) & is.null(varColsPaired)) { #at least one of groupCol or varColsPaired should be defined
    stop(pate0("at least one of groupCol or varColsPaired should be defined"))
  }
  if (is.null(varColsPaired)) {
    rawDataPairedOrderedGroup1=rawDataPairedOrdered[which(as.numeric(as.factor(rawDataPairedOrdered[,groupCol]))==1),]
    rawDataPairedOrderedGroup2=rawDataPairedOrdered[which(as.numeric(as.factor(rawDataPairedOrdered[,groupCol]))==2),]
    if (is.null(groupColLabel)) {
      groupColLabel=levels(as.factor(rawDataPairedOrdered[,groupCol]))
    }
  } else {
    rawDataPairedOrderedGroup1=rawDataPairedOrdered[,varCols,drop=FALSE]
    rawDataPairedOrderedGroup2=rawDataPairedOrdered[,varColsPaired,drop=FALSE]
    if (is.null(groupColLabel)) {
      groupColLabel=c("Pre","Post")
    }
  }

  tableAll=NULL
  for (i in 1:length(varCols)) {
    varCol=varCols[i]
    if (!is.null(varColsPaired)) {
      varColPaired=varColsPaired[i]
    } else {
      varColPaired=varCols[i]
    }
    #categorical, mcnemar.test
    if (is.factor(rawDataPairedOrdered[,varCol]) | is.character(rawDataPairedOrdered[,varCol])) {
      dataForTable=data.frame(Pre=rawDataPairedOrderedGroup1[,varCol],Post=rawDataPairedOrderedGroup2[,varColPaired])

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
          for (i in colMissingLevel) {
            matrixForTest<-cbind(matrixForTest,rep(0,length(allLevels)))
            colnames(matrixForTest)[ncol(matrixForTest)]=i
          }
          matrixForTest=matrixForTest[,allLevels]
        }
        if (length(rowMissingLevel)>0) {
          for (i in rowMissingLevel) {
            matrixForTest<-rbind(matrixForTest,rep(0,length(allLevels)))
            row.names(matrixForTest)[nrow(matrixForTest)]=i
          }
          matrixForTest=matrixForTest[allLevels,]
        }

        pValue=mcnemar.test(matrixForTest)$p.value
        statistic=mcnemar.test(matrixForTest)$statistic
        dataOneVariableCount1=rowSums(matrixForTest)
        dataOneVariableCount2=colSums(matrixForTest)
      }

      tableOneOut<-cbind(c(paste0('<p align="left"><b>',varCol,'</b></p>'),paste0(" ",names(dataOneVariableCount1)," ")),
                         c("",rowSums(cbind(dataOneVariableCount1,dataOneVariableCount2))),
                         c(sum(dataOneVariableCount1),dataOneVariableCount1),
                         c(sum(dataOneVariableCount2),dataOneVariableCount2),
                         c(paste0(names(statistic),"=",round(statistic,2),"; ",showP(pValue)),rep("",length(dataOneVariableCount1)))
      )
      tableAll<-rbind(tableAll,tableOneOut)
    } else {#continus, wilcox rank sum
      testResult=wilcox.test(rawDataPairedOrderedGroup1[,varCol],rawDataPairedOrderedGroup2[,varColPaired],paired = TRUE)
      pValue=testResult$p.value
      statistic=testResult$statistic
      dataOneVariableTestResult=paste(paste0(names(statistic),"=",statistic),showP(pValue),collapse="; ")

      temp1=which(!is.na(rawDataPairedOrderedGroup1[,varCol]))
      temp2=which(!is.na(rawDataPairedOrderedGroup2[,varColPaired]))
      dataOneVariableCount=c(length(temp1),length(temp2),length(intersect(temp1,temp2)))
      group1Summary=summaryFun(rawDataPairedOrderedGroup1[,varCol])
      group2Summary=summaryFun(rawDataPairedOrderedGroup2[,varColPaired])

      if (nonPairedTest) {
        testResult=wilcox.test(rawDataPairedOrderedGroup1[,varCol],rawDataPairedOrderedGroup2[,varColPaired],paired = FALSE)
        dataOneVariableTestResult=paste(dataOneVariableTestResult,"; Non-Paired Test ",showP(testResult$p.value))
      }
      if (varCol != varColPaired) {
        varColLabel=paste0(varCol," vs ",varColPaired)
        dataOneVariableCount=paste0(c(paste0(c(varCol,varColPaired),"="),"Both="),dataOneVariableCount,collapse="; ")
      } else {
        varColLabel=varCol
        dataOneVariableCount=paste0(c(paste0(groupColLabel,"="),"Both="),dataOneVariableCount,collapse="; ")
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
  colnames(tableAll)=c("","N",groupColLabel,"Test Statistic")
  return(tableAll)
}

printSummaryPairedTable<-function(tableOut) {
  plSignLabel=markupSpecs[["html"]]$plminus
  summaryFootContent=paste0("a b c (x",plSignLabel,"s). a b c represent the lower quartile a, the median b, and the upper quartile c for continuous variable in different categories. x",plSignLabel,"s represents X",plSignLabel,"SD.")

  testFootContentMcNemar=ifelse(any(grepl("McNemar",tableOut[,"Test Statistic"])),"McNemar's chi-squared test for symmetry of rows and columns in a two-dimensional contingency table;","")
  testFootContentPairedWilcox=ifelse(any(grepl("V=",tableOut[,"Test Statistic"])),"Wilcoxon Signed Rank Test for continuous variable; ","")
  testFootContentNonPairedWilcox=ifelse(any(grepl("Non-Paired",tableOut[,"Test Statistic"])),"Non-Paired Wilcoxon Rank Sum Test for continuous variable; ","")
  if (testFootContentMcNemar!="" | testFootContentPairedWilcox!="" | testFootContentNonPairedWilcox!="") {
    testFootContentAll=paste0("
Tests used: ",testFootContentMcNemar,testFootContentPairedWilcox,testFootContentNonPairedWilcox)
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

summaryFun<-function(x) {
  plSignLabel=markupSpecs[["html"]]$plminus
  temp1<-round(quantile(x,c(0.25,0.5,0.75),na.rm=TRUE),2)
  temp2<-round(c(mean(x,na.rm=TRUE),sd(x,na.rm=TRUE)),2)
  result<-paste0(paste(temp1,collapse=" ")," (",paste(temp2,collapse=plSignLabel),")")
  return(result)
}


