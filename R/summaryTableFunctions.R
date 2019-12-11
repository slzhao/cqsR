
# Make/Load Data

rawData<-matrix(nrow=200,ncol=10,rnorm(200*10))
row.names(rawData)<-paste0("Sample",1:200)
colnames(rawData)<-paste0("Feature",1:10)
rawData<-data.frame(rawData,FeatureYN1=sample(c(rep(0,100),rep(1,100)),200),FeatureYN2=as.character(sample(c(rep(0,100),rep(1,100)),200)),FeatureCategory1=sample(c("F","E"),200,replace = TRUE),FeatureCategory2=sample(c("A","B","C","D"),200,replace = TRUE),stringsAsFactors = TRUE)


tableOut=summaryPairedTable(rawData,groupCol="FeatureYN1",varCols=c("FeatureYN2"))
printSummaryPairedTable(tableOut)

require(htmlTable)
summaryPairedTable<-function(rawDataPairedOrdered,groupCol,varCols) {
  rawDataPairedOrderedGroup1=rawDataPairedOrdered[which(as.numeric(as.factor(rawDataPairedOrdered[,groupCol]))==1),]
  rawDataPairedOrderedGroup2=rawDataPairedOrdered[which(as.numeric(as.factor(rawDataPairedOrdered[,groupCol]))==2),]
  groupColLabel=levels(as.factor(rawDataPairedOrdered[,groupCol]))

  tableAll=NULL
  for (varCol in varCols) {
    #categorical, mcnemar.test
    if (is.factor(rawDataPairedOrdered[,varCol]) | is.character(rawDataPairedOrdered[,varCol])) {
      dataForTable=data.frame(Pre=rawDataPairedOrderedGroup1[,varCol],Post=rawDataPairedOrderedGroup2[,varCol])

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
                         c(sum(dataOneVariableCount1),dataOneVariableCount1),
                         c(sum(dataOneVariableCount2),dataOneVariableCount2),
                         c(paste0(names(statistic),"=",round(statistic,2),"; ",showP(pValue)),rep("",length(dataOneVariableCount1)))
      )
      tableAll<-rbind(tableAll,tableOneOut)
    } else {#continus, wilcox rank sum


    }

  }
  row.names(tableAll)<-NULL
  colnames(tableAll)=c("",groupColLabel,"Test Statistic")
  return(tableAll)
}

printSummaryPairedTable<-function(tableOut) {
  htmlTable(tableOut,
            css.cell = 'padding: 0px 10px 0px;',
            #			header =  c("","N",groupVariable,"Test Statistic"),
            header =  colnames(tableOut),
            tfoot="Tests used: McNemar's chi-squared test for symmetry of rows and columns in a two-dimensional contingency table."
  )
}



showP<-function(p,digits=2,text="P=",pCut=10^-digits) {
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


