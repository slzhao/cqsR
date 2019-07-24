
#library(VariantAnnotation)
#library(data.table)

fl <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")
vcf <- readVcf(fl, "hg19")
vcf <- VariantAnnotation::expand(x = vcf, row.names = TRUE)

variantsTable=data.frame(chr="22", start=c(50300078,50300085), end=c(50300078,50300102), score=1:2)
variantsTableGr=makeGRangesFromDataFrame(variantsTable)
#matchGrToVcfObj(variantsTableGr,vcf)
readVcfAndMatchGr(fl,variantsTableGr)


matchGrToVcfObj=function(gr,vcfYield,perfectMatch=FALSE,extracVcfCols=NULL,extracVcfInfos=NULL,includingVcfPos=TRUE) {
  if (is.null(extracVcfCols)) {
    vcfOutTable=mcols(vcfYield)
  } else {
    vcfOutTable=mcols(vcfYield)[,extracVcfCols,drop=FALSE]
  }
  if (!is.null(extracVcfInfos)) {
    vcfInfosTable=info(vcfYield)[,extracVcfInfos,drop=FALSE]

    vcfOutTable=cbind(vcfOutTable,vcfInfosTable)
  }
  if (includingVcfPos) {
    vcfPosTable=data.table(chr=as.character(seqnames(vcfYield)),start=start((vcfYield)),end=end((vcfYield)),Ref=as.character(fixed(vcfYield)$REF),Alt=as.character(fixed(vcfYield)$ALT))

    vcfOutTable=cbind(vcfPosTable,vcfOutTable)
  }

  if (perfectMatch) {
    match(gr,rowRanges(vcfYield))
  } else {
    matchResult=findOverlaps(gr,rowRanges(vcfYield))
  }
  matchedList=tapply(subjectHits(matchResult),queryHits(matchResult),function(x) x)
  matchedListVcfCols=lapply(matchedList,function(x) vcfOutTable[x,])

  return(matchedListVcfCols)
}


readVcfAndMatchGr=function(vcfFile,variantsTableGRange,variantsTableInVcfAnnotation=NULL,genomeVersion="hg19") {
  #  vcfFile="/scratch/cqs/references/TOPMed/bravo-dbsnp-all.vcf.gz"
  readVcfParam <- ScanVcfParam(info="AF", geno=NA,fixed=c("ALT","FILTER"))

  if (is.null(variantsTableInVcfAnnotation)) {
    variantsTableInVcfAnnotation=data.table(matrix("",ncol=7,nrow=length(variantsTableGRange)))
    colnames(variantsTableInVcfAnnotation)=c("chr","start","end","Ref","Alt","FILTER","AF")
  }

  tab <- TabixFile(vcfFile, yieldSize=50000)
  open(tab)
  vcfProgress=0


  while (nrow(vcfYield <- readVcf(tab, genomeVersion, param=readVcfParam))) {
    #vcfYield <- readVcf(tab, "hg38", param=readVcfParam)
    vcfProgress=vcfProgress+length(vcfYield)
    print(paste0("Reading vcf: ", vcfProgress))

    vcfYield <- VariantAnnotation::expand(x = vcfYield, row.names = TRUE)
    #rowRanges(vcfYield)

    grInVcf=matchGrToVcfObj(variantsTableGRange,vcfYield,perfectMatch=FALSE,extracVcfCols="FILTER",extracVcfInfos="AF",includingVcfPos=TRUE)
    if (length(grInVcf)>0) {
      grInVcfOut=sapply(grInVcf,function(x) if (any(x[,6]!="PASS")) {unlist(x[which(x[,6]!="PASS")[1],])} else {unlist(as.data.frame(x[which.max(unlist(x$AF)),]))})
      grInVcfOut=data.table(t(grInVcfOut))
      row.names(grInVcfOut)=names(grInVcf)
      #     print(head(grInVcfOut))
      print(names(grInVcf))
      #variantsTableInVcfAnnotation[as.integer(row.names(grInVcfOut)),]=grInVcfOut

      set(variantsTableInVcfAnnotation,as.integer(row.names(grInVcfOut)),names(variantsTableInVcfAnnotation),grInVcfOut)
    }
  }

  close(tab)

  return(variantsTableInVcfAnnotation)


}
