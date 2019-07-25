#' @export
#'
matchGrToVcfObj=function(gr,vcfYield,perfectMatch=FALSE,extracVcfCols=NULL,extracVcfInfos=NULL,includingVcfPos=TRUE) {
  vcfOutTable=fixed(vcfYield)
  if (!is.null(extracVcfCols)) {
    vcfOutTable=cbind(mcols(vcfYield)[,extracVcfCols,drop=FALSE])
  }
  if (!is.null(extracVcfInfos)) {
    vcfInfosTable=info(vcfYield)[,extracVcfInfos,drop=FALSE]
    vcfOutTable=cbind(vcfOutTable,vcfInfosTable)
  }
  if (includingVcfPos) {
#    vcfPosTable=data.table(chr=as.character(seqnames(vcfYield)),start=start((vcfYield)),end=end((vcfYield)),Ref=as.character(fixed(vcfYield)$REF),Alt=as.character(fixed(vcfYield)$ALT))
    vcfPosTable=data.table(chr=as.character(seqnames(vcfYield)),start=start((vcfYield)),end=end((vcfYield)))
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

#' @export
#'
readVcfAndMatchGr=function(vcfFile,variantsTableGRange,variantsTableInVcfAnnotation=NULL,genomeVersion="hg19",extracVcfInfos="AF") {
  #  vcfFile="/scratch/cqs/references/TOPMed/bravo-dbsnp-all.vcf.gz"
  extracVcfInfos=intersect(row.names(info(header(vcf))),extracVcfInfos)

  readVcfParam <- ScanVcfParam(info=extracVcfInfos, geno=NA,fixed=c("ALT","FILTER"))

  if (is.null(variantsTableInVcfAnnotation)) {
    variantsTableInVcfAnnotation=data.table(matrix("",ncol=6+length(extracVcfInfos),nrow=length(variantsTableGRange)))
    colnames(variantsTableInVcfAnnotation)=c("chr","start","end","REF","ALT","FILTER",extracVcfInfos)
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

    grInVcf=matchGrToVcfObj(variantsTableGRange,vcfYield,perfectMatch=FALSE,extracVcfInfos=extracVcfInfos,includingVcfPos=TRUE)
    if (length(grInVcf)>0) {
      #grInVcfOut=sapply(grInVcf,function(x) if (any(x[,6]!="PASS")) {unlist(x[which(x[,6]!="PASS")[1],])} else {unlist(as.data.frame(x[which.max(unlist(x$AF)),]))})
      grInVcfOut=sapply(grInVcf,function(x) if (any(x[,"FILTER"]!="PASS")) {unlist(x[which(x[,"FILTER"]!="PASS")[1],])} else {unlist(as.data.frame(x[1,]))})
      grInVcfOut=data.table(t(grInVcfOut))
      row.names(grInVcfOut)=names(grInVcf)
      #print(names(grInVcf))
      #variantsTableInVcfAnnotation[as.integer(row.names(grInVcfOut)),]=grInVcfOut

      set(variantsTableInVcfAnnotation,as.integer(row.names(grInVcfOut)),names(variantsTableInVcfAnnotation),grInVcfOut)
    }
  }

  close(tab)

  return(variantsTableInVcfAnnotation)


}
