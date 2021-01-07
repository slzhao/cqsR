
#https://cran.r-project.org/web/packages/qicharts2/vignettes/qicharts2.html

spcChartDetection=function(x,rules=c("A1","A2")) {
  n=length(x)
  if ("A1" %in% rules) {
    minN=round(log2(n)+3)
    labelSign1=x>mean(x)
    labelSign2=x<mean(x)

    ruleDetectedInd=NULL
    for (i in minN:length(labelSign)) {
      if (all(labelSign1[(i-minN+1):i]) | all(labelSign2[(i-minN+1):i])) {
        ruleDetectedInd=c(ruleDetectedInd,i)
      }
    }
  }

  if ("A2" %in% rules) {
    minN=qbinom(p = 0.05, size = n - 1, prob = 0.5)



  }

}
