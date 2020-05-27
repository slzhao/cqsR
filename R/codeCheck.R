###################################################
#check pair of { and } in code
#NOT FINISHED, need make into a function
###################################################

#
# codeFile="/home/zhaos/source/perl_cqs/test/cromwell/cromwell.examples.local.simg.conf"
# codeFile="/home/zhaos/source/perl_cqs/test/cromwell/cromwell.examples.slurm.simg.conf"
#
# codeContent=readLines(codeFile)
#
# #parameters
# commentLabel="#"
# otherSkipLabel="\\$\\{"
# pair1="\\{"
# pair2="\\}"
#
# #codeContent=codeContent[1:32]
#
# pair1LineAll=NULL
# pair1LabelAll=NULL
# pairedLabelAll=NULL
# for (i in 1:length(codeContent)) {
#   if (grepl(commentLabel,codeContent[i])) {
#     pairLabel=codeContent[i]
#     next;
#   } #skip comment
#   if (grepl(otherSkipLabel,codeContent[i])) {
#     next;
#   }
#   if (grepl("^\\s?$",codeContent[i])) {next;} #skip empty
#   if (grepl(pair1,codeContent[i])) {
#     pair1LineAll=c(pair1LineAll,i)
#     lineContent=gsub(pair1,"",codeContent[i])
#     lineContent=gsub("\\s+","",lineContent)
#     if (lineContent!="") {
#       pairLabel=lineContent
#     }
#     pair1LabelAll=c(pair1LabelAll,pairLabel)
#   } else if (grepl(pair2,codeContent[i])) {
#     pair1LinePaired=pair1LineAll[length(pair1LineAll)]
#     pair1LineAll=pair1LineAll[-length(pair1LineAll)]
#     pair1LabelPaired=pair1LabelAll[length(pair1LabelAll)]
#     pair1LabelAll=pair1LabelAll[-length(pair1LabelAll)]
#     pairedLabelAll=rbind(pairedLabelAll,c(pair1LabelPaired,pair1LinePaired,i))
#   } else {
#     pairLabel=codeContent[i]
#   }
# }


