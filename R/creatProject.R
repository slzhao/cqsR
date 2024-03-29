#source('D:/source/cqsR/R/creatProject.R')
#creatProject("Projects/20190504_XiangmingTrend")
#creatProject("ChevisShannon/20190509_AntenatalHydronephrosisANH")
#creatProject("test/test1")
#creatProject("Pierre","targetSequencingUploadFile")

#' @export
#'
creatProject <- function(pi,project,note="",projectDate=gsub("-","",Sys.Date()),
                         sourcePDir=paste0(Sys.getenv("SOURCEDIR","d:/source/"),"/r_cqs"),
                         dataPDir=Sys.getenv("WORKDIR","D:/workSync"),
                         templateFile="D:/source/r_cqs/myPkg/example/reportExample.Rmd") {
  projectName=paste0(pi,"/",projectDate,"_",project)
  sourceDir=paste0(sourcePDir,"/",projectName)
  dataDir=paste0(dataPDir,"/",projectName)
  rmarkdownFile=file.path(paste0(sourceDir,"/R/",basename(projectName), ".Rmd"))

  # #create_project will report project under project, use testthat trick to skip it
  # #https://stackoverflow.com/questions/53819291/how-to-create-an-r-package-nested-in-git-directory
  # #usethis::create_project(sourceDir,open =FALSE)
  # testthat::with_mock(
  #   check_not_nested = function(path, name) return(),
  #   usethis::create_project(sourceDir,open =FALSE),
  #   .env = "usethis"
  # )
  usethis::create_project(sourceDir,open =FALSE)

  dir.create(dataDir)
  file.symlink(dataDir,paste0(sourceDir,"/","data"))

  #Copy templateFile file to project folder
  #Didn't use use_template in create_project as want to keep the templateFile run-able
  if (!file.exists(templateFile)) {
    warning(paste0(templateFile," doesn't exists. Can't copy it to project folder!"))
  }
  templateFileContent=readLines(templateFile)
  templateFileContent[2]=paste0('title: "',basename(projectName),'"') #Change report title in markdown file
  #Change report working folder in markdown file
  templateFileContent=gsub("##PreDefinedForChangingWorkingFolder##",paste0('setwd("',dataDir,'")'),templateFileContent)

  if (file.exists(rmarkdownFile)) {
    warning(paste0(rmarkdownFile," exists. Can't replace it!"))
  } else {
    cat(paste(templateFileContent, collapse="\n"), file=rmarkdownFile)
  }

#  message(paste("Project ",basename(projectName), "has been created at ",sourceDir))
  writeWorkList(pi,project,note,projectDate,workListFile=paste0(sourcePDir,"/workList.txt"))
  usethis::proj_activate(sourceDir)
}

#' @export
#'
writeWorkList=function(pi,project,note=note,projectDate=gsub("-","",Sys.Date()),workListFile="d:/source/r_cqs/workList.txt") {
  workListContent=readr::read_tsv(workListFile)
  workListContentThisProject=workListContent %>% filter(PI==pi,Project==project,Date==projectDate)
  if (nrow(workListContentThisProject)>0) {
    projectName=paste0(pi,"/",projectDate,"_",project)
    warning(paste0("Project ",projectName," alreaday in workListFile. Not changed."))
  } else {
    workListContent=rbind(workListContent,c(pi,project,projectDate,note,""))
    write.table(workListContent,workListFile,sep = "\t",quote =FALSE,row.names = FALSE)
  }
}

#' @export
#'
showWorkList=function(workListFile=paste0(Sys.getenv("SOURCEDIR","d:/source/"),"/r_cqs/workList.txt"),
                      showFinished=FALSE) {
  workListContent=readr::read_tsv(workListFile)
  if (!showFinished) {
    finishedWorkInd=which(workListContent$Finished=="Finished")
    if (length(finishedWorkInd)>0) {
      workListContent=workListContent[-finishedWorkInd,]
    }
  }
  knitr::kable(workListContent)
}

#' @export
#'
editWorkList=function(workListFile=paste0(Sys.getenv("SOURCEDIR","d:/source/"),"/r_cqs/workList.txt")) {
#  require(editData)
  workListContent<<-readr::read_tsv(workListFile)
  workListContent <- editData::editData(data=workListContent)
  if (!is.null(workListContent)) {
    write.table(workListContent,workListFile,sep = "\t",quote =FALSE,row.names = FALSE)
  } else {
#    warning("Not able to change/save workListFile")
  }
}

#' @export
#'
copyTemplateFile <- function(project,projectDate=gsub("-","",Sys.Date()),
                         sourceDir=getwd(),
                         templateFile="D:/source/r_cqs/myPkg/example/reportExample.Rmd") {
  projectName=paste0(projectDate,"_",project)
  rmarkdownFile=file.path(sourceDir,paste0(basename(projectName), ".Rmd"))

  #Copy templateFile file to project folder
  if (!file.exists(templateFile)) {
    warning(paste0(templateFile," doesn't exists. Can't copy it to project folder!"))
  }
  templateFileContent=readLines(templateFile)
  templateFileContent[2]=paste0('title: "',basename(projectName),'"') #Change report title in markdown file

  if (file.exists(rmarkdownFile)) {
    warning(paste0(rmarkdownFile," exists. Can't replace it!"))
  } else {
    cat(paste(templateFileContent, collapse="\n"), file=rmarkdownFile)
  }
}
