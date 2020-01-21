#source('D:/source/cqsR/R/creatProject.R')
#creatProject("Projects/20190504_XiangmingTrend")
#creatProject("ChevisShannon/20190509_AntenatalHydronephrosisANH")
#creatProject("test/test1")

creatProject <- function(projectName,
                         sourcePDir="d:/source/r_cqs",
                         dataPDir="D:/OneDriveWork/OneDriveVanderbilt/work",
                         templateFile="D:/source/r_cqs/myPkg/example/reportExample.Rmd") {
  sourceDir=paste0(sourcePDir,"/",projectName)
  dataDir=paste0(dataPDir,"/",projectName)
  rmarkdownFile=file.path(paste0(sourceDir,"/R/",basename(projectName), ".Rmd"))

  #create_project will report project under project, use testthat trick to skip it
  #https://stackoverflow.com/questions/53819291/how-to-create-an-r-package-nested-in-git-directory
  #usethis::create_project(sourceDir,open =FALSE)
  testthat::with_mock(
    check_not_nested = function(path, name) return(),
    usethis::create_project(sourceDir,open =FALSE),
    .env = "usethis"
  )

  dir.create(dataDir)
  file.symlink(dataDir,paste0(sourceDir,"/","data"))

  #Copy templateFile file to project folder
  #Didn't use use_template as want to keep the templateFile run-able
  if (file.exists(rmarkdownFile)) {
    stop(paste0(rmarkdownFile," exists. Can't make project folder and replace it!"))
  }

  if (!file.exists(templateFile)) {
    warning(paste0(templateFile," doesn't exists. Can't copy it to project folder!"))
  }
  templateFileContent=readLines(templateFile)
  templateFileContent[2]=paste0('title: "',basename(projectName),'"') #Change report title in markdown file
  #Change report working folder in markdown file
  templateFileContent=gsub("##PreDefinedForChangingWorkingFolder##",paste0('setwd("',dataDir,'")'),templateFileContent)

  cat(paste(templateFileContent, collapse="\n"), file=rmarkdownFile)
#  message(paste("Project ",basename(projectName), "has been created at ",sourceDir))

  usethis::proj_activate(sourceDir)
}


