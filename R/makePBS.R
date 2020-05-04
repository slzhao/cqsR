#' @export
#'
makePBS=function(
  taskDir,
  command,
  sampleName,
  email="shilin.zhao@vanderbilt.edu",emailType="FAIL",
  nodes=1,ntasks=1,ncpus=1,
  time="24:00:00",mem="20G",
  logFile=paste0(taskDir,"/log/",sampleName,".log")
  ) {

  pbsDir=paste0(taskDir,"/pbs")
  logDir=paste0(taskDir,"/log")
  resultDir=paste0(taskDir,"/result")

  logFile=paste0(logDir,"/",sampleName,".log")
  pbsFile=paste0(pbsDir,"/",sampleName,".pbs")



  if (!file.exists(taskDir)) {
    dir.create(taskDir)
  }
  if (!file.exists(pbsDir)) {
    dir.create(pbsDir)
  }
  if (!file.exists(logDir)) {
    dir.create(logDir)
  }
  if (!file.exists(resultDir)) {
    dir.create(resultDir)
  }

    pbsHeader=paste0(
"#!/bin/bash
#SBATCH --mail-user=",email,"
#SBATCH --mail-type=",emailType,"
#SBATCH --nodes=",nodes,"
#SBATCH --ntasks=",ntasks,"
#SBATCH --cpus-per-task=",ncpus,"
#SBATCH --time=",time,"
#SBATCH --mem=",mem,"
#SBATCH --constraint=haswell

#SBATCH -o ",logFile)

    pbsCommand=paste0("
cd ",resultDir,"
",command,"
"
)

    writeLines(paste0(
pbsHeader,"
",pbsCommand),
               con=pbsFile,)
}

