#!/usr/bin/env Rscript
cmdArgs <- commandArgs(trailingOnly = FALSE)
root_dir <- normalizePath(dirname((sub("^--file=", "", cmdArgs[grep("^--file=", cmdArgs)]))))[1]
#root_dir <- dirname(sys.frame(1)$ofile)
sys_info <- Sys.info()
os_type <- sys_info["sysname"]
machine <- sys_info["machine"]
user <- sys_info["user"]
if (os_type == "Linux") {
  Sys.setenv(PKG_CONFIG_PATH=paste0("/usr/lib/", machine, "-gnu/pkgconfig"))
  passwd_f = paste0(root_dir,"/passwd")
  passwd <- readChar(passwd_f, nchars = file.info(passwd_f)$size)
  if (!nchar(passwd)) {
    passwd <- NULL
  }
  if (user != "root") {
    cat("Change /usr/lib/R to be writable")
    system("sudo -S chmod -R 777 /usr/lib/R/",
           input=passwd)
    system("sudo -S chmod -R 777 /usr/share/R/doc/html",
           input=passwd)
  }
  cat("Installing the packages")
  devtoos_pkgs <- c("build-essential", "libssl-dev", "curl", "libssl-dev", "libxml2-dev", "libxml2-dev")
  ufs_pkgs <- c("libfontconfig1-dev")
  others <- c("libssl-dev")
  cmd <- c("sudo -S", "apt-get install" , devtoos_pkgs, ufs_pkgs, others, "-y")
  if (user == "root") {
    cmd <- cmd[2:length(cmd)]
  }
  cmd <- paste(cmd, collapse=" ")
  returncode <- system(cmd, input=passwd)
} else {returncode <- 0}

if (returncode) {
  cat("Fail to install packages"); quit()
}
pkgs <- read.csv(file=file.path(root_dir,"requirements.txt"), header = F)[,1]
for (i in 1:length(pkgs)) {
  p <- pkgs[i]
  
  if (!requireNamespace(p, quietly = TRUE)) {install.packages(p, repos = "https://cloud.r-project.org",
                                                              dependencies = TRUE )}
}

bioman_pkgs <- c("AnnotationHub", "DOSE", "BiocGenerics", "clusterProfiler","fgsea")
for (p in bioman_pkgs) {
  if (!requireNamespace(p, quietly=TRUE)) {cat("Installing", p, "..."); 
    BiocManager::install(p, ask=FALSE, site_repository="https://bioconductor.org/packages/3.14/bioc", 
                         force=TRUE)}
}

cat("Finished !\n")
