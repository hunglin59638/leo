#!/usr/bin/env Rscript
cmdArgs <- commandArgs(trailingOnly = FALSE)
root_dir <- normalizePath(dirname((sub("^--file=", "", cmdArgs[grep("^--file=", cmdArgs)]))))[1]
#root_dir <- dirname(sys.frame(1)$ofile)

if (Sys.info()['sysname'] == "Linux") {
  Sys.setenv(PKG_CONFIG_PATH="/usr/lib/x86_64-linux-gnu/pkgconfig")
  cat("Change /usr/lib/R to be writable")
  system("sudo -kS chmod -R 777 /usr/lib/R/",
         input=NULL)
  # readline("Enter your password: ")
  cat("Installing the packages")
  devtoos_pkgs <- c("build-essential", "libssl-dev", "curl", "libssl-dev", "libxml2-dev", "libxml2-dev")
  ufs_pkgs <- c("libfontconfig1-dev")
  others <- c("libssl-dev")
  cmd <- c("sudo -kS apt-get install" ,devtoos_pkgs, ufs_pkgs, others, "-y")
  cmd <- paste(cmd, collapse=" ")
  returncode <- system(cmd)
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

bioman_pkgs <- c("AnnotationHub", "BiocGenerics", "parallel", "clusterProfiler",
                 "fgsea","DOSE")
for (p in bioman_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {cat("Installing", p, "..."); BiocManager::install(p)}
}

cat("Finished !")
