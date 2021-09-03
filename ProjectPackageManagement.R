#function for checking necessary R Packages, installing if needed, and running the libraries
#this is intended to clean up some of the multiple scripts and create better dependency management

PackageDependency <- function() {
  
  packages = readLines("PackagesList")

  installed_packages = packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  invisible(lapply(packages, library, character.only = TRUE))
}

#function for adding packages to the dependency list on the fly

AddPackage = function(PackageToAdd) {
  write(PackageToAdd, file = "PackagesList",append = TRUE)
}