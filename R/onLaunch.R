.onAttach <- function(libname, pkgname){


  if(interactive()){

    #### The following part should be used, if no source, but binary files are provided.
    #### Additionally, all (apart from 'shiny') packages should be removed from import in DESCRIPTION and NAMESPACE (runScreening.R)
    ## Note: code is untested, but should work
    ## Note2: It still needs to be tested if all dependencies are installed

    # requiredPackages <- c("shiny","plyr","xlsx","tm","SnowballC","wordcloud","e1071","kernlab" ,"rpart", "rpart.plot","nnet" ,"caret" ,"RWeka","topicmodels","pROC","DMwR","DT","randomForest", "hmeasure","pdftools","tabulizer")
    # installedPackages <- installed.packages()
    # # Note if there is a version dependency as well, the info installedPackages[,"Version"] can be used to check this!
    # install_index <- which(!(requiredPackages %in% installedPackages[,"Package"]))
    # if(length(install_index)>0){
    #   message(paste0("Installing missing packages: ",paste0(requiredPackages[install_index],collapse=", ")))
    #   # setRepositories(ind=1:5) # ONLY ENABLE THIS IF SOME PACKAGES ALSO COME FROM BIOCONDUCTOR
    #   install.packages(requiredPackages[install_index])
    # }

     # if(!("caretEnsemble" %in% installedPackages[,"Package"])){
     #   message("caretEnsemble not available, installing Development Version from GitHub")
     #   devtools::install_github('zachmayer/caretEnsemble')
     # }
     #
     # if(!("tabulizer" %in% installedPackages[,"Package"])){
     #   message("tabulizer not available, installing Development Version from GitHub")
     #   devtools::install_github(c("ropenscilabs/tabulizerjars", "ropenscilabs/tabulizer"))
     # }

    runAbstractScreening()
  }else{
    return()
  }
}
