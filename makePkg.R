require(roxygen2)

getwd()
setwd("../")

roxygenise(package.dir="EduRPkg")

## system("R CMD check wzRfun")
## system("R CMD build wzRfun")
