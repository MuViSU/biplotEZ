setwd("C:/Users/29827094/Documents/GitHub/biplotEZ")
devtools::document()
#?biplotEZ
# 1. Recompile Rcpp bindings from src/
Rcpp::compileAttributes()


# 3. Check for any issues
devtools::check()
#install.packages(c("geometry", "rgl", "R.devices"))
