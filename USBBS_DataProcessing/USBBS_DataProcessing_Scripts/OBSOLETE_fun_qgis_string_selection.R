###################################################################################################################
#'
#' **Function to make a string including multiple vector characters for selection in QGIS**
#' 

qgis_string_selection <- function(selec.vec=0) {

selec.vec <- as.character(selec.vec)

temp.sq.string <-""
for (sq in selec.vec) {
  
  temp.sq.string <- paste(temp.sq.string, "OR", " ", '"RTENAME"', " " , "=", " ", "'",sq, "'", " ", collapse = " ", sep="")
  
}

temp.sq.string

return(cat(temp.sq.string, "\n"))

}