library(Metrics)

clean.diagnosus <- function(diagnosis) {
  diagnosus <- diagnosis
  diagnosus <- substr(diagnosus, 2, nchar(diagnosus))
  diagnosus <- gsub("[[:punct:]]", "", diagnosus)
  diagnosus <- gsub(" ", "", diagnosus)

  return(diagnosus)
}
