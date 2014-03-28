library(Metrics)



clean.diagnosus <- function(diagnosis) {
  diagnosus <- diagnosis
  diagnosus <- substr(diagnosus, 2, nchar(diagnosus))
  diagnosus <- gsub("[[:punct:]]", "", diagnosus)
  diagnosus <- gsub(" ", "", diagnosus)

  return(diagnosus)
}



get.diagnosis.group <- function(diagnosis.code, ifICD9, ifICD10, ICD9, ICD10){
  diagnosis.group <- "Others"
  
  if(diagnosis.code != ""){
  
  ## for ICD 9 codes
  if(ifICD9 == 1){
    if(substr(diagnosis.code, 1, 1) == "E"){
      diagnosis.group <- ICD9[19, 3]
    } else if(substr(diagnosis.code, 1, 1) == "V"){
      diagnosis.group <- ICD9[19, 3]
    } else{
      code <- as.numeric(substr(diagnosis.code, 1, 3))
      for(i in 1:18){
        if(code <= ICD9[i, 2]){
          diagnosis.group <- ICD9[i, 3]
          break
        }
      }
    }
  }
  
  ## for ICD 10 codes
  if(ifICD10 == 1){
    for(i in 1:nrow(ICD10)){
      if(substr(diagnosis.code, 1, 1) %in% c(ICD10[i, "StartAlpha"], ICD10[i, "EndAlpha"])){
        if(as.numeric(substr(diagnosis.code, 2, 3)) <= ICD10[i, "EndNum"]){
          diagnosis.group <- ICD10[i, "DIAGNOSISGROUP"]
          break
        }
      }
    }
  }
  
  }
  
  return(diagnosis.group)
}

get.age.group <- function(age){
  return(min(5, ceiling(age/20)))
}
