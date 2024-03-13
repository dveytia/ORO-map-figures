## Function to map oro_type to branch
branchMapFn <- function(x){
  if(grepl("renewable",x, ignore.case=TRUE) | 
     grepl("CO2",x, ignore.case=TRUE) | 
     grepl("efficiency",x, ignore.case=TRUE)){
    return("Mitigation")
  }else if(grepl("Conservation",x, ignore.case=TRUE) | 
           grepl("evolution",x, ignore.case=TRUE)){
    return("Natural resilience")
  }else if(grepl("infrastructure",x, ignore.case=TRUE) | 
           grepl("institution",x, ignore.case=TRUE)){
    return("Societal adaptation")
  }else{
    return(NA)
  }
}