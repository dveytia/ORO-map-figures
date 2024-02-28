
## mCDR research in France March 2024
## KEYWORD EXTRACTION SCRIPT FOR ROSSI

## Load libraries and functions
library(dplyr)
source(here::here("utils.R")) 
source(here::here("clean_string.R")) 
source(here::here("bool_detect.R")) 

## Load data
load(here::here("data/mCDR_data_March2024_beforeKeyword.RData"))

## Search queries
oae_qry = "OAE OR 'alkalin* enhanc*' OR liming OR 'enhanc* weathering' OR olivine OR (electrochem* AND (remov* OR stor*)) OR 'bubble strip*' OR 'water split*'"
oif_qry = "'iron fertil*'"
au_qry = "artificial AND (upwell* OR downwell*)"
deep_qry = "(deep AND (sea OR seabed OR ocean) AND (stor* OR sequest*)) OR (biomass AND sink*)"
cultivation_qry = "(microalg* OR macroal* OR seaweed* OR kelp) AND (farm* OR cultivat*)"


# to test -- comment out when I actually run the script
# mCDR_df_top10 <- mCDR_df_top10[1:5,]

## Parallelize keyword matching
results = parallel::mclapply(1:nrow(mCDR_df_top10), function(i){
  
  # Clean text
  text <- clean_string(paste(mCDR_df_top10$title[i], 
                             mCDR_df_top10$abstract[i], 
                             mCDR_df_top10$keywords[i], 
                             collapse = " "))
  
  # match for different queries
  oae_match <- bool_detect2(text, oae_qry)
  oif_match <- bool_detect2(text, oif_qry)
  au_match <- bool_detect2(text, au_qry)
  deep_match <- bool_detect2(text, deep_qry)
  cult_match <- bool_detect2(text, cultivation_qry)
  
  ## Bind both together into dataframe to return
  matches <- data.frame(
    analysis_id = mCDR_df_top10$analysis_id,
    OAE = oae_match,
    OIF = oif_match,
    Artificial_upwelling = au_match,
    Deep_sea_storage = deep_match,
    Algal_cultivation = cult_match
  )
  return(matches)

  
}, mc.cores = 10) ## End of mclapply


## Bind results together
matches_all <- do.call(rbind.data.frame, results)

## Save
save(matches_all, file = "mCDR_data_March2024_afterKeyword.RData")




