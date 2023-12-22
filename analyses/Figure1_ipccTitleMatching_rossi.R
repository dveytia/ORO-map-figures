## FIGURE 1: sub-analysis title matching between IPCC and database ##
# Code written to run on rossi server

## Set up
# Source functions for analysis
source("utils.R")
source("clean_string.R")

# Load data files
load("IPCC_AR6_uniquerefs.RData")
load("oro_uniquerefs.RData")
uniquerefs <- uniquerefs[,c("analysis_id","duplicate_id","year","title","doi")]

# subset the ipcc to the temporal range of our database, including NA years
# identify common years between two databases
my_years <- sort(unique(as.numeric(uniquerefs$year))) 
ipcc_years <- sort(unique(as.numeric(ipccBibUnique$year)))
years <- intersect(my_years, ipcc_years)
years <- c(as.character(years), NA)


## For each year, calculate if there is a match
results = parallel::mclapply(1:length(years), function(y) {
  
  # subset data to that year
  if(is.na(years[y])){
    tempDf <- uniquerefs[is.na(uniquerefs$year),]
    ipccTempDf <- ipccBibUnique[is.na(ipccBibUnique$year),]
  }else{
    tempDf <- uniquerefs[uniquerefs$year == years[y],] 
    ipccTempDf <- ipccBibUnique[ipccBibUnique$year == years[y],]
  }
  
  # first look for doi matches
  tempDf <- merge(tempDf, ipccTempDf[,c("doi","ipccRef_id")],
                  all.x = TRUE, all.y = FALSE, by = "doi")
  
  # if there are still NA values in ipccRef_id column, go to fuzzy title
  if(0 < sum(!is.na(tempDf$title)) & 0 < sum(!is.na(ipccTempDf$title))){ # if both have valid titles
    if(0 < sum(is.na(tempDf$ipccRef_id))){ # if there are still matches to be found
      # match titles using fuzzy matching
      ipccTitles <- clean_string(ipccTempDf$title[!is.na(ipccTempDf$title)])
      titleMatches <- parallel::mclapply(1:length(tempDf$title), function(i){
        myTitle <- tempDf$title[i]
        if(!is.na(myTitle)){
          myMatch <- stringdist::amatch(x = clean_string(myTitle), table = ipccTitles,method = "osa", maxDist = 5)
          if(1 < length(myMatch)){
            myMatch <- myMatch[1]
          }
          return(myMatch)
        }else{
          return(NA)
        }
      }, mc.cores = 4)
      titleMatches <- do.call(c, titleMatches)
      
      # titleMatches <- stringdist::amatch(
      #   x = clean_string(myTitles), table = clean_string(ipccTitles),
      #   method = "osa", maxDist = 5
      # )
      # fill in any NA values from title Matching
      tempDf$ipccRef_id[is.na(tempDf$ipccRef_id)] = titleMatches[is.na(tempDf$ipccRef_id)]
    }
  } 
  
  # return result
  return(tempDf)

}, mc.cores = 4)

ipccMatches <- do.call(rbind.data.frame, results)

save(ipccMatches,file = "IPCC_AR6_uniquerefs_matches.RData")