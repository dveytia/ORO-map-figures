
getTiles <- function(
    df, # input dataframe
    filterVar, # which variable to subset data by
    filterVal, # the value of the variable to filter to
    orderVars, # A vector of variables on the x and y axis to sort by
    tileLoc = c("bottomright","topleft") # Which triangle location you want to plot
    ){
  
  require(dplyr)
  
  if(tileLoc == "bottomright"){xTrans=-0.5+c(0,1,1); yTrans=-0.5+c(0,0,1)}
  if(tileLoc == "topleft"){xTrans=-0.5+c(0,1,0); yTrans = -0.5+c(0,1,1)}
  
  # function to get the points of a triangle from the transformations
  getPoints <- function(vector, trans){
    for(i in 1:length(vector)){
      temp <- vector[i]+trans
      if(i==1){
        out <- temp
      }else{
        out <- c(out, temp)
      }
    }
    return(out)
  }
  
  # Filter dataset based on chosen Variable x Value
  df = df[which(df[,filterVar] == filterVal),]
  
  # Create group names for polygon plotting
  df <- df %>%
    #arrange(oro_type, ecosystem_type)%>%
    arrange(across(matches(orderVars["y"])), across(matches(orderVars["x"]))) %>%
    mutate(id = paste0(filterVal, row_number())) #%>% select(oro_type, ecosystem_type, id)
  
  # Get x an y positions for each group, and triple them 
  x <- as.numeric(df[,orderVars["x"]])
  y <- as.numeric(df[,orderVars["y"]])
  df <- rbind(df,df,df) 
  
  #df <- df %>% arrange(oro_type, ecosystem_type) %>% head
  
  # Use get points function to transform these triplicates into points of the triangle
  df <- df %>% arrange(across(matches(orderVars["y"])), across(matches(orderVars["x"]))) 
  df$x <- getPoints(x, xTrans)
  df$y <- getPoints(y, yTrans)
  
  return(df)
}