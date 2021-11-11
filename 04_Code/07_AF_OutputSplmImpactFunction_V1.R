# Author: M.L.

# output: output_SPML_model_impacts

# output_SPML_model_impacts: "function()" the function to output spml model impacts

# end

library(tidyverse)
library(dplyr)

# the function to output spml model impacts.
output_SPML_model_impacts <- function(impact_summary, variable_name){
  col1 <- variable_name
  col2 <- impact_summary$res$direct %>% round(6) %>% as.character()
  col3 <- impact_summary$res$indirect %>% round(6) %>% as.character()
  col4 <- impact_summary$res$total %>% round(6) %>% as.character()
  col5 <- impact_summary$pzmat[,1] %>% as.vector()
  col6 <- impact_summary$pzmat[,2] %>% as.vector()
  col7 <- impact_summary$pzmat[,3] %>% as.vector()
  
  output <- cbind(col1, col2, col3, col4, col5, col6, col7) %>% as.data.frame()
  output <- output %>%
    mutate(
      col2 = ifelse(col5 < 0.01, paste0(col2, "*"), col2),
      col2 = ifelse(col5 < 0.05, paste0(col2, "*"), col2),
      col2 = ifelse(col5 < 0.1, paste0(col2, "*"), col2),
      col3 = ifelse(col6 < 0.01, paste0(col3, "*"), col3),
      col3 = ifelse(col6 < 0.05, paste0(col3, "*"), col3),
      col3 = ifelse(col6 < 0.1, paste0(col3, "*"), col3),
      col4 = ifelse(col7 < 0.01, paste0(col4, "*"), col4),
      col4 = ifelse(col7 < 0.05, paste0(col4, "*"), col4),
      col4 = ifelse(col7 < 0.1, paste0(col4, "*"), col4)
    )
  output <- output[,1:4]
  
  output$num <- c(1:nrow(output)) 
  output <- rbind(output, output)
  output <- output %>% arrange(num)
  
  col1 <- c("", "", "","", "", "","")
  col2 <- paste0("(",impact_summary$semat[,1] %>% as.vector() %>% round(6) %>% as.character(),")")
  col3 <- paste0("(",impact_summary$semat[,2] %>% as.vector() %>% round(6) %>% as.character(),")")
  col4 <- paste0("(",impact_summary$semat[,3] %>% as.vector() %>% round(6) %>% as.character(),")")
  
  output.se <- cbind(col1, col2, col3, col4) %>% as.data.frame()
  output.se$num <- c(1:nrow(output.se)) 
  output[seq(2,nrow(output),2),] <- output.se 
  output <- output %>% dplyr::select(-num)
  colnames(output) <- c("X", "Direct Impacts", "Indirect Impacts", "Total Impacts")
  output <- as.data.frame(output)
  return(output)
}