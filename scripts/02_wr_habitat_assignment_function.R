# HABITAT ASSIGNMENT FUNCTION

wr_habitat_assigner <- function(x) {
  
  # 1. Set habitat isotopic ranges in function argument so if we want to we can change default

  LAS_max = 0.70467 # min measured upper Sac water value (after Phillis et al 2018)
  SAC_max = 0.7061  # after Phillis et al based on mean value at Freeport 
  DEL_max = 0.70785 # mean Chipps value (after Phillis et al 2018) 
  
  # 2. Assign a habitat to each spot based on measured Sr8786   
  
 library(dplyr)
  
  x <- x %>% mutate(Isotopic_range = case_when(
    
      Sr8786_norm + SE2 < LAS_max & Escap_yr > 2009 ~ 'LAS', # In later years higher quality data (smaller SE) AND Sac mainstem 
      #value seems lower so making our threshold a bit lower (more conservative towards *not* assigning a fish to LAS)
      
      Sr8786_norm < LAS_max & Escap_yr <= 2009 ~ 'LAS',
      
      Sr8786_norm + SE2 >= LAS_max & Sr8786_norm < SAC_max & Escap_yr > 2009 ~ 'SAC', # In later years higher quality data (smaller SE) AND Sac mainstem 
      #value seems lower so making our threshold a bit lower (more conservative towards *not* assigning a fish to LAS)
      
      Sr8786_norm >= LAS_max & Sr8786_norm < SAC_max & Escap_yr <= 2009 ~ 'SAC',
      
      Sr8786_norm >= SAC_max & Sr8786_norm <= DEL_max ~ 'DEL',
      
      Sr8786_norm > DEL_max ~ 'AME')) %>%
      
    filter(Distance_um >= Exog_dist & Distance_um <= FWExit_dist) # cut out "mom" and ocean spots
    
  
# 3. ID SAC-DEL-AME or AME-DEL-SAC sequences and replace DEL in these cases with AME. 
  
x$Habitat <- x$Isotopic_range # Make a back up so we can see where we've modified spots

which_DEL <- which(x$Habitat=="DEL") # ID Delta-assigned spots in sequence
which_DEL <- which_DEL[!which_DEL %in% c(1, length(x$Habitat))] #Delete cases of DEL if first or last spot or breaks loop below

if(length(which_DEL)==0){
  
  return(x)
  
}

for(i in which_DEL){ #need to do a loop in case multiple DEL spots in a single profile..

    if(x$Habitat[i - 1] =="SAC" & x$Habitat[i + 1] =="AME"){ # Find instances where preceding spot = SAC and proceding spot = AME
      x$Habitat[i] <- "AME" # replace with AME (given that still enough material to deviate from Sac isotopic range count it as non-natal)
    }
  
  if(x$Habitat[i - 1] =="AME" & x$Habitat[i + 1] =="SAC"){ # Find instances where preceding spot = AME and proceding spot = SAC
    x$Habitat[i] <- "AME" # replace with AME (given that still enough material to deviate from Sac isotopic range count it as non-natal)
  }
  
}
  
 return(x)

}