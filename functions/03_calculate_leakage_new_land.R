
# function to calculate new land brought to production

calc_INL <- function(fp,IS,NL,y){
  
  INL <- (fp * IS * NL)/y
  
}

calc_fp_leaked <- function(fp,IS,NL){
  
  INL <- (fp * IS * NL)
  
}

calc_leakage_deforestation <- function(project, IS, NL, FP) {
  
  if (FP == "capped_FP") {
    project <- project %>%
      rowwise() %>%
      mutate(FP_leaked = calc_fp_leaked(fp = capped_FP, IS = IS, NL = NL)) %>%
      ungroup()
    
  } else if (FP == "FP_carrying_capacity") {
    project <- project %>%
      rowwise() %>%
      mutate(FP_leaked = calc_fp_leaked(fp = max_production, IS = IS, NL = NL)) %>%
      ungroup()
    
  } else if (FP == "FP_zero_growth") {
    project <- project %>%
      rowwise() %>%
      mutate(FP_leaked = calc_fp_leaked(fp = FP_zero_growth, IS = IS, NL = NL)) %>%
      ungroup()
    
  } else {
    stop("Invalid FP value. Use 'capped_FP', 'FP_carrying_capacity', or 'FP_zero_growth'")
  }
  
  project$leakage_scale <- "NA"
  return(project)
}
