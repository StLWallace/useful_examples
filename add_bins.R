add_bins <- function(df, col_name, ref) {
  require(dplyr)
  dist_vals <- if(is.factor(ref[, col_name])) {
    levels(ref[, col_name])
  } else distinct(ref[col_name])[, 1]
  
  df_aug <- df
  n <- length(dist_vals)
  
  for(i in 1:(n-1)) {
    df_aug[paste(col_name, as.character(dist_vals[i]), sep = "_"] <- as.integer(ifelse(as.character(df_aug[, col_name]) == dist_vals[i], 1, 0))
  }
  
  df_aug
}