#-------------------------------------------
# Author  : Izi (izi31416@protonmail.com)
# Project :
# Created :
# License :
# Updated :
#-------------------------------------------
round_df <- function(x, dig = 4)
{
    numeric_columns <- sapply(x, class) == 'numeric'
    x[numeric_columns] <-  round(x[numeric_columns], dig)
    x
}
