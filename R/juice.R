juice <- function(df, strat_clusters = 25){
    if(!is.data.frame(df) | !tibble::is_tibble(df)) stop("df must be a data.frame or tibble")
    
    # Data is small don't bother stratifying
    if(nrow(df) <= 100) return(df)

    # Data is big take a sample and stratify that
    if(nrow(df) >= 1000000) {
        df <- df[sample(nrow(df), 1000000, replace=FALSE), ]
        message("You supplied a large number of rows which have been randomly sampled.")
    }
    stratification_scheme <- cluster::clara(x = df, 
                                             k = strat_clusters,
                                             samples = 50,
                                             pamLike = TRUE
                              )
    df[stratification_scheme$i.med,]
}