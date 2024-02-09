transform_mort_5yr <- function(mort_week){ 
    #' Sum weekly mortality counts by age category
    #'
    #' @description This function inputs mortality downloaded from ONS 
    #' "Deaths Registered in England and Wales" xls file.  
    #' 1. The input data records weekly counts.  These are summed to
    #' year totals. 
    #' 2. Next, the function aggregates counts according to the following 
    #' age categories:
    #' "Under 1 year" "1-14"  "15-44" "45-64" "65-74" "75-84" "85+" 
     
    #' 
    #' @param mort data table
    #'     column x1: 
    #'     twenty 5-year age categories up to 90+
    #'     columns x2-x53: weekly mortality counts
    #' @usage mort_mort(mort_dt)
    #' @return A data table one column for the six ONS age categories and 
    #' the other giving the mortality counts.  
    #'  and over
    
    setnames(mort_week, "x1", "age_cat")
    # Source data records weeks in columns. 
    # Pivot longer so that week is a single column.
    mort_w <-pivot_longer(mort_week, cols = !age_cat,
                             names_to = "week",
                             values_to = "count") |>
        as.data.table()
    mort_w[, week := NULL]
    
    mort_tx <- convert_5yr(mort_w)
    
    return(mort_tx)
}

convert_5yr <- function(mort_dt){
    
    # Sum all weeks by age category.
    mort_dt[, count_tot := sum(count), by = age_cat][, count := NULL]
    mort <- dplyr::distinct(mort_dt)
    
    # NOMIS population counts for years 2018 to 2023 does not have a separate 
    # category for under 1 year. Combine counts for age 0 with age 1-4:
    # age_0_4 <- mort[1, count_tot] + mort[2, count_tot]
    # mort[2, count_tot := age_0_4]
    
    # remove first row
    # mort <- mort[2:7, ]
    # mort[1, age_cat := "0-14"]
    
    setnames(mort, "count_tot", "count")
    
    # Record ONS age categories
    age_tx <- c("Under 1 year", "1-14", "15-44", "45-64", "65-74", "75-84", "85+" )
    
    # Count of persons in ONS age categories 
    # Under 1 year
    counts_tx <- mort[1, count]
    # 1-14 
    counts_tx <- c(counts_tx, mort[2:4, sum(count)])
    # "15-44"
    counts_tx <- c(counts_tx, mort[5:10, sum(count)])
    # "45-64"
    counts_tx <- c(counts_tx, mort[11:14, sum(count)])
    # "65-74"
    counts_tx <- c(counts_tx, mort[15:16, sum(count)])
    # "75-84"
    counts_tx <- c(counts_tx, mort[17:18, sum(count)])
    # 85+
    counts_tx <- c(counts_tx, mort[19:20, sum(count)])
    
    # Construct new data table for CSO categories
    mort_tx <- data.table(age_cat = age_tx,
                          count = counts_tx)
    return(mort_tx)
    
}
