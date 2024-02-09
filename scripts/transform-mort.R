transform_mort <- function(mort_week){ 
    #' Sum weekly mortality counts by age category
    #'
    #' @description This function inputs mortality downloaded from ONS 
    #' "Deaths Registered in England and Wales" xls file.  It extracts the 
    #' following age categories:
    #' "0-14"  "15-44" "45-64" "65-74" "75-84" "85+" 
    #' The input data records weekly counts.  These are aggregated to
    #' year totals.  
    #' 
    #' @param mort data table
    #'     column x1: age categories
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
    
    # Sum all weeks by age category.
    mort_w[, count_tot := sum(count), by = age_cat][, count := NULL]
    mort_w[, week := NULL]
    mort <- dplyr::distinct(mort_w)
    
    # NOMIS population counts for years 2018 to 2023 does not have a separate 
    # category for under 1 year. Combine counts for age 0 with age 1-14:
    # age_0_14 <- mort[1, count_tot] + mort[2, count_tot]
    # mort[2, count_tot := age_0_14]
    
    # remove first row
    # mort <- mort[2:7, ]
    # mort[1, age_cat := "0-14"]
    
    setnames(mort, "count_tot", "count")
    
    
    return(mort)
}