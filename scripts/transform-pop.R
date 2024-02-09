transform_pop <- function(persons_dt){
    #' Transform from 5-year age groups 
    #' to age categories used in ONS mortality counts
    #'
    #' 
    #' @description This function converts 5 yr age categories 
    #' to 10 of the groups in mortality table for the year 2018:
    #' "0-14",  "15-44",  "45-64",  "65-74, "75-84", "85+" 
    #' and sums the number of persons according to these age categories
    #' Counts may be population numbers or number of deaths
    #' 
    #' @param persons_dt data table. First column: "age_cat" 
    #' identifies the age categories in source data   
    #' In most cases, there are 17 five-year age categories, 
    #' concluding with the "80+" group 
    #' Second column: "count" 
    #' gives the number of persons in each age group.
    #' @usage transform_pop(persons_dt)
    #' @return A data table containing the 14 CSO age 
    #' categories and the number of persons in each group.
    
    # Record ONS age categories
    age_tx <- c("0-14", "15-44", "45-64", "65-74", "75-84", "85+" )
 
    # Count of persons in CSO age categories 
    # 0-14 
    counts_tx <- c(persons_dt[1:3, sum(count)])
    # "15-44"
    counts_tx <- c(counts_tx, persons_dt[4:9, sum(count)])
    # "45-64"
    counts_tx <- c(counts_tx, persons_dt[10:13, sum(count)])
    # "65-74"
    counts_tx <- c(counts_tx, persons_dt[14:15, sum(count)])
    # "75-84"
    counts_tx <- c(counts_tx, persons_dt[16:17, sum(count)])
    # 85+
    counts_tx <- c(counts_tx, persons_dt[18, count])
        
    # Construct new data table for CSO categories
    persons_tx <- data.table(age_cat = age_tx,
                             count = counts_tx)
    return(persons_tx)
}