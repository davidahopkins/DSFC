#### Packages & Global####

library(blsR)


bls_set_key("9f8572a7b9514593b2b6896c0de5a6e2")

lab_series <- get_series_table("LNS14000000", 
                               start_year = 1991,
                               end_year = 2000,
                               parse_values = TRUE)

