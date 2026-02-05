library(rgcam)
library(dplyr)


# one of Reference, Carbon Tax, Lower Meat Demand, Total Carbon 30%, Species Richness 30%
# Scenario 3 APC2 CT10_5, Scenario 3 APC1p5 CT10_5, Scenario 3 LMD CT10_5
name <- "Total Carbon 30%"


# choose between:
# fooddemand, fooddemandprice, cropproduction, agprices, landprofit, 
# detailedlandallocation, landallocation, lucemissionsbyregion
#co2emissions, nonco2emissions, energy_query, csequestration, meatdemand
queryName <- "co2emissions"


make_query <- function(scenario){
  dbLoc <- paste0("/Users/fdolan/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/", scenario)
  queryFile = paste0("/Users/fdolan/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/queries/", queryName,".xml")
  queryData = paste0("/Users/fdolan/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/temp/", queryName,".dat")
  queryResult <- rgcam::addScenario(dbLoc, queryData, queryFile = queryFile)
  file.remove(queryData)
  queryResult[[1]][[1]]
}


# Scenario databases

# Reference: db_Reference

# Scenario 3
# db_HighIncomeLowMeatDemand_agprodchangeGlobalSouth2
#db_Scenario3_lmd_ct10_5, db_Scenario3_apc1p5_ct10_5, db_Scenario3_apc2_ct10_5

# Scenario 1
# db_Scenario1_TotalCarbon_Real30, db_Scenario1_SpeciesRichnessReal30

bind_rows(make_query("db_Scenario1_TotalCarbon_Real30"),
          make_query("db_Scenario1_TotalCarbon_Real30")) %>% unique() %>%
  readr::write_csv(paste0("/Users/fdolan/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/", queryName, "_", name, ".csv"))
                           

