
library(DBI)
library(dplyr)

#reading input dataset
my_cnf_file <- "C:/Users/C837393425/.my.cnf" #add your cnf
trillium    <- "trillium.nrel.colostate.edu"
dbName      <- "inv2022_output"
tblName1    <- "INV2022_OUT_NRI_Yearly_Iter1_rev491_COMP_FEB2024"
tblName2    <- "INV2022_OUT_NRI_Yearly_Iter1_rev513_COMP_FEB2024"

db_conn <- dbConnect(RMySQL::MySQL(), 
                     host = trillium, 
                     dbname = dbName, 
                     default.file = my_cnf_file)

#Readuing Daycent 491 outputs
query_491 <- paste("SELECT * FROM", tblName1)  # Adjust query if needed
df_491 <- dbGetQuery(db_conn, query_491)

#Readuing Daycent 513  outputs
query_513 <- paste("SELECT * FROM", tblName2)  # Adjust query if needed
df_513 <- dbGetQuery(db_conn, query_513)

#site info reading 
dbName_nri      <- "nri2017"
db_conn_nri <- dbConnect(RMySQL::MySQL(), 
                         host = trillium, 
                         dbname = dbName_nri, 
                         default.file = my_cnf_file)

tblName_nri    <- "LandArea_Combined_1979_2022"
query_nri <- paste("SELECT * FROM", tblName_nri)  
nri_point_info <- dbGetQuery(db_conn_nri, query_nri)

tblName_site_info <- "INV_lookup_point_site_info"
query_site <- paste("SELECT * FROM", tblName_site_info)  
site_info <- dbGetQuery(db_conn_nri, query_site)

nri_all_info <- nri_point_info %>%
  left_join(site_info %>% select(recordid2017, state_name), by = "recordid2017")




#Example
variables <- c("somsc", "n2oflux", "strmac1", "sdrema", "egrain1", "fertot21", "annppt", "accrst")
variable_units <- c("(ton/ha)", "(ton/ha)", "(ton/ha)", "(ton/ha)", "(ton/ha)", "(ton/ha)", "(cm)", "(ton/ha)" )
plot_types <- c("line", "scatter", "both", "scatter", "scatter", "scatter", "scatter", "scatter")
output_dir = "N:/Research/Ogle/DayCent_Model_Development/temp/DayCent_output_comparison"
highest_no_of_plots_per_page = 6

#load function source file
source("N:/Research/Ogle/DayCent_Model_Development/temp/DayCent_output_comparison/create_plots_function.R")

create_plots(    df1 = df_491, 
                 df2 = df_513, 
                 nri_all_info = nri_all_info, 
                 variables = variables, 
                 variable_units = variable_units,
                 plot_types = plot_types,
                 no_of_plots_per_page = highest_no_of_plots_per_page,
                 output_dir = output_dir)



