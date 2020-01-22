# for some reason, there is 82 data points for Glaucoma participants, 
#    but we know that there are only 81 people with glaucoma.
#    There must be some reason the extra person is counted...

# function to get glaucoma diagnosis
get_diag <- function(df = data_octa_global_intra, group){
  df %>%
    # filter by glaucoma status
    filter(glc_dx_bin == group) %>% group_by(ptid) %>% 
    # take the average glc_dx_bin score to see if anything is different from integer values
    #    if someone had glaucoma in one eye, but not in another, then the average would be a decimal.
    summarise(avg_glc = mean(na.rm = TRUE, as.double(glc_dx_bin))) 
}

# be sure that there are 
#    68 normal people without glaucoma and 
#    81 people with glaucoma
data_normal   <- get_diag(group = "Normal")
data_glaucoma <- get_diag(group = "Glaucoma")

  data_normal %>% filter(avg_glc != 1) %>% nrow()
data_glaucoma %>% filter(avg_glc != 2) %>% nrow()

# check if there are people counted in both groups
inner_join(by = "ptid", x = data_normal, y = data_glaucoma) %>% nrow()



######################################

test <- print_table1_people(data_octa_global_intra)
test_logic <- test %>% select(ptid) %>% duplicated()
test$ptid[test_logic]

# this person ID has more than one observation in the data
test$glc_dx_bin[test$ptid == 6862281]

# look at the raw data for sociodemographics to see if any other repeated numbers
data_sociodem %>% select(ptid) %>% duplicated()


# choose the first value 
data_sociodem <- read_xlsx(paste0(dir_data, "/R&R_PoHX_IOP_all.xlsx"), sheet = "PoHx") %>% 
  form_sociodem_vars() %>% 
  exclude_patients()
data_sociodem %>% filter(ptid == 6862281)
