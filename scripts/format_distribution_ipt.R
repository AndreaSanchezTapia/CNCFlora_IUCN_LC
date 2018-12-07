#Reads and formats FdB----
library(readr)
##distribution----
# distribution  <- read_delim("./ipt/distribution.txt", delim = "\t", quote = "") %>%
#     group_by(id) %>%
#     mutate(location = paste(locationID, collapse = "-")) %>%
#     select(-locationID) %>% distinct() %>% ungroup()
# library(tidyr)
# library(stringr)
# library(jsonlite)
# names(distribution)
# ocurrence_remarks <- distribution %>% select(occurrenceRemarks) %>%
#     #slice(10000:10100) %>%
#     data.frame() %>%
#     #pull() %>%
#     mutate(occurrenceRemarks = as.character(occurrenceRemarks))
#
# occurrenceRemarks_df <-
#     purrr::map(ocurrence_remarks$occurrenceRemarks,
#                ~data.frame(jsonlite::fromJSON(.))) %>%
#     #purrr::map(., ~ifelse(is.null(dim(.)), ,)
#     purrr::map(.,
#                ~ mutate(.,
#                         om = paste(.$endemism, .$phytogeographicDomain,
#                                    sep = "/"))) %>%
#     purrr::map( ~ mutate(., om_all = paste(om, collapse = "-"))) %>%
#     purrr::map( ~ select(., om_all)) %>%
#     purrr::map( ~ distinct(.)) %>%
#     purrr::map(., .f = ~change_others_to_dataframe(.))
#
# omdf <- bind_rows(occurrenceRemarks_df,.id = "sp")
# omdf[10031:10033,]
#
# #omdf <- data.table::rbindlist(occurrenceRemarks_df, idcol = T)
# omdf <- data.frame(omdf)
# head(omdf)
# names(distribution)
# distribution_mod <- distribution %>% mutate(occurrenceRemarks = omdf$om_all)
# write.csv(distribution_mod, "./ipt/distribution_modified.csv")
