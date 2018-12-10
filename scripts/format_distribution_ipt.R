#Reads and formats FdB----
library(readr)
##distribution----
# distribution  <- read_delim("./ipt/distribution.txt", delim = "\t", quote = "") %>%
#     group_by(id) %>%
#     mutate(location = paste(locationID, collapse = "-")) %>%
#     select(-locationID) %>% distinct() %>% ungroup()
library(tidyr)
library(stringr)
library(jsonlite)
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


###lifeform
lf_habitat <-
    read_delim("./ipt/speciesprofile.txt", delim = "\t", quote = "")
names(lf_habitat)
i <- 1
lf <- list()
hab <- list()
veg <- list()
for (i in seq_along(lf_habitat$lifeForm)) {
    lf[[i]] <- data.frame(id = lf_habitat$id[i], lifeForm = NA)
    hab[[i]] <- data.frame(id = lf_habitat$id[i], habitat = NA)
    veg[[i]] <- data.frame(id = lf_habitat$id[i], vegetationType = NA)
    if (!is.na(lf_habitat$lifeForm[i])) {
        jason <- jsonlite::fromJSON(as.character(lf_habitat$lifeForm[i]))
        if ("lifeForm" %in% names(jason)) lf[[i]] <- data.frame(id = lf_habitat$id[i], lifeForm = jason["lifeForm"])
        if ("habitat" %in% names(jason)) hab[[i]] <- data.frame(id = lf_habitat$id[i], habitat = jason["habitat"])
        if ("vegetationType" %in% names(jason)) veg[[i]] <- data.frame(id = lf_habitat$id[i], vegetationType = jason["vegetationType"])
        }
    }
head(lf)
head(veg)
head(hab)
lf2 <- lf %>%
    purrr::map(~ mutate(., lifeForm = paste(lifeForm, collapse = "-"))) %>%
    purrr::map(~distinct(.))
lf3 <- bind_rows(lf2)
veg2 <- veg %>%
    purrr::map(~ mutate(., vegetationType = paste(vegetationType, collapse = "-"))) %>%
    purrr::map(~distinct(.))
veg3 <- bind_rows(veg2)

hab2 <- hab %>%
    purrr::map(~ mutate(., habitat = paste(habitat, collapse = "-"))) %>%
    purrr::map(~distinct(.))
hab3 <- bind_rows(hab2)

lf_hab <- lf3 %>% left_join(veg3) %>% left_join(hab3)
lf_habitat %>%
    #filter(!is.na(lifeForm)) %>%
    head(.)
lf_hab %>%
    #filter(lifeForm != "NA") %>%
    head(.)
write.csv(lf_hab, "./ipt/lf_hab_modified.csv")
# distribution_mod <- distribution %>% mutate(occurrenceRemarks = omdf$om_all)
# write.csv(distribution_mod, "./ipt/distribution_modified.csv")
