library(readr)
library(readxl)
library(dplyr)
library(flora)
#read flora data
flora <- read_csv("./ipt/all_flora.csv") %>% dplyr::select(-1)
names(flora)
# read original data----
treespp <- read_excel("./data/LeastConcern_BrazilEndemics_original.xlsx", sheet = 1) %>%
    rename(scientificName = ScientificName) %>%
    mutate(nombre = purrr::map(scientificName, ~remove.authors(.)) %>%
               simplify2array())
treespp$nombre
treespp[642,] %>% View()
treesspp2 <- treespp %>%
    dplyr::select(Family, scientificName, nombre) %>%
    left_join(flora) %>%
    dplyr::select(
           Family,
           scientificName,
           nombre,
           taxonID,
           acceptedNameUsageID,
           acceptedNameUsage,
           parentNameUsage,
           family,
           taxonomicStatus,
           nomenclaturalStatus
           #occurrenceRemarks,
           #location,
           #lifeForm,
           #vegetationType,
           #habitat,
           #locality
           )

treesp3 <- treesspp2 %>%
    #new column final_name
    mutate(final_name = ifelse(taxonomicStatus == "NOME_ACEITO", scientificName, acceptedNameUsage)) %>%
    mutate(final_ID = ifelse(taxonomicStatus == "NOME_ACEITO", taxonID, acceptedNameUsageID)) %>%
    mutate(final_name = ifelse(is.na(taxonID) & is.na(acceptedNameUsageID), scientificName, final_name)) %>%
    #new column notes
    mutate(notes = if_else(is.na(taxonID) & is.na(acceptedNameUsageID),
                           "not in LFB", "LFB")) %>%
    #new column syn
    mutate(tax_notes = if_else(taxonomicStatus == "NOME_ACEITO", "name ok", "")) %>%
    mutate(tax_notes = if_else(taxonomicStatus == "SINONIMO", "synonym", tax_notes)) %>%
    mutate(family = if_else(!is.na(family), family, Family)) %>%
    #mutate(notes_fam = if_else(family != Family, "family changed", "")) %>%
    dplyr::select(scientificName, taxonID, final_name, final_ID, notes,tax_notes, family) %>%
    rename(original_name = scientificName,
           original_ID = taxonID,
           scientificName = final_name,
           taxonID = final_ID)
write.csv(treesp3, "./results/names_flora1.csv")

names(treesp3)
names(flora)
treesp4 <- treesp3 %>%
    left_join(flora) %>%
dplyr::select(-c(8:31, 35:40, 44:46, 48:50))
names(treesp4)
write.csv(treesp4, "./results/names_flora.csv")
treesp4 %>% View()
