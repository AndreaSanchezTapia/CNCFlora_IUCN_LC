# Format Flora do Brasil first
##loads packages----
library(dplyr)
library(flora)
library(readr)

#Downloads data from FdB----
#library("downloader")
#pag <- "http://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil"
#download(url = pag, destfile = "iptflora")
#unzip("iptflora",exdir = "./ipt")

#reads formatted distribution
distribution <- read_csv("./ipt/distribution_modified.csv") %>%
    dplyr::select(-1)
head(distribution)

#####------
taxon <- read_tsv("./ipt/taxon.txt", quote = "", trim_ws = T)
head(taxon)
relationship <- read_delim("./ipt/resourcerelationship.txt",
           delim = "\t", quote = "") %>%
    distinct()

ref <- read_delim("./ipt/reference.txt",delim = "\t", quote = "")
#lf_habitat <- read_delim("./ipt/speciesprofile.txt", delim = "\t", quote = "")
lf_mod <- read.csv("./ipt/lf_hab_modified.csv")
head(lf_mod)
length(unique(lf_mod$id))
types <- read_delim("./ipt/typesandspecimen.txt", delim = "\t", quote = "") %>%
    group_by(id) %>%
    mutate_all(.funs = function(x) paste(x, collapse = "-"))

vernacular <- read_delim("./ipt/vernacularname.txt", delim = "\t") %>%
    mutate(vernacular = paste(vernacularName, language, locality, sep = "-")) %>%
    dplyr::select(id, vernacular) %>%
    group_by(id) %>% mutate(vernacular_names = paste(vernacular, collapse = "/")) %>%
    dplyr::select(-vernacular) %>% distinct()
names(vernacular)
relacion <- unique(relationship$relationshipOfResource)
relacion

taxon_dist               <- left_join(taxon, distribution)
taxon_dist_ref           <- left_join(taxon_dist, ref)
taxon_dist_ref_lfh       <- left_join(taxon_dist_ref, lf_mod)
taxon_dist_ref_lfh_types <- left_join(taxon_dist_ref_lfh, types)
all                      <- left_join(taxon_dist_ref_lfh_types, vernacular)
all <- all %>% distinct()
all <- all %>% mutate(nombre = purrr::map(scientificName, ~remove.authors(.)) %>%
           simplify2array())
write.csv(all, "./ipt/all_flora.csv")
