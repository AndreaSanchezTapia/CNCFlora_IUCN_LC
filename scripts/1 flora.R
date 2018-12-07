#Flora do Brasil first
##loads packages----
library(readxl)
library(dplyr)
library(flora)
library(readr)
# read original data----
treespp <- read_excel("./data/LeastConcern_BrazilEndemics_original.xlsx", sheet = 1)
names(treespp)
#especies <- purrr::map(especies, ~remove.authors(.)) %>% simplify2array()
treespp <- treespp %>%
    mutate(original_binomial = purrr::map(ScientificName, ~remove.authors(.)) %>%
               simplify2array())

#Downloads data from FdB----
#library("downloader")
#pag <- "http://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil"
#download(url = pag, destfile = "iptflora")
#unzip("iptflora",exdir = "./ipt")



#####------
taxon <- read_delim("./ipt/taxon.txt", delim = "\t", quote = "")

relationship <- read_delim("./ipt/resourcerelationship.txt",
           delim = "\t", quote = "") %>% distinct()

ref <- read_delim("./ipt/reference.txt",delim = "\t", quote = "")
lf_habitat <- read_delim("./ipt/speciesprofile.txt", delim = "\t", quote = "")
types <- read_delim("./ipt/typesandspecimen.txt", delim = "\t", quote = "") %>%
    group_by(id) %>%
    mutate_all(.funs = function(x) paste(x, collapse = "-"))

vernacular <- read_delim("./ipt/vernacularname.txt", delim = "\t") %>%
    mutate(vernacular = paste(vernacularName, language, locality, sep = "-")) %>%
    select(id, vernacular) %>%
    group_by(id) %>% mutate(all_names = paste(vernacular, collapse = "/")) %>%
    select(-vernacular) %>% distinct()

relacion <- unique(relationship$relationshipOfResource)
relacion

taxon_dist <- left_join(taxon, distribution)
taxon_dist_ref <- left_join(taxon, distribution, ref)
taxon_dist_ref_sp <- left_join(taxon_dist_ref, sp)
taxon_dist_ref_sp_types <- left_join(taxon_dist_ref_sp, types_collapsed)

test <- left_join(taxon_dist_ref_sp, types)
all <- left_join(taxon_dist_ref_sp_types, vernacular_edit)
all <- mutate(all, nombre = paste (genus, specificEpithet))

#los nombres están aceitos en la flora?
taxon.filt2 <- filter(all, nombre %in% especies)
taxon.filt2 <- filter(all, nombre %in% especies, taxonRank == "ESPECIES")
aceitos <- taxon.filt2 %>% filter(taxonomicStatus == "NOME_ACEITO")
sinonimos <- taxon.filt2 %>%
    filter(taxonomicStatus != "NOME_ACEITO") %>%
    left_join(relationship) %>% group_by(id) %>%
    mutate(sinonimo_de= relatedResourceID)
sin.filt <- filter(all, id %in% sinonimos$sinonimo_de)
sin.filt <- filter(all, id %in% sinonimos$sinonimo_de) %>% mutate(id_correct = id)

