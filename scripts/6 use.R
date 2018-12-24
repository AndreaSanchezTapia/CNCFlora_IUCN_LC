library(rvest)
library(stringr)
library(magrittr)
library(dplyr)

treespp <- read.csv("./results/names_flora.csv", row.names = 1)
names(treespp)
familias <- treespp$final_family
especies <- treespp$nombre
#Specifying the url for desired website to be scraped
url_id <- 'http://tropical.theferns.info/viewtropical.php?id='
url_full <- 'http://tropical.theferns.info/viewtropical.php?full='

names <- especies %>% stringr::str_split(" ", simplify = T) %>% data.frame() %>% rename(Genero = X1, epiteto = X2)
especie <- especies[1]
buscar_the_ferns <- function(especie) {
    names <- especie %>% stringr::str_split(" ", simplify = T) %>%
        data.frame() %>% rename(Genero = X1, epiteto = X2)
    url_vectors_id <-  paste0(url_id, names$Genero,"+", names$epiteto)
    #url_vectors_full <-  paste0(url_full, names$Genero,"+", names$epiteto)
    #Reading the HTML code from the website
    webpage_i <- read_html(url_vectors_id)
    #webpage_f <- read_html(url_vectors_full)
    rank_data_html <- html_nodes(webpage_i,'.PageBox')
    #rank_data_html2 <- html_nodes(webpage_f,'.PageBox')
    #Converting the ranking data to text
    teste <- html_text(rank_data_html) #### si no hay=0
    #teste2 <- html_text(rank_data_html2) #### si no hay=0
    write(teste,paste0("./results/use/",especie,".txt"))
    Sys.sleep(0.5)
}

especies[282]
i
for (i in 287:length(especies)) {
    print(especies[i], i)
    buscar_the_ferns(especies[i])

}

#211, 286 paila

files <- list.files("./results/use", full.names = T)
done <- files %>% str_split("/", simplify = T) %>% data.frame() %>% dplyr::select(4) %>% pull %>% str_split(".txt", simplify = T) %>% data.frame() %>% dplyr::select(1)
setdiff(done$X1, especies)
setdiff(especies, done$X1)
which(especies %in% setdiff(especies, done$X1))
head(files)
head(done)



Res <- list()
for (i in seq_along(files)) {

    names <- done[i,] %>% stringr::str_split(" ", simplify = T) %>%
        data.frame() %>% rename(Genero = X1, epiteto = X2)
    url_vectors_id <-  paste0(url_id, names$Genero,"+", names$epiteto)
    rl <- readLines(files[i],skipNul = T)
    gr <- paste0("^", done[i,], "$")
    gr2 <- rl[grep(gr, rl)]
    Res[[i]] <- data.frame(especies = done[i,]) %>%
        mutate(use = ifelse(length(gr2) == 0, "", "use")) %>%
        mutate(url = ifelse(use == "use",url_vectors_id,""))
}
Results_use <- Res %>% bind_rows()
write.csv(Results_use, "./results/Use_results_general.csv")

