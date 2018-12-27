library(googledrive)
library(dplyr)
crea el folder - no rodar
folder <- drive_mkdir("CNCFlora_LC")
# #esto fue la primera vez
Archivo <- list.files("./output_final/", recursive = T, full.names = T, pattern = "sp_filt.csv")


# #ejecuta para varios local_files la funcion drive_upload
file <- purrr::map(Archivo, drive_upload, path = folder)


#        #ejecuta drive_uplaod solo de lo que queremos
# drive_upload(Archivo, path = folder, verbose = TRUE)
#
# #where is the folder
# big <- drive_find(pattern = "Big", type = "folder")#
# bigpath <- drive_get(id = big$id) %>% drive_reveal("path")
# archivo <- drive_ls(bigpath) %>% drive_reveal(c("mime_type")) %>% slice(2)
# drive_update(file = archivo,
#              media = Archivo[1])
# drive_upload(media = Archivo[1],
#              path = bigpath$path,
#              verbose = TRUE,
#              type = "document")
#########
# DE AHORA EN ADELANTE
# big <- drive_get(path = "~/Bignoniae_MA") %>%
#     #drive_reveal("path") %>%
#     drive_ls()
#
# drive_update(file = big,
#              media = "./docs/big.docx")
