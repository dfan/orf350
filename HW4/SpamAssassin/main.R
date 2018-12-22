#replace the line below with the folder with the "spam" and "easy_ham" folders
top = "C:/Users/Uikos/Dropbox/GitHub/SML"
Directories = c("easy_ham","spam")
dirs = paste(top, Directories, sep ="/")

source("readRawEmail.R")
mail = readAllMessages(dirs = dirs)
