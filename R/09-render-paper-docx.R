
# setup -------------------------------------------------------------------

library("readr")
library("stringr")
library("rmarkdown")

# modify rmd --------------------------------------------------------------

# paper:

path <- "Rmarkdown/paper.Rmd" # path to Rmd
path_temp <- str_replace(path,"(.+/)(.+)", "\\1_\\2") # path to temporary modified version
path_docx <- str_replace(path, "\\.Rmd", "\\.docx") # path to Word doc
path_temp_docx <- str_replace(path_temp, "\\.Rmd", "\\.docx") # path to temporary Word doc

# modify citations:

read_file(path) |>
  # move arabic numberal refs after punctuation:
  str_replace_all("\\s+(\\[@.*?\\])(\\)?[.,;:])?", "\\2\\1") |>
  write_file(path_temp)

# render ------------------------------------------------------------------

rmarkdown::render(path_temp) # render

file.rename(path_temp_docx, path_docx) # rename temporary Word file

unlink(path_temp)

system(paste("open", path_docx)) # open Word document

# SI:

# rmarkdown::render("Rmarkdown/paper-si.Rmd")
