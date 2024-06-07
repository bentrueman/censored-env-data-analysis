
# setup -------------------------------------------------------------------

library("readr")
library("stringr")
library("rmarkdown")

# modify rmd --------------------------------------------------------------

# paper:

path <- "Rmarkdown/README.Rmd" # path to Rmd
path_temp <- str_replace(path,"(.+/)(.+)", "\\1_\\2") # path to temporary modified version
path_md <- str_replace(path, "\\.Rmd", "\\.md") # path to md doc
path_temp_md <- str_replace(path_temp, "\\.Rmd", "\\.md") # path to temporary md doc

# modify citations:

read_file(path) |>
  # move arabic numberal refs after punctuation:
  str_replace_all("\\s+(\\[@.*?\\])(\\)?[.,;:])?", "\\2\\1") |>
  write_file(path_temp)

# render ------------------------------------------------------------------

rmarkdown::render(path_temp, output_format = "md_document") # render

file.rename(path_temp_md, path_md) # rename temporary md file

unlink(path_temp)

# SI:

# rmarkdown::render("Rmarkdown/paper-si.Rmd")
