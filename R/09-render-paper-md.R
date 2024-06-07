
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

# fix inline latex:

read_file(path_md) |>
  str_replace("I\\*\\*n\\*\\*v\\*\\*G\\*\\*a\\*\\*m\\*\\*m\\*\\*a", "InvGamma") |>
  str_replace("s\\*\\*p\\*\\*l\\*\\*i\\*\\*n\\*\\*e", "spline") |>
  str_replace("s\\*\\*i\\*\\*t\\*\\*e\\*", "site") |>
  str_replace("H\\*\\*a\\*\\*l\\*\\*f", "Half") |>
  str_replace("c\\*\\*e\\*\\*n\\*\\*s\\*\\*o\\*\\*r\\*\\*e\\*\\*d", "censored") |>
  str_replace("o\\*\\*b\\*\\*s\\*\\*e\\*\\*r\\*\\*v\\*\\*e\\*\\*d", "observed") |>
  write_file(path_md)

# SI:

# rmarkdown::render("Rmarkdown/paper-si.Rmd")
