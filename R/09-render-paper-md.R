
# notes -------------------------------------------------------------------

# changes from paper.Rmd -> README.Rmd

# 1. changed figure paths to URLs
# 2. added <br> after figure chunks
# 3. changed \\ inside bmatrix to \\\

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

# modifications to md doc:

read_file(path_md) |>
  # fix inline latex:
  str_replace_all("I\\*\\*n\\*\\*v\\*\\*G\\*\\*a\\*\\*m\\*\\*m\\*\\*a", "InvGamma") |>
  str_replace_all("s\\*\\*p\\*\\*l\\*\\*i\\*\\*n\\*\\*e", "spline") |>
  str_replace_all("s\\*\\*i\\*\\*t\\*\\*e", "site") |>
  str_replace_all("H\\*\\*a\\*\\*l\\*\\*f", "Half") |>
  str_replace_all("l\\*\\*e\\*\\*f\\*\\*t", "left") |>
  str_replace_all("c\\*\\*e\\*\\*n\\*\\*s\\*\\*o\\*\\*r\\*\\*e\\*\\*d", "censored") |>
  str_replace_all("o\\*\\*b\\*\\*s\\*\\*e\\*\\*r\\*\\*v\\*\\*e\\*\\*d", "observed") |>
  # change paper -> document:
  str_replace_all("this paper", "this document") |>
  # remove extra table captions:
  str_remove("\\*\\*Table \\d\\.\\*\\*[^\\.]+\\.") |>
  write_file(path_md)

# SI:

# rmarkdown::render("Rmarkdown/paper-si.Rmd")
