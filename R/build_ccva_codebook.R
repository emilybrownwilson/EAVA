library(readxl)
library(dplyr)
library(stringr)

raw <- read_excel(
  file.path("inst", "extdata", "CCVA_codebook.xlsx")
)

ccva_codebook <- raw %>%
  select(openVA_var, `2016_WHO_question`, `2016_WHO_var`) %>%

  rename(
    openVA_var = openVA_var,
    question = `2016_WHO_question`,
    who2016_var = `2016_WHO_var`
  ) %>%

  # Drop rows with no OpenVA variable
  filter(!is.na(openVA_var)) %>%

  # Clean text
  mutate(
    question = str_replace_all(question, "\n", " "),
    question = str_squish(question)
  ) %>%
  select(who2016_var, question, openVA_var) %>%

  distinct()

usethis::use_data(ccva_codebook, overwrite = TRUE)
