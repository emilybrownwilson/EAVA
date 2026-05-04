library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

raw <- read_excel(
  file.path("inst", "extdata", "CCVA_codebook.xlsx")
)

ccva_codebook_1 <- raw %>%
  select(openVA_var, `2016_WHO_question`, `2016_WHO_var`) %>%

  rename(
    openVA_var = openVA_var,
    question = `2016_WHO_question`,
    who2016_var = `2016_WHO_var`
  ) %>%

  # Remove blank / NA question rows
  filter(!is.na(question) & str_trim(question) != "") %>%

  # Create group id: new group whenever openVA_var is NOT NA
  mutate(group_id = cumsum(!is.na(openVA_var))) %>%

  # Fill down identifiers so continuation rows inherit them
  fill(openVA_var, who2016_var, .direction = "down") %>%

  # Combine question text within each group
  group_by(group_id, openVA_var, who2016_var) %>%
  summarise(
    question = str_c(question, collapse = " "),
    .groups = "drop"
  ) %>%

  # Clean text
  mutate(
    question = str_squish(question)
  ) %>%

  select(who2016_var, question, openVA_var) %>%
  distinct()

  # Add EAVA-specific mapping from odk2EAVA

  whoNames_add <- c("Id10183","Id10167","Id10173","Id10167","Id10161","Id10250, Id10120","Id10182","Id10148","Id10234",
                    "Id10126","Id10446","Id10184","Id10151","Id10148","Id10234","Id10154","Id10173",
                    "Id10154","Id10173","Id10167","Id10120")
  whoNames_add <- tolower(whoNames_add)
  eavaNames <- c("i183b","fb_day0","i173b","i167c","i161b","swell_duration","i182d","i148d","i234c",
                 "i126o","i446o","i184b","i151b","i148e","i234d","i154c","i173c","i154d","i173d",
                 "i167d","i120d")

  eavaQs <- c("Did the baby or child have more than 4 stools on the day that loose liquid stools were most frequent?", #i183b
            "Fast breathing starting on day 0",            #fb_day0
            "Did his/her breathing sound like grunting?",  #i173b
            "Fast breathing lasting 1 day or more",        #i167c
            "Difficult breathing lasting 1 day or more",   #i161b
            "Swelling duration greater than or equal to illness duration", #swelll_duration
            "Liquid stools lasting more than 30 days",      #i182d
            "Fever lasting more than 30 days",              #i148d
            "Skin rash lasting more than 30 days",          #i234c
            "Was an HIV test ever positive?",               #i126o
            "Has the (biological) mother ever been told she had HIV/AIDS by a health worker?", #i446o
            "Did the frequent loose or liquid stools start more than 14 days before death?", #i184b
            "Was the pattern of fever on and off?", #i151b
            "Did the fever last at more than 2 days?", #i148e
            "Did the rash last for more than 2 days", #i234d
            "Did the cough last more than 2 weeks before death?", #i154c
            "Did his/her breathing sound like stridor?", #i173c
            "Did the cough last for more than 2 weeks before death?", #i154d
            "Did his/her breathing sound like stridor, grunting or wheezing?", #i173d
            "Did the fast breathing last for more than 2 days before death?", #i167d
            "Did the final illness last 1 day or less?"  #i120d
            )

    ccva_codebook_2 <- as.data.frame(cbind(whoNames_add,eavaQs,eavaNames))
    ccva_codebook_2 <- ccva_codebook_2 %>% rename(who2016_var = whoNames_add,
                                                  question = eavaQs,
                                                  openVA_var = eavaNames)

  # combine old and new codebooks
    ccva_codebook <- rbind(ccva_codebook_1, ccva_codebook_2)

    usethis::use_data(ccva_codebook, overwrite = TRUE)


