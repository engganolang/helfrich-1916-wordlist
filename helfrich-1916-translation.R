# The code to run DeepL translator.

source("deeplkey.R")

library(deeplr)

nl <- hform |> 
  filter(!is.na(dutch)) |> 
  count(ID, dutch, sort = TRUE) |> 
  select(-n)

nl <- nl |> 
  mutate(indonesian = deeplr::translate2(dutch, 
                                         target_lang = "ID", 
                                         source_lang = "NL", 
                                         split_sentences = FALSE, 
                                         preserve_formatting = TRUE, 
                                         auth_key = deeplkey))
nl |> write_rds("nl_to_idn_via_deeplr.rds")



en <- hform |> 
  filter(!is.na(english)) |> 
  count(ID, english, sort = TRUE) |> 
  select(-n)

en <- en |> 
  mutate(indonesian = deeplr::translate2(english, 
                                         target_lang = "ID", 
                                         source_lang = "EN", 
                                         split_sentences = FALSE, 
                                         preserve_formatting = TRUE, 
                                         auth_key = deeplkey))

en |> write_rds("en_to_idn_via_deeplr.rds")
