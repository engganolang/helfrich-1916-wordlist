library(tidyverse)

source("Helfrich-1916-orthoprofile.R")

# load Helfrich's (1916) wordlist ======
helfrich_wl <- readr::read_rds("helfrich_wl.rds")
helfrich_wl

# handle the corrections for the English translation and the Dutch
helfrich_wl <- helfrich_wl |> 
  mutate(English = if_else(is.na(english_corrected), english_deepl, english_corrected)) |> 
  select(-english, -english_corrected, -english_deepl) |>  # remove the old English versions
  rename(english = English) |> 
  mutate(Dutch = if_else(is.na(dutch_corrected), dutch, dutch_corrected)) |> 
  select(-dutch, -dutch_corrected) |> # remove the old Dutch versions
  rename(dutch = Dutch)
helfrich_wl

helfrich_wl$form_tokenised <- h1916_form_common_tokenised
helfrich_wl <- helfrich_wl |> 
  mutate(form_non_tokenised = str_replace_all(form_tokenised, "((?<=[^;]) |(?<=[^,]) )", ""),
         form_non_tokenised = str_replace_all(form_non_tokenised, "\\#", " ")) |> 
  select(ID, page, entry, form, form_non_tokenised, form_tokenised, everything())


helfrich_wl$variant_tokenised <- h1916_variant_common_tokenised
helfrich_wl <- helfrich_wl |> 
  mutate(variant_non_tokenised = str_replace_all(variant_tokenised, "((?<=[^;]) |(?<=[^,]) )", ""),
         variant_non_tokenised = str_replace_all(variant_non_tokenised, "\\#", " ")) |> 
  select(ID, page, entry, form, form_non_tokenised, form_tokenised, variant, variant_non_tokenised, variant_tokenised, everything())

helfrich_wl$crossref_tokenised <- h1916_xref_common_tokenised
helfrich_wl <- helfrich_wl |> 
  mutate(crossref_non_tokenised = str_replace_all(crossref_tokenised, "((?<=[^;]) |(?<=[^,]) )", ""),
         crossref_non_tokenised = str_replace_all(crossref_non_tokenised, "\\#", " ")) |> 
  select(ID, page, entry, dutch, english, form, form_common_transcription = form_non_tokenised, form_common_segments = form_tokenised, variant, variant_common_transcription = variant_non_tokenised, variant_common_segments = variant_tokenised, crossref_form, crossref_common_transcription = crossref_non_tokenised, crossref_common_segments = crossref_tokenised, everything()) |> 
  mutate(english = str_replace_all(english, '"\\b', '“'), 
         english = str_replace_all(english, '\\b"', '”'),
         comment = str_replace_all(comment, '"\\b', '“'), 
         comment = str_replace_all(comment, '\\b"', '”'))

helfrich_wl$form_ipa_segments <- h1916_form_ipa_tokenised
helfrich_wl$variant_ipa_segments <- h1916_variant_ipa_tokenised
helfrich_wl$crossref_ipa_segments <- h1916_xref_ipa_tokenised

helfrich_wl <- helfrich_wl |> 
  mutate(variant_ID = if_else(!is.na(variant), row_number(variant), NA))

# save the form table
helfrich_wl |> 
  select(-matches("(variant$|variant_com|variant_ipa)")) |> 
  select(ID, page, entry, dutch, english, form, variant_ID, form_common_transcription, form_common_segments, form_ipa_segments, matches("crossref"), everything()) |> 
  write_tsv("data/helfrich1916.tsv", na = "")

# save the variant table
helfrich_wl |> 
  select(FORM_ID = ID, matches("variant")) |> 
  filter(!is.na(variant)) |> 
  select(ID = variant_ID, everything()) |> 
  write_tsv("data/helfrich1916_variant.tsv", na = "")


# all characters in Helfrich's (1916)
# all_chars_forms <- helfrich_wl %>% 
#   pull(form) %>% 
#   str_split("\\W") %>% 
#   map(function(x) x[nzchar(x)]) %>% 
#   unlist() %>% 
#   str_split("") %>% 
#   unlist() %>% 
#   table(letters = .) %>% 
#   tibble(letters = names(.), freq = .) %>% 
#   filter(str_detect(letters, "[0-9]", negate = TRUE))
# all_chars_variant <- helfrich_wl %>% 
#   filter(!is.na(variant)) %>% 
#   pull(variant) %>% 
#   str_split("\\W") %>% 
#   map(function(x) x[nzchar(x)]) %>% 
#   unlist() %>% 
#   str_split("") %>% 
#   unlist() %>% 
#   table(letters = .) %>% 
#   tibble(letters = names(.), freq = .) %>% 
#   filter(str_detect(letters, "[0-9]", negate = TRUE))
# all_chars_xref <- helfrich_wl %>% 
#   filter(!is.na(crossref_form)) %>% 
#   pull(crossref_form) %>% 
#   str_split("\\W") %>% 
#   map(function(x) x[nzchar(x)]) %>% 
#   unlist() %>% 
#   str_split("") %>% 
#   unlist() %>% 
#   table(letters = .) %>% 
#   tibble(letters = names(.), freq = .) %>% 
#   filter(str_detect(letters, "[0-9]", negate = TRUE))
# all_chars_combined <- all_chars_forms %>% 
#   bind_rows(all_chars_variant, all_chars_xref) %>% 
#   group_by(letters) %>% 
#   summarise(freq = sum(freq), .groups = "drop") %>% 
#   arrange(letters)
# # all_chars_combined %>% write_tsv("Helfrich-1916-wordlist-all-chars.tsv")
# # all_chars_combined %>% writexl::write_xlsx("Helfrich-1916-wordlist-all-chars.xlsx")
# helfrich_wl %>% 
#   filter(if_any(matches("crossref|form|variant"), 
#                 ~str_detect(., "(á|à|é)")))

# convert to common transcription ======
# helfrich_wl1 <- helfrich_wl %>% 
#   mutate(form_common = form,
#          variant_common = variant,
#          crossref_form_common = crossref_form,
#          across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(qä)", "'a")),
#          across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(dj)", "@")),
#          across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(nj)", "&")),
#          across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(tj)", "§")),
#          across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(j)", "y")),
#          across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "\\&", "ñ")),
#          across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "\\@", "j")),
#          across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "\\§", "c")),
#          across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(q)", "'")),
#          across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(ie|iè)", "i")),
#          across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(oeoe)", "u'u")),
#          across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(oe|oè)", "u")),
#          across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(ää)", "a'a")),
#          across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(öä)", "o'a")),
#          across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(ïï)", "i'i")),
#          across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(öö|oö)", "o'o")),
#          across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(ä)", "'a")),
#          across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(ü)", "'u")),
#          across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(ë)", "'e")),
#          across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(ï)", "'i")),
#          across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(ö)", "u̇")),
#          across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(ĕ|ě)", "ė")),
#          across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(è)", "e")),
#          across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(á)", "a")),
#          across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(ó)", "o"))
#          ) %>%
#   mutate(form_common_uncontr = form,
#          variant_common_uncontr = variant,
#          crossref_form_common_uncontr = crossref_form,
#          across(matches("_uncontr"), ~str_replace_all(., "(dj)", "@")),
#          across(matches("_uncontr"), ~str_replace_all(., "(nj)", "&")),
#          across(matches("_uncontr"), ~str_replace_all(., "(tj)", "§")),
#          across(matches("_uncontr"), ~str_replace_all(., "(j)", "y")),
#          across(matches("_uncontr"), ~str_replace_all(., "\\&", "ñ")),
#          across(matches("_uncontr"), ~str_replace_all(., "\\@", "j")),
#          across(matches("_uncontr"), ~str_replace_all(., "\\§", "c")),
#          across(matches("_uncontr"), ~str_replace_all(., "(ie|iè)", "i")),
#          across(matches("_uncontr"), ~str_replace_all(., "(oe|oè)", "u")),
#          across(matches("_uncontr"), ~str_replace_all(., "(ĕ|ě)", "ė")),
#          across(matches("_uncontr"), ~str_replace_all(., "(è)", "e")),
#          across(matches("_uncontr"), ~str_replace_all(., "(á)", "a")),
#          across(matches("_uncontr"), ~str_replace_all(., "(ó)", "o"))
#   ) %>% 
#   select(ID, page, entry, form, form_common, form_common_uncontr, variant, variant_common, variant_common_uncontr, dutch, english, crossref_form, crossref_form_common, crossref_form_common_uncontr, everything())
# 
# helfrich_wl1
# 
# helfrich_wl1 |> 
#   select(1:6, dutch, english) |> 
#   slice_sample(n = 10)
