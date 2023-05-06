library(tidyverse)

# load Helfrich's (1916) wordlist ======
helfrich_wl <- readr::read_rds("helfrich_wl.rds")
helfrich_wl

# all characters in Helfrich's (1916)
all_chars_forms <- helfrich_wl %>% 
  pull(form) %>% 
  str_split("\\W") %>% 
  map(function(x) x[nzchar(x)]) %>% 
  unlist() %>% 
  str_split("") %>% 
  unlist() %>% 
  table(letters = .) %>% 
  tibble(letters = names(.), freq = .) %>% 
  filter(str_detect(letters, "[0-9]", negate = TRUE))
all_chars_variant <- helfrich_wl %>% 
  filter(!is.na(variant)) %>% 
  pull(variant) %>% 
  str_split("\\W") %>% 
  map(function(x) x[nzchar(x)]) %>% 
  unlist() %>% 
  str_split("") %>% 
  unlist() %>% 
  table(letters = .) %>% 
  tibble(letters = names(.), freq = .) %>% 
  filter(str_detect(letters, "[0-9]", negate = TRUE))
all_chars_xref <- helfrich_wl %>% 
  filter(!is.na(crossref_form)) %>% 
  pull(crossref_form) %>% 
  str_split("\\W") %>% 
  map(function(x) x[nzchar(x)]) %>% 
  unlist() %>% 
  str_split("") %>% 
  unlist() %>% 
  table(letters = .) %>% 
  tibble(letters = names(.), freq = .) %>% 
  filter(str_detect(letters, "[0-9]", negate = TRUE))
all_chars_combined <- all_chars_forms %>% 
  bind_rows(all_chars_variant, all_chars_xref) %>% 
  group_by(letters) %>% 
  summarise(freq = sum(freq), .groups = "drop") %>% 
  arrange(letters)
# all_chars_combined %>% write_tsv("Helfrich-1916-wordlist-all-chars.tsv")
# all_chars_combined %>% writexl::write_xlsx("Helfrich-1916-wordlist-all-chars.xlsx")
helfrich_wl %>% 
  filter(if_any(matches("crossref|form|variant"), 
                ~str_detect(., "(á|à|é)")))

# convert to common transcription ======
helfrich_wl1 <- helfrich_wl %>% 
  mutate(form_common = form,
         variant_common = variant,
         crossref_form_common = crossref_form,
         across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(qä)", "'a")),
         across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(dj)", "@")),
         across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(nj)", "&")),
         across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(tj)", "§")),
         across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(j)", "y")),
         across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "\\&", "ñ")),
         across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "\\@", "j")),
         across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "\\§", "c")),
         across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(q)", "'")),
         across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(ie|iè)", "i")),
         across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(oeoe)", "u'u")),
         across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(oe|oè)", "u")),
         across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(ää)", "a'a")),
         across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(öä)", "o'a")),
         across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(ïï)", "i'i")),
         across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(öö|oö)", "o'o")),
         across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(ä)", "'a")),
         across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(ü)", "'u")),
         across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(ë)", "'e")),
         across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(ï)", "'i")),
         across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(ö)", "u̇")),
         across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(ĕ|ě)", "ė")),
         across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(è)", "e")),
         across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(á)", "a")),
         across(matches("(form_common|variant_common|crossref_form_common)"), ~str_replace_all(., "(ó)", "o")),
         ) %>%
  mutate(form_common_uncontr = form,
         variant_common_uncontr = variant,
         crossref_form_common_uncontr = crossref_form,
         across(matches("_uncontr"), ~str_replace_all(., "(dj)", "@")),
         across(matches("_uncontr"), ~str_replace_all(., "(nj)", "&")),
         across(matches("_uncontr"), ~str_replace_all(., "(tj)", "§")),
         across(matches("_uncontr"), ~str_replace_all(., "(j)", "y")),
         across(matches("_uncontr"), ~str_replace_all(., "\\&", "ñ")),
         across(matches("_uncontr"), ~str_replace_all(., "\\@", "j")),
         across(matches("_uncontr"), ~str_replace_all(., "\\§", "c")),
         across(matches("_uncontr"), ~str_replace_all(., "(oe|oè)", "u"))
  ) %>% 
  select(ID, page, entry, form, form_common, form_common_uncontr, variant, variant_common, variant_common_uncontr, dutch, english, crossref_form, crossref_form_common, crossref_form_common_uncontr, everything())

helfrich_wl1

