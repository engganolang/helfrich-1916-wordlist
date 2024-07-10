library(tidyverse)
library(stringi)
library(qlcData)

helfrich_wl <- readr::read_rds("helfrich_wl.rds") |> 
  mutate(ortho_id_form = row_number(),
         ortho_id_variant = row_number(),
         ortho_id_xref = row_number(),
         ortho_id_form = if_else(is.na(form), 0, ortho_id_form),
         ortho_id_variant = if_else(is.na(variant), 0, ortho_id_variant),
         ortho_id_xref = if_else(is.na(crossref_form), 0, ortho_id_xref))

# dir.create("ortho")

# Create profile ====
## form column ====
# helfrich_wl |> 
#   filter(!is.na(form)) |> 
#   pull(form) |> 
#   qlcData::write.profile(normalize = "NFC", editing = TRUE, info = FALSE,
#                          file.out = "ortho/_01-h1916-form_profile-skeleton.tsv")
## variant column ====
# helfrich_wl |> 
#   filter(!is.na(variant)) |> 
#   pull(variant) |> 
#   qlcData::write.profile(normalize = "NFC", editing = TRUE, info = FALSE,
#                          file.out = "ortho/_02-h1916-variant_profile-skeleton.tsv")

## cross-reference column ====
# helfrich_wl |> 
#   filter(!is.na(crossref_form)) |> 
#   pull(crossref_form) |> 
#   qlcData::write.profile(normalize = "NFC", editing = TRUE, info = FALSE,
#                          file.out = "ortho/_03-h1916-xref_profile-skeleton.tsv")


# transliterate ====
read_tsv("https://raw.githubusercontent.com/engganolang/enolex/main/ortho/_12-helfrich1916_profile-skeleton-ipa.tsv") |> 
  write_tsv("ortho/_00-ortho-ipa.tsv", na = "")

## form ====
h1916 <- qlcData::tokenize(helfrich_wl$form, 
                           profile = "ortho/_01-h1916-form_profile-skeleton.tsv", 
                           file.out = "ortho/_04-h1916-form",
                           method = "global",
                           transliterate = "Replacement", 
                           ordering = NULL, # cf. Moran & Cysouw (2018: 112-114)
                           normalize = "NFC", 
                           sep.replace = "#",
                           regex = TRUE)
# h1916_form_ipa_tokenised <- qlcData::tokenize(helfrich_wl$form, 
#                                               profile = "ortho/_00-ortho-ipa.tsv", 
#                                               file.out = "ortho/_04-h1916-form-ipa",
#                                               method = "global",
#                                               transliterate = "Phoneme", 
#                                               ordering = NULL, # cf. Moran & Cysouw (2018: 112-114)
#                                               normalize = "NFC", 
#                                               sep.replace = "#",
#                                               regex = TRUE)
h1916_form_common_tokenised <- h1916$strings$transliterated

## variant ====
h1916_variant <- qlcData::tokenize(helfrich_wl$variant, 
                                   profile = "ortho/_01-h1916-form_profile-skeleton.tsv", 
                                   file.out = "ortho/_04-h1916-form",
                                   method = "global",
                                   transliterate = "Replacement", 
                                   ordering = NULL, # cf. Moran & Cysouw (2018: 112-114)
                                   normalize = "NFC", 
                                   sep.replace = "#",
                                   regex = TRUE)

h1916_variant_common_tokenised <- h1916_variant$strings$transliterated

## cross-ref ====
h1916_xref <- qlcData::tokenize(helfrich_wl$crossref_form, 
                                profile = "ortho/_01-h1916-form_profile-skeleton.tsv", 
                                file.out = "ortho/_04-h1916-form",
                                method = "global",
                                transliterate = "Replacement", 
                                ordering = NULL, # cf. Moran & Cysouw (2018: 112-114)
                                normalize = "NFC", 
                                sep.replace = "#",
                                regex = TRUE)

h1916_xref_common_tokenised <- h1916_xref$strings$transliterated


