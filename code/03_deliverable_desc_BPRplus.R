
###############################################################################
# - AUTOMATISK NYCKELTALSBESKRIVNING -                                        #
#      - LEVERANS TILL KOLADA -                                               #
#                                                                             #
# Alla nyckeltal i Kolada måste ha en beskrivning, detta inkluderar de index  #
# som levereras, däribland index tillhörande BRP+. Det finns 6x17 olika       #
# nyckeltalsbeskrvningar, denna kod skapar dessa beskrivningar automatiskt.   #
#                                                                             #
#                                                                             #
# Eric Klingener, 2025-11-24                                                  #
###############################################################################

# rm(list=ls())

# 0. Installera nödvändiga paket ####
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, rKolada, dplyr, purrr, stringr)
pacman::p_loaded(here, rKolada, dplyr, purrr, stringr)

#    ####

# 1. Importera brpplus_data_base ####
# 1.1. Sätt path och packa upp
# Hitta mapp där hela RProject är sparat och skapa path för /brpplus_data_base ZIP-filen med brpplus_data_base_base
zip_path <- here::here("data", "BRP_plus_data_base.zip")

# One-block: unzip to temp, read CSV, remove temp automatically
brpplus_data_base <- {
  tmp_dir <- tempdir() # create a temporary folder
  unzip(zip_path, exdir = tmp_dir) # unzip the CSV to the temp folder
  csv_file <- list.files(tmp_dir, pattern = "\\.csv$", full.names = TRUE) # find the CSV file (assumes only one CSV in the ZIP)
  read.csv2(csv_file, stringsAsFactors = FALSE) # read CSV
}

# Kontrollera att importen fungerade
head(brpplus_data_base)
n_distinct(brpplus_data_base$kpi)


#    ####

# 2. Hämta alla nyckeltals beskrivningar från Kolada ####
# 2.1. Skapa en lista av unika nyckeltal och annat
kpi_ids <- brpplus_data_base %>%
  distinct(kpi, Tema, Del, municipality_type, gender)

# 2.2. Skapa funktion som hämta hem alla nyckeltals beskrivningar 
fetch_kpi_meta <- function(id) {
  get_kpi(id) %>% select(kpi = id, title, description)
}

# 2.3. Skapa en dataframe
kpi_meta <- purrr::map_df(unique(kpi_ids$kpi), fetch_kpi_meta)

# 2.4. Join'a metadata med resten av kolumnerna från brpplus_data_base
kpi_ids_meta <- kpi_ids %>% left_join(kpi_meta, by = "kpi")

# 2.5 Skapa ny kolumn 'source'
final_df <- kpi_ids_meta %>%
  mutate(
    # Försök 1: allt efter "Källa:" (med kolon)
    source_try1 = str_extract(description, "(?<=Källa:).*"),
    # Försök 2: allt efter "Källa" utan kolon (t.ex. "Källa SCB ...", eller "Källa - SCB")
    # Vi tillåter eventuella kolon, bindestreck eller blanksteg efter "Källa" i denna extrahering
    source_try2 = str_extract(description, "(?<=Källa)[:\\-\\s]*.*"),
    # Välj första icke-NA (dvs. föredra try1 men fallback till try2)
    source_raw  = coalesce(source_try1, source_try2),
    # Städa den valda texten
    source = source_raw %>%
      # ersätt escape-sekvenser och nya rader med mellanslag
      str_replace_all("\\\\n", " ") %>%
      str_replace_all("\\n", " ") %>%
      # ta bort punkttecken
      str_replace_all("\\.", "") %>%
      # ta bort eventuella kvarvarande "Källa" eller "Källa:" i början
      str_replace_all("^\\s*Källa[:\\-\\s]*", "") %>%
      # ta bort dubbla snedstreck om de finns (t.ex. //n)
      str_replace_all("//n", " ") %>%
      # ta bort extra mellanslag i början/slutet och reducerar flera mellanslag till ett
      str_squish()
  ) %>%
  # Byt ut "SCB och Skolverket" mot "Skolverket och SCB"
  mutate(
    source = case_when(
      source == "SCB och Skolverket" ~ "Skolverket och SCB",
      TRUE ~ source
    )
  ) %>%
  # Ta bort hjälpkolumner 
  select(-source_try1, -source_try2, -source_raw)


#    ####

# 3. Lägg till texter som beskrivngarna är baserade på ####

# 3.1. Hantera Tema och deras källor
# Samla unika titlar per Tema från final_df
tema_titlar <- final_df %>%
  filter(!is.na(Tema)) %>%
  distinct(Tema, title, municipality_type, gender) %>% 
  group_by(Tema, municipality_type, gender) %>%
  summarise(
    titles_text = str_c(unique(title), collapse = ", "),
    .groups = "drop" # Stoppa grupperingen
  )

# Gör om tabellen från long till wide
tema_titlar_wide <- tema_titlar %>%
  tidyr::pivot_wider(names_from = c(municipality_type, gender), values_from = titles_text, names_prefix = "text_") 


# Samla unika källor per Tema från final_df
tema_source <- final_df %>%
  filter(!is.na(Tema)) %>%
  distinct(Tema, source, municipality_type, gender) %>% 
  group_by(Tema, municipality_type, gender) %>%
  summarise(
    source_text = str_c(unique(source), collapse = ", "),
    .groups = "drop" # Stoppa grupperingen
  )

# Gör om tabellen från long till wide
tema_source_wide <- tema_source %>%
  tidyr::pivot_wider(names_from = c(municipality_type, gender), values_from = source_text, names_prefix = "source_") 

# Sätt ihop titlar och källor (wide)
tema_wide_final <- tema_titlar_wide %>%
  left_join(tema_source_wide, by = "Tema")

# 3.2. Hantera delarna och deras källor
# Samla unika delar och källor från final_df (de specifika indikatorerna behövs ej)
del_titlar <- final_df %>%
  filter(!is.na(Del)) %>%
  distinct(Del, source, municipality_type, gender) %>% 
  group_by(Del, municipality_type, gender) %>%
  summarise(
    source_text = str_c(unique(source), collapse = ", "),
    .groups = "drop" # Stoppa grupperingen
  )

# Gör om tabellen från long till wide
del_titlar_wide <- del_titlar %>%
  tidyr::pivot_wider(names_from = c(municipality_type, gender), values_from = source_text, , names_prefix = "source_")

# Döp om Del --> Tema (för att kunna joina tabellerna)
del_wide_final <- del_titlar_wide %>%
  rename_at('Del', ~'Tema')

# 3.3. Sätt ihop Tema med Del
wide_final <- tema_wide_final %>% 
  full_join(del_wide_final, by = "Tema")

wide_final <- bind_rows(tema_wide_final, del_wide_final) %>%
  group_by(Tema) %>%
  summarise(
    across(
      everything(),          # Välj alla kolumner i datan (efter group_by) bortsett från själva gruppkolumnen
      ~ first(.[!is.na(.)]), # Välj första icke-NA-värdet i kolumnen
      .names = "{.col}"      # Behåll samma kolumnnamn
      ),
    .groups = "drop"         # Ta bort gruppering i resultatet
    ) %>% 
  mutate(sorting = case_when(Tema == "Livskvalitet" ~ "100",
                             Tema == "Hållbarhet" ~ "200"))
  

# 3.4. Skapa beskrivningarna
# Förbered fast text
tema_text <- wide_final %>%
  filter(!is.na(Tema)) %>%
  distinct(Tema) %>% 
  mutate(geografi_kommun = "Kommun") %>%
  mutate(geografi_region = "Region") %>%
  mutate(geografi_kommun_index = "Kommunindex") %>%
  mutate(geografi_region_index = "Regionindex") %>%
  mutate(geografi_kommun_index_tema = "Kommunindex för tema") %>%
  mutate(geografi_region_index_tema = "Regionindex för tema") %>%
  mutate(konsuppdelning_man = "Mäns") %>%
  mutate(konsuppdelning_kvinnor = "Kvinnors") %>%
  mutate(livskvalitet_desc = "är en sammanvägning av samtliga ingående teman som mäter livskvalitet. Ingående indikatorer normaliseras så att samtliga regioners värden placeras på en skala från 0 till 100 där 0 är sämst och 100 är bäst (för vissa indikatorer används inverterad skala). I nästa steg vägs de standardiserade indikatorvärdena samman till index på aspektnivå. Detta görs med medelvärden, samtliga indikatorer vägs samman med samma vikt inom respektive aspekt. Värdena hamnar även på denna nivå i intervallet 0 till 100. Därefter vägs indexet på aspektnivå ihop till temanivå enligt samma princip och även dessa värden hamnar mellan 0 och 100. Slutligen vägs värdet för samtliga teman ihop enligt samma princip, med samma vikt, till ett sammantaget index för livskvalitet.") %>%
  mutate(hållbarhet_desc = "är en sammanvägning av samtliga ingående teman som mäter hållbarhet. Ingående indikatorer normaliseras så att samtliga regioners värden placeras på en skala från 0 till 100 där 0 är sämst och 100 är bäst (för vissa indikatorer används inverterad skala). I nästa steg vägs de standardiserade indikatorvärdena samman till index på aspektnivå. Detta görs med medelvärden, samtliga indikatorer vägs samman med samma vikt inom respektive aspekt. Värdena hamnar även på denna nivå i intervallet 0 till 100. Därefter vägs indexet på aspektnivå ihop till temanivå enligt samma princip och även dessa värden hamnar mellan 0 och 100. Slutligen vägs värdet för samtliga teman ihop enligt samma princip, med samma vikt, till ett sammantaget index för hållbarhet.") %>%
  mutate(baseras = "baseras på indikatorerna") %>%
  mutate(nyckeltal_desc = "Nyckeltalen normaliseras så att samtliga regioners värden placeras på en skala från 0 till 100 där 0 är sämst och 100 är bäst (för vissa indikatorer används inverterad skala). I nästa steg vägs de normaliserade indikatorvärdena samman till index på aspektnivå. Detta görs med medelvärden, samtliga indikatorer vägs samman med samma vikt inom respektive aspekt. Därefter vägs indexet på aspektnivå ihop till temanivå enligt samma princip och även dessa värden hamnar mellan 0 och 100. Viktningen är lika stor för samtliga aspekter inom temat.") %>%
  mutate(källa = "Källa: ")

# 3.5. Kombinera allt i wide
tema_text_wide_final <- wide_final %>%
  left_join(tema_text, by="Tema")

#    ####

# 4. Skapa en färdig tabell med beskrivningar ####
# 4.1. Sätt ihop till tabell med en kolumn för tema (plus del) och sex kolumner för distinkta beskrivningar
BRPplus_tema_desc <- tema_text_wide_final %>%
  mutate(
    # Bestäm vilken geografi_index som ska användas baserat på tema
    geografi_region_index_vald = ifelse(Tema %in% c("Hållbarhet", "Livskvalitet"), 
                                        geografi_region_index, 
                                        geografi_region_index_tema),
    
    geografi_kommun_index_vald = ifelse(Tema %in% c("Hållbarhet", "Livskvalitet"), 
                                        geografi_kommun_index, 
                                        geografi_kommun_index_tema),
    
    # Avgör vilken källa som ska användas
    kalla_att_anvanda = case_when(
      !is.na(källa) & källa != "" & källa != "Källa: " ~ källa,
      TRUE ~ NA_character_
    ),
    
    # Bygg regionindex-beskrivningar för kön = totalt
    Information_regionindex_totalt = case_when(
      # För Hållbarhet och Livskvalitet används hållbarhet_desc och livskvalitet_desc
      Tema %in% c("Hållbarhet", "Livskvalitet") ~ 
        paste(geografi_region_index_vald, Tema, 
              ifelse(Tema == "Hållbarhet", hållbarhet_desc, livskvalitet_desc),
              "Källa:",
              ifelse(!is.na(kalla_att_anvanda), kalla_att_anvanda, source_L_T),
              sep = " "),
      # För andra teman används text_L_... och nyckeltal_desc
      TRUE ~ 
        paste(geografi_region_index_vald, Tema, "baseras på indikatorerna", text_L_T, nyckeltal_desc,
              "Källa:",
              ifelse(!is.na(kalla_att_anvanda), kalla_att_anvanda, source_L_T),
              sep = " ")
    ),
    
    # Bygg regionindex-beskrivningar för kön = kvinnor
    Information_regionindex_kvinnor = case_when(
      # För Hållbarhet och Livskvalitet används hållbarhet_desc och livskvalitet_desc
      Tema %in% c("Hållbarhet", "Livskvalitet") ~ 
        paste("Kvinnors", geografi_region_index_vald, Tema, 
              ifelse(Tema == "Hållbarhet", hållbarhet_desc, livskvalitet_desc),
              "Källa:",
              ifelse(!is.na(kalla_att_anvanda), kalla_att_anvanda, source_L_K),
              sep = " "),
      # För andra teman används text_L_... och nyckeltal_desc
      TRUE ~ 
        paste("Kvinnors", geografi_region_index_vald, Tema, "baseras på indikatorerna", text_L_K, nyckeltal_desc,
              "Källa:",
              ifelse(!is.na(kalla_att_anvanda), kalla_att_anvanda, source_L_K),
              sep = " ")
    ),
    
    # Bygg regionindex-beskrivningar för kön = män
    Information_regionindex_män = case_when(
      # För Hållbarhet och Livskvalitet används hållbarhet_desc och livskvalitet_desc
      Tema %in% c("Hållbarhet", "Livskvalitet") ~ 
        paste("Mäns", geografi_region_index_vald, Tema, 
              ifelse(Tema == "Hållbarhet", hållbarhet_desc, livskvalitet_desc),
              "Källa:",
              ifelse(!is.na(kalla_att_anvanda), kalla_att_anvanda, source_L_M),
              sep = " "),
      # För andra teman används text_L_... och nyckeltal_desc
      TRUE ~ 
        paste("Mäns", geografi_region_index_vald, Tema, "baseras på indikatorerna", text_L_M, nyckeltal_desc,
              "Källa:",
              ifelse(!is.na(kalla_att_anvanda), kalla_att_anvanda, source_L_M),
              sep = " ")
    ),
    
    # Bygg kommunindex-beskrivningar för kön = totalt
    Information_kommunindex_totalt = case_when(
      # För Hållbarhet och Livskvalitet används hållbarhet_desc och livskvalitet_desc
      Tema %in% c("Hållbarhet", "Livskvalitet") ~ 
        paste(geografi_kommun_index_vald, Tema, 
              ifelse(Tema == "Hållbarhet", hållbarhet_desc, livskvalitet_desc),
              "Källa:",
              ifelse(!is.na(kalla_att_anvanda), kalla_att_anvanda, source_K_T),
              sep = " "),
      # För andra teman används text_K_... och nyckeltal_desc
      TRUE ~ 
        paste(geografi_kommun_index_vald, Tema, "baseras på indikatorerna", text_K_T, nyckeltal_desc,
              "Källa:",
              ifelse(!is.na(kalla_att_anvanda), kalla_att_anvanda, source_K_T),
              sep = " ")
    ),
    
    # Bygg kommunindex-beskrivningar för kön = kvinnor
    Information_kommunindex_kvinnor = case_when(
      # För Hållbarhet och Livskvalitet används hållbarhet_desc och livskvalitet_desc
      Tema %in% c("Hållbarhet", "Livskvalitet") ~ 
        paste("Kvinnors", geografi_kommun_index_vald, Tema, 
              ifelse(Tema == "Hållbarhet", hållbarhet_desc, livskvalitet_desc),
              "Källa:",
              ifelse(!is.na(kalla_att_anvanda), kalla_att_anvanda, source_K_K),
              sep = " "),
      # För andra teman används text_K_... och nyckeltal_desc
      TRUE ~ 
        paste("Kvinnors", geografi_kommun_index_vald, Tema, "baseras på indikatorerna", text_K_K, nyckeltal_desc,
              "Källa:",
              ifelse(!is.na(kalla_att_anvanda), kalla_att_anvanda, source_K_K),
              sep = " ")
    ),
    
    # Bygg kommunindex-beskrivningar för kön = män
    Information_kommunindex_män = case_when(
      # För Hållbarhet och Livskvalitet används hållbarhet_desc och livskvalitet_desc
      Tema %in% c("Hållbarhet", "Livskvalitet") ~ 
        paste("Mäns", geografi_kommun_index_vald, Tema, 
              ifelse(Tema == "Hållbarhet", hållbarhet_desc, livskvalitet_desc),
              "Källa:",
              ifelse(!is.na(kalla_att_anvanda), kalla_att_anvanda, source_K_M),
              sep = " "),
      # För andra teman används text_K_... och nyckeltal_desc
      TRUE ~ 
        paste("Mäns", geografi_kommun_index_vald, Tema, "baseras på indikatorerna", text_K_M, nyckeltal_desc,
              "Källa:",
              ifelse(!is.na(kalla_att_anvanda), kalla_att_anvanda, source_K_M),
              sep = " ")
    )
  ) %>%
  
  # Rensa texten från onödiga mellanslag och ta bort eventuella "NA" i texterna
  mutate(across(starts_with("Information"), 
                ~ str_squish(str_replace_all(.x, " NA | NA|NA |^NA$", "")))) %>%
  
  # Ta bort eventuella dubbletter av "Källa:"
  mutate(across(starts_with("Information"), 
                ~ str_replace_all(.x, "Källa: Källa:", "Källa:"))) %>%
  
  # Lägg till punkt i slutet av varje textsträng
  mutate(across(starts_with("Information"), 
                ~ ifelse(!is.na(.x) & .x != "" & !str_detect(.x, "\\.$"), 
                         paste0(.x, "."), 
                         .x))) %>%
  
  # Välj bara de kolumner vi behöver
  select(Tema, starts_with("Information"))



# 4.2. Kontrollera 
# Kontrollera hela tabellen
print(BRPplus_tema_desc)

# Stickprov
BRPplus_tema_desc$Information_regionindex_totalt[5]

# 4.3. Lägg på kolumn 'sorting'  
#      (Denna del kan tas bort genom att ha med kolumnen redan i BRPplus_data_base.csv)

# Skapa en dataframe med Tema och sorting för att sortera tabellen
sorting_df <- data.frame(
  Tema = c(
    "T1 - Medborgarengagemang och demokratisk delaktighet",
    "T2 - Trygghet och säkerhet",
    "T3 - Miljökvalitet",
    "T4 - Tillgänglighet till tjänster",
    "T5 - Inkomst och förmögenhet",
    "T6 - Arbete och löner",
    "T7 - Bostad",
    "T9 - Hälsa",
    "T10 - Utbildning och kompetens",
    "T11 - Sociala relationer och tillit",
    "T12 - Individuellt välbefinnande",
    "Ekonomi",
    "Miljö",
    "Social",
    "Livskvalitet",
    "Hållbarhet"
  ),
  sorting = c(101, 102, 103, 104, 105, 106, 107, 109, 110, 111, 112, 201, 202, 203, 100, 200)
)

# Lägg till sorting och ändra kolumnordning så sorting blir först
BRPplus_tema_desc <- BRPplus_tema_desc %>%
  left_join(sorting_df, by = "Tema") %>%
  select(sorting, everything()) %>%
  arrange(sorting)

#    ####

# 5. Export ####
# Exportera filen
csv_path <- here::here("data", "BRPplus_tema_desc.csv")

write.csv2(
  BRPplus_tema_desc,
  file = csv_path,
  row.names = FALSE
)

#    ####

