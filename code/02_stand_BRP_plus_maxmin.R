
# Import av BRP_plus_brpplus_data_base_base ####
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, rKolada, readxl, pbapply, here, conflicted) # Ladda paket, eller installera dem om de inte finns och ladda dem sen
conflicted::conflict_prefer("filter", "dplyr") # Om flera paket har funktionen "filter()" använd alltid dplyr-paketet som sandard för "filter()" 
conflicted::conflict_prefer("lag", "dplyr") # Om flera paket har funktionen "lag()" använd alltid dplyr-paketet som sandard för "lag()" 

# Om nödvändigt, rensa Environment
 rm(list=ls())

# Hitta mapp där hela RProject är sparat och skapa path för /brpplus_data_base ZIP-filen med brpplus_data_base_base
zip_path  <- here::here("brpplus_data_base", "BRP_plus_data_base.zip")

# One-block: unzip to temp, read CSV, remove temp automatically
brpplus_data_base <- {
                      tmp_dir <- tempdir() # create a temporary folder
                      unzip(zip_path, exdir = tmp_dir) # unzip the CSV to the temp folder
                      csv_file <- list.files(tmp_dir, pattern = "\\.csv$", full.names = TRUE) # find the CSV file (assumes only one CSV in the ZIP)
                      data <- read.csv2(csv_file, stringsAsFactors = FALSE) # read CSV
                      data # return the data
                      }
# Ta bort brpplus_data_baseframe 'data'
rm(data)

# check
head(brpplus_data_base)


################################################################################
### Standardisering enligt "max-min-metoden"
# Villkor: min != max + Omvänd skala
#
# Standardiseringen görs inom enskilda indikatorer och inom regioner ELLER kommuner. 
#
# Senaste året används alltid som bas, i.e., max(year), men man kan lägga till ett laggat år.
# Det laggade året också använder max(year) som bas. 
# Default-värde på laggat år är -5, om detta ska ändras görs det i anropet. 
#
#
# Det finns tre sätt att hantera könsuppdelningen: 
#
# a) total_bas 
#     Basen som används är allid "Kön = Totalt" 
#     Möjliggör jämförelse mellan könen inom kommunerna eller regionerna. (Standard!)
#
# b) separat_bas
#     Basen som används är separat för "Kön = Totalt", "Kön = Män" eller "Kön = Kvinnor". 
#     Möjliggör jämförelse inom könen mellan kommunerna eller regionerna. 
#
# c) sammantaget_bas 
#     Basen som används är "Kön = ALL" över alla kön tillsammans. 
#     Möjliggör jämförelse mellan kommunerna eller regionerna oberoende av kön. 

################################################################################
standardisera_maxmin <- function(brpplus_data_base,
                                 gender_mode = c("total_bas",
                                                 "separat_bas",
                                                 "sammantaget_bas"),
                                 year_mode   = c("latest_year",
                                                 "latest_year_lag"),
                                 lag = 5, # <-- antal laggade år (ändras i anropet vid behov)
                                 validate = TRUE) {
  
  library(dplyr)
  
  gender_mode <- match.arg(gender_mode)
  year_mode   <- match.arg(year_mode)
  
  # ============================
  # KPI-specifik årshantering (robust och varningsfri)
  # ============================
  years_per_kpi <- brpplus_data_base %>%
    group_by(kpi) %>%
    summarise(years = list(year), .groups = "drop") %>%
    rowwise() %>%
    mutate(
      latest_year = max(unlist(years), na.rm = TRUE),
      target_year = latest_year - lag,
      selected_year_per_kpi = case_when(
        year_mode == "latest_year" ~ latest_year,
        year_mode == "latest_year_lag" ~ {
          yrs <- unlist(years)
          lower_yrs <- yrs[yrs <= target_year]
          if(length(lower_yrs) > 0) {
            max(lower_yrs)
          } else {
            # ingen finns ≤ target_year → välj närmaste
            yrs[which.min(abs(yrs - target_year))]
          }
        }
      )
    ) %>%
    ungroup() %>%
    select(kpi, selected_year_per_kpi)
  
  # ============================
  # Bas-data: ALLTID latest_year (per KPI)
  # ============================
  base_data <- brpplus_data_base %>%
    left_join(years_per_kpi %>% 
                rename(latest_year_per_kpi = selected_year_per_kpi),
              by = "kpi") %>%
    group_by(kpi) %>%
    mutate(latest_year_per_kpi = max(year, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(year == latest_year_per_kpi)
  
  
  # ============================
  # Slå ihop årsinformation med datan och filtrera
  # ============================
  data2 <- brpplus_data_base %>%
    left_join(years_per_kpi, by = "kpi") %>%
    filter(year == selected_year_per_kpi)
  
  # ============================
  # Validering (saknade kön, logg)
  # ============================
  if(validate){
    message("=== Validering pågår ===")
    
    missing_gender <- data2 %>%
      group_by(kpi, municipality_type, year) %>%
      summarise(
        missing_genders = list(setdiff(c("T","K","M"), gender)),
        .groups = "drop"
      ) %>%
      filter(lengths(missing_genders) > 0)
    
    if(nrow(missing_gender) > 0){
      message("Obs! Saknade kön (per KPI:s valda år):")
      print(missing_gender)
    } else {
      message("Alla kön finns för respektive KPI:s valda år")
    }
  }
  
  # ============================
  # Standardisering
  # ============================
  if (gender_mode == "total_bas") {
    ref_table <- base_data %>%
      filter(gender == "T") %>%
      group_by(kpi, municipality_type, Omvand_skala) %>%
      summarise(
        min_ref = min(value, na.rm = TRUE),
        max_ref = max(value, na.rm = TRUE),
        .groups = "drop"
      )
    
    result <- data2 %>%
      left_join(ref_table, by = c("kpi", "municipality_type", "Omvand_skala")) %>%
      mutate(standard_value = case_when(
        Omvand_skala == 1 & min_ref != max_ref ~
          100 * ((max_ref - value) / (max_ref - min_ref)),
        Omvand_skala == 0 & min_ref != max_ref ~
          100 * ((value - min_ref) / (max_ref - min_ref)),
        TRUE ~ 0
      )) %>%
      select(-min_ref, -max_ref)
    
    return(result)
    
  } else if (gender_mode == "separat_bas") {
    ref_table <- base_data %>%
      group_by(kpi, municipality_type, Omvand_skala, gender) %>%
      summarise(
        min_ref = min(value, na.rm = TRUE),
        max_ref = max(value, na.rm = TRUE),
        .groups = "drop"
      )
    
    result <- data2 %>%
      left_join(ref_table,
                by = c("kpi", "municipality_type", "Omvand_skala","gender")) %>%
      mutate(standard_value = case_when(
        Omvand_skala == 1 & min_ref != max_ref ~
          100 * ((max_ref - value) / (max_ref - min_ref)),
        Omvand_skala == 0 & min_ref != max_ref ~
          100 * ((value - min_ref) / (max_ref - min_ref)),
        TRUE ~ 0
      )) %>%
      select(-min_ref, -max_ref)
    
    return(result)
    
  } else if (gender_mode == "sammantaget_bas") {
    ref_table <- base_data %>%
      group_by(kpi, municipality_type, Omvand_skala) %>%
      summarise(
        min_ref = min(value, na.rm = TRUE),
        max_ref = max(value, na.rm = TRUE),
        .groups = "drop"
      )
    
    result <- data2 %>%
      left_join(ref_table, by = c("kpi", "municipality_type", "Omvand_skala")) %>%
      mutate(standard_value = case_when(
        Omvand_skala == 1 & min_ref != max_ref ~
          100 * ((max_ref - value) / (max_ref - min_ref)),
        Omvand_skala == 0 & min_ref != max_ref ~
          100 * ((value - min_ref) / (max_ref - min_ref)),
        TRUE ~ 0
      )) %>%
      select(-min_ref, -max_ref)
    
    return(result)
  }
}
# ####

#### Skapa max-min-standardisering ####
# Standardisering + validering för senaste året
brpplus_stand_maxmin <- standardisera_maxmin(
  brpplus_data_base,
  gender_mode = "total_bas",
  year_mode   = "latest_year",
  validate    = TRUE
)


#### Anrop till max-min-standardisering för laggat år ####
# Standardisering + validering för senaste året minus laggade år (default lag = 5)
brpplus_stand_maxmin_lag <- standardisera_maxmin(
  brpplus_data_base,
  gender_mode = "total_bas",
  year_mode   = "latest_year_lag",
  lag         = 5,
  validate    = TRUE
)




### kontrollera så omvands_skala fortf är unik för varje kpi
# Detta anrop ska resultera i en tom tabell
test <- brpplus_stand_maxmin %>% group_by(kpi, Omvand_skala) %>% tally %>% 
  pivot_wider(names_from = Omvand_skala, values_from=n) %>% drop_na

brpplus_stand_maxmin %>% 
  group_by(kpi_text, municipality_type, gender, year, Omvand_skala) %>% 
  summarise(min=min(standard_value), 
            max=max(standard_value),
            min_value = min(value), 
            max_value = max(value)
  ) 

brpplus_stand_maxmin %>% summary

brpplus_stand_maxmin %>% glimpse



################################################################################
### Exportera till 2 filer för kommun & region
# Exporterat innehåll klistras in i flikarna
# "brpplus_data_baseunderlag_region" och "brpplus_data_baseunderlag_kommun" i filen "brpplus_data_base och index BRP+ 2022.xlsx"
################################################################################
### Skapa funktion för att exportera resultatbrpplus_data_base
exportera_brpplusbrpplus_data_base <- function(obj, 
                                  filnamnet) {
  obj %>%
    # filter(municipality!="Riket") %>% 
    
    # Skriv om "region" till "län"
    left_join(region_lan_nyckel) %>% 
    mutate(municipality = if_else(!is.na(region_lan), 
                                  region_lan, 
                                  municipality)) %>% 
    
    # skapa variabel "Region" med läns och kommun-nr
    left_join(municipality_id_nyckel) %>% 
    # byt namn
    rename(municipality_id_numerical = municipality_id, 
           municipality_id = m_id_text) %>% 
    mutate(municipality_id = if_else(is.na(municipality_id), 
                                     as.character(municipality_id_numerical), 
                                     municipality_id)) %>% 
    # variabeln Region
    mutate(Region = paste0(municipality_id , " ", municipality)) %>% 
    
    # Skriv om variabel gender
    mutate(gender = case_when(gender=="T"~"Totalt",
                              gender=="K"~"Kvinnor",
                              gender=="M"~"Män",
                              TRUE~gender)) %>% 
    # Byt namn på variabler som passar BRP Excelark
    select(Ar = year, 
           Kon = gender, 
           Varde = standard_value, 
           Indikator_name = kpi_text,
           Tema, 
           Aspekt,
           Del,
           kpi, 
           Omrade_typ = municipality_type, 
           Region,
           Kon_ford, 
           Log, 
           Lagar, 
           Maxar, 
           Omvand_skala
    ) %>% 
    
    ### Filtrera på kommun/län
    #filter(Omrade_typ == kommun_eller_lan)  %>% 
    
    ### Ordna kolumnerna på samma sätt som i excelarket där det ska klistras in
    relocate(Indikator_name, Number=kpi, Region, Kon, Ar, Varde, 
             Del, Tema, Aspekt, 
             Kon_ford, 
             Maxar, Lagar, 
             Log, 
             Omvand_skala,
             Omrade_typ) %>% 
    
    arrange(Indikator_name, Region, Kon, Ar) %>%   
    
    # Export till csv (sep=";"), Använd write.csv2 för option fileEncoding
    # write.csv2(file=filnamnet, fileEncoding="ISO-8859-1", row.names=FALSE)
    writexl::write_xlsx(filnamnet)
}


# Oklart varför detta steg görs...
  # Filtrerar fram alla rader där kön saknas (gender = NA)
  brpplus_stand_maxmin %>% filter(is.na(gender)) 
  # Ersätt värdet i municipality_type med "L" för de rader där
  # municipality_type är NA *och* municipality är "Riket"; 
  # annars behålls det ursprungliga värdet.
  brpplus_stand_maxmin <- brpplus_stand_maxmin %>% mutate(municipality_type = if_else(is.na(municipality_type) & municipality=="Riket", 
                                                                                      "L", municipality_type))

brpplus_stand_maxmin %>% exportera_brpplusbrpplus_data_base("brpplus_resultat.xlsx")

### Exportera län
# brpplus_stand_maxmin %>% export_brpplus_data_base_lan_kommun("L","brp_plus brpplus_data_baseunderlag_region.csv")
### Exportera kommun
# brpplus_stand_maxmin %>% export_brpplus_data_base_lan_kommun("K","brp_plus brpplus_data_baseunderlag_kommun.csv")



################################################################################
## kontrollera lite brpplus_data_base
################################################################################
# Alla kpi-texter från Burt är desamma i slutresultatet
check_df <- metabrpplus_data_base_kpi %>% distinct(kpi_text) %>% arrange(kpi_text) %>% rename(burt_kpi_text = kpi_text) %>% drop_na %>%  
  cbind(brpplus_brpplus_data_base %>% distinct(kpi, kpi_text) %>% arrange(kpi_text)  ) %>% 
  mutate(check= if_else(burt_kpi_text == kpi_text, 1, 0))

#check_df %>% view
check_df %>% glimpse
check_df %>% str
check_df %>% tally(check)


