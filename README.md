--- OBS: WORK IN PROGRESS ---

# BRP+ dokumentation
*Eric Klingener*


## 1. Introduktion

BRP+ är ett mätverktyg vars index har som målsättning att jämföra individers upplevda livskvalitet i hela Sverige, på kommun-, region- och riksnivå. Ambitionen är att gå ifrån de traditionella ekonomiska måtten såsom BRP/BNP mot mjukare självupplevda värden på aggregerad individnivå. Indexet är särskilt användbart för att jämföra regioner och kommuner sinsemellan. 

Arbetet med BRP+ påbörjades 2014 av Tillväxtverket, SKR och en handfull regionala analytiker. Sedan 2023 är Tillväxtverket systemägare och förvaltar samt sprider BRP+ tillsammans med en arbetsgrupp bestående av Region Skåne, Region Sörmland, Region Dalarna och Region Gävleborg. BRP+ publiceras årligen på <www.kolada.se> (RKA – Rådet för främjande av kommunala analyser). Sedan 2025 är källkoden öppen på [det regionala] Analytikernätverkets GitHub-sida. 


## 2. Indexets uppbyggnad

BRP+ bygger på en mängd indikatorer. Majoriteten av indikatorerna är könsuppdelade enligt totalt, män och kvinnor. Indikatorerna är standardiserade efter geografi (kommun och region). De är sedan vägda samman inom ett 30-tal aspekter, som är vägda samman i 15 teman, som är vägda samman i två delar. Livskvalitet representerar nuvarande livskvalitet, hållbarhet representerar framtida potentiell livskvalitet. Oftast är livskvalitet indexet i fokus. Det finns inga justerande vikter i modellen. Indexet uppdateras årligen runt påsk. 

Källkoden för BRP+ bygger på fem underliggande datafiler varav fyra är konstanta och en uppdateras årligen. 

### Årligen uppdaterade filer
- **start_BRP_plus_indikatorer_tbl.csv:** Tabell med alla indikatorer som används i koden under innevarande år. Tillväxtverket ansvarar för att det görs en översyn av denna tabell inför varje årlig uppdatering av indexet. Äldre versioner av denna tabell sparas separat med suffixet ”_YYYY.csv” i mappen /archive på GitHub. 

### Permanenta filer
- **start_riket_kodnyckel_tbl.csv:** Kodnyckel för riket. En av tre kodnycklar med geografiska namn och numreringar, både enligt SCB och Koladas standard. Uppdaterad år 2021. 
- **start_region_kodnyckel_tbl.csv:** Kodnyckel för de 21 regionerna. En av tre kodnycklar med geografiska namn och numreringar, både enligt SCB och Koladas standard. Uppdaterad 2021.
- **start_kommun_kodnyckel_tbl.csv:** Kodnyckel för de 290 regionerna. En av tre kodnycklar med geografiska namn och numreringar, både enligt SCB och Koladas standard. Uppdaterad 2021.
- **start_BRP_plus_kategorisering_tbl.csv:** Överblick av kategoriseringen i BRP+ enligt indikatorer, aspekter, teman och delar. Uppdaterad 2025. 


## 3. Användning av källkoden för BRP+

Källkoden för BRP+ har en arkitektur som ska underlätta vidareutveckling. De tre första koderna i projektet är grundläggande och standard. 

### 01_get_BRP_plus.R
Den första R-koden i projektet använder startfilerna samt R-paketet rKolada (som bygger på Koladas API) för att hämta all data för alla indikatorer. Vissa av dessa indikatorer beräknas av Tillväxtverket men levereras för till Kolada vilket möjliggör en så kallad one-stop-shop. Koden tar ca 40 minuter att köra och avslutas med att grunddatat exporteras som en zippad csv-fil, BRP_plus_data_base.zip. 

### 02_stand_BRP_plus_maxmin.R
Den andra koden i projektet skapar standardiseringar av alla indikatorer och beräknar index för alla aspekter, teman och delar på kommun-, region- och riksnivå. Detta baseras på den zippade csv-filen BRP_plus_data_base.zip. Denna modell har varit standard sedan BRP+ utvecklades första gången. Sedan publicering av ”stämpelår 2025” (2026-05-04) är det möjligt att anpassa könsparametern referenspunkt, något som efterfrågats av regioner för att kunna göra andra jämförelser mellan män och kvinnor i indexet. Dessutom kan man lägga till ett laggat år, vilket dock bör göras med försiktighet. 

### 03_deliverable_desc_BRPplus.R
Den tredje koden i projektet skapar dynamiska beskrivningar av de olika kombinationerna av underindex som beräknas utifrån tema, geografi och kön. Dessa levereras årligen till Kolada tillsammans med uppdaterade indexsiffror och publiceras på Koladas hemsida med indexsiffrorna. 

### (04_stand_BRP_plus_PLS_SEM.R)
De tre första koderna i projektet består av de etablerade modellerna. Detta är en kommande vidareutveckling av BRP+ i form av en alternativ standardiseringsmodell enligt PLS-SEM (*partial least squares - structural equation modeling*). Fördelen med denna modellen är att den automatiskt viktar alla indikatorer dynamiskt; i standardmodellen ”maxmin” används samma vikt för alla indikatorer. 

### Flödesschema: 
<img width="734" height="803" alt="Image" src="https://github.com/user-attachments/assets/12324a3d-11d1-4111-b6f1-e5c7125fc2ef" />


## 4. Anpassad användning av källkoden för BRP+ (maxmin-beräkning)

Under hösten 2025 gjordes ett förbättringsarbete av koden för standardmodellen för BRP+, den så kallade maxmin-beräkningen. Förbättringsarbetet innebar implementering av alternativa referenspunkter i beräkningsmodellen. Användare kan nu enkelt justera referenspunkten för kön och år direkt i koden. 

### Kön: Justering av referenspunkt
I sin standardberäkning utgår alla index från den sammanlagda könsparametern (`kön == ”total”`). Det innebär att alla standardiseringar där könsparametern inte är den sammanlagda (i.e., `kön == ”total”`) dock utgår från denne. Detta kan dock korrigeras enkelt direkt i källkoden. 

**a) total_bas [default]**
Kön ”totalt” är referenspunkt i standardiseringen. Värdena för män och kvinnor är jämförbara mot totalvärdet. 
```
kon == "T"
kon == "M" | kon == "K"
```

**b) separat_bas**
Varje kön, inklusive totalt, har sin egen bas i standardiseringen. Värdena för totalt, män och kvinnor är jämförbara inom sina respektive könsgrupper. 
```
kon == "T"
kon == "M"
kon == "K"
```

**c) sammantaget_bas**
Alla värden, oberoende av kön, bidrar till standardiseringen. Värdena för totalt, män och kvinnor är fritt jämförbara mellan sina könsgrupper. 
```
kon == "T" | kon == "M" | kon == "K"
```

### Lagg: Justering av referenspunkt
Alla index utgår dessutom från det senaste året med tillgänglig data (`max(year)`), vill man lägga till ett laggat år som utgår från senaste året kan man också göra det direkt i källkoden. 

**a) latest_year [default]**
Bara värden som är tillgängliga i varje separat indikatorns senaste år används som i standardiseringen. 
```
max(years)
```

**b) latest_year_lag**
Vid aktivering av lagg väljs, för varje indikator (KPI), ett år som ligger cirka X år (t.ex. 5 år) före indikatorns senaste tillgängliga år. Om det inte finns något år som ligger exakt eller minst X år bakåt väljs det år som ligger närmast denna gräns. Beräkningen utgår alltid från indikatorns senaste år, `max(years)`, som fungerar som referenspunkt för laggen.
```
ifelse(
  any(years <= max(years) - lag), # Finns det minst ett år som är ≤ (senaste år − lag)?
  max(years[years <= max(years) - lag]), # Om ja → välj det senaste av dessa år
  years[which.min(abs(years - (max(years) - lag)))] # Om nej → välj året närmast max(years) - lag 
)
```


