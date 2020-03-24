
# setwd("~/Desktop/Git/edwinbet")

# =============================================================================
# Maps  
# =============================================================================

# -----------------------------------------------------------------------------
# Packages
# -----------------------------------------------------------------------------

source("eda_functions.R")
loadPkg(tidyverse)
loadPkg(geojsonio)
loadPkg(leaflet)

# =============================================================================
# Data Cleaning 
# =============================================================================

# Raw data 
raw_wine_reviews <- read.csv("data/winemag-data-130k-v2.csv")

# =============================================================================
# California 
# =============================================================================

# Counties 
county_list <- list(
  napa <- c("Napa Valley", "Rutherford", "Oakville",
   "St. Helena", "Howell Mountain", "Mount Veeder",
   "Stags Leap District", "Diamond Mountain District",
   "Calistoga", "Spring Mountain District", "Oak Knoll District"),
  sonoma <- c("Sonoma", "Russian River Valley", "Sonoma Coast", 
   "Sonoma County", "Carneros", "Dry Creek Valley", "Alexander Valley",
   "Sonoma Valley", "Green Valley", "Sonoma Mountain", "Knights Valley"),
  santa_barbara <- c("Santa Barbara County", "Sta. Rita Hills",
    "Happy Canyon of Santa Barbara"),
  san_luis_obispo <- c("San Luis Obispo", "Paso Robles", "Edna Valley",
    "Arroyo Grande Valley", "San Luis Obispo County", "Adelaida District"),
  monterey <- c("Santa Lucia Highlands", "Monterey", "Monterey County",
    "Arroyo Seco"),
  san_joaquin <- c("Lodi"),
  contra_coast <- c("Central Coast"),
  mendocino <- c("Anderson Valley", "Mendocino County", "Mendocino"),
  santa_clara <- c("Santa Cruz Mountains"),
  alameda <- c("Livermore Valley"),
  amador <- c("Sierra Foothills", "Amador County", "Shenandoah Valley (CA)"),
  lake <- c("North Coast", "Lake County"),
  el_dorado <- c("El Dorado"),
  riverside <- c("Temecula Valley")
 )

ca_wine_reviews <- raw_wine_reviews %>% 
  filter(province == "California") %>% 
  mutate(county = case_when(
    region_1 %in% county_list[[1]] ~ "Napa",
    region_1 %in% county_list[[2]] ~ "Sonoma",
    region_1 %in% county_list[[3]] ~ "Santa Barbara",
    region_1 %in% county_list[[4]] ~ "San Luis Obispo",
    region_1 %in% county_list[[5]] ~ "Monterey",
    region_1 %in% county_list[[6]] ~ "San Joaquin",
    region_1 %in% county_list[[7]] ~ "Contra Coast",
    region_1 %in% county_list[[8]] ~ "Mendocino",
    region_1 %in% county_list[[9]] ~ "Santa Clara",
    region_1 %in% county_list[[10]] ~ "Alameda",
    region_1 %in% county_list[[11]] ~ "Amador",
    region_1 %in% county_list[[12]] ~ "Lake",
    region_1 %in% county_list[[13]] ~ "El Dorado",
    region_1 %in% county_list[[14]] ~ "Riverside"))

# -----------------------------------------------------------------------------
# Prepare map data 
# -----------------------------------------------------------------------------

# Grab the geojson & json 
ca_geojson <- geojsonio::geojson_read("json/california-counties-2012.geojson", what = "sp")
ca_topo <- readLines("json/california_topography.json") %>% paste(collapse = "\n")

# Grab the counties from the geojson
ca_counties <- tibble(ca_geojson$name) %>% setNames("county")

# Merge in w/ wine reviews 
ca_county_count <- ca_wine_reviews %>% 
  group_by(county) %>% 
  summarise(count = n()) %>%
  right_join(ca_counties, by = "county") %>%
  mutate(count = ifelse(is.na(count), 0, count))

# -----------------------------------------------------------------------------
# Mapping function 
# -----------------------------------------------------------------------------

wine_heat <- function(location, count, topo, geojson){
  
  colors <- c('#eae8e8', '#9a005d', '#830048', '#690040', '#540037')
  pal <- colorNumeric(colors, NULL, reverse = F)
  
  heat_map <- leaflet(data=geojson, height=800, width=800) %>%
    addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.8,
                fillColor = ~pal(count),
                label = ~paste0(location, ": ", formatC(count, big.mark = ",")))  %>%
    addTopoJSON(topo, weight = 1, color = "#444444", fill = FALSE) %>%
    addLegend(pal = pal, values = count, opacity = 1.0, title = "Quantity")
  return(heat_map)
}

# -----------------------------------------------------------------------------
# Call function 
# -----------------------------------------------------------------------------

wine_heat(ca_county_count$county, ca_county_count$count, ca_topo, ca_geojson)

# =============================================================================
# France 
# =============================================================================

# Regions
fr_region_list <- list(
  Île_de_France = list(),              
  Centre_Val_de_Loire = list(),         
  Bourgogne_Franche_Comté = list("Burgundy", "Beaujolais"),    
  Normandie = list(),                  
  Hauts_de_France = list(),          
  Grand_Est = list("Alsace", "Champagne"),  
  Pays_de_la_Loire  = list("Loire Valley"),
  Bretagne = list(),                 
  Nouvelle_Aquitaine = list("Bordeaux", "Southwest France"),       
  Occitanie = list("Languedoc-Roussillon"),                 
  Auvergne_Rhone_Alpes = list("Rhône Valley"),     
  Provence_Alpes_Cote = list("Provence"), 
  Corse = list()        
)

fr_wine_reviews <- raw_wine_reviews %>% 
  filter(country == "France") %>% 
  mutate(region = case_when(
    province %in% fr_region_list[[1]] ~ "Île-de-France",
    province %in% fr_region_list[[2]] ~ "Centre-Val de Loire",
    province %in% fr_region_list[[3]] ~ "Bourgogne-Franche-Comté",
    province %in% fr_region_list[[4]] ~ "Normandie",
    province %in% fr_region_list[[5]] ~ "Hauts-de-France",
    province %in% fr_region_list[[6]] ~ "Grand Est",
    province %in% fr_region_list[[7]] ~ "Pays de la Loire",
    province %in% fr_region_list[[8]] ~ "Bretagne",
    province %in% fr_region_list[[9]] ~ "Nouvelle-Aquitaine",
    province %in% fr_region_list[[10]] ~ "Occitanie",
    province %in% fr_region_list[[11]] ~ "Auvergne-Rhône-Alpes",
    province %in% fr_region_list[[12]] ~ "Provence-Alpes-Côte d'Azur",
    province %in% fr_region_list[[13]] ~ "Corse"))

# -----------------------------------------------------------------------------
# Prepare map data 
# -----------------------------------------------------------------------------

# Grab the geojson & json (note that the file is named "provinces" but is capturing French "regions." Need to fix)
fr_geojson <- geojsonio::geojson_read("json/france_regions.geojson", what = "sp")
fr_topo <- readLines("json/fr_topo_regions.json") %>% paste(collapse = "\n")

# Grab the regions from the geojson
fr_region <- tibble(fr_geojson$nom) %>% setNames("region") 

# Merge in w/ wine reviews 
fr_region_count <- fr_wine_reviews %>% 
  filter(country == "France") %>% 
  group_by(region) %>% 
  summarise(count = n()) %>% 
  right_join(fr_region, by = "region") %>% 
  mutate(count = ifelse(is.na(count), 0, count))

# -----------------------------------------------------------------------------
# Call map function 
# -----------------------------------------------------------------------------


wine_heat(fr_region_count$region, fr_region_count$count, fr_topo, fr_geojson)

# =============================================================================
# Italy 
# =============================================================================

# Regions
# Greco di Tufo, Vino Spumante, Prosseco, Aglianico del Vulture grown in many places. 
# Choosing to select the first on the wikipedia page.

it_region_list <- list(
  Piemonte = list("Piemonte", "Barbera di Piemonte"),
  Valle_dAosta_Vallée_dAoste = list("Northwestern Italy"),
  Lombardia = list("Collio", "Franciacorta", "Vino Spumante"),
  Trentino_Alto_Adige_Südtirol = list("Alto Adige", "Trentino"),
  Veneto = list("Amarone della Valpolicella Classico", "Conegliano Valdobbiadene Prosecco Superiore",
                "Amarone della Valpolicella", "Valdobbiadene Prosecco Superiore", "Prosecco",
                "Veneto", "Soave Classico", "Soave",
                "Prosecco di Valdobbiadene"),
  Friuli_Venezia_Giulia	= list("Colli Orientali del Friuli", "Delle Venezie", "Venezia Giulia"),
  Liguria = list(),
  Emilia_Romagna = list(),
  Toscana	= list("Brunello di Montalcino", "Toscana", "Chianti Classico",
                 "Rosso di Montalcino", "Vino Nobile di Montepulciano", "Bolgheri",
                 "Morellino di Scansano", "Vernaccia di San Gimignano", "Chianti",
                 "Rosso di Montepulciano", "Maremma Toscana"),
  Umbria = list("Umbria"),
  Marche = list(),
  Lazio = list(),
  Abruzzo = list("Montepulciano d'Abruzzo"),
  Molise = list(),
  Campania = list("Lugana", "Taurasi", "Fiano di Avellino",
                  "Greco di Tufo", "Campania"),
  Puglia = list("Salento", "Puglia"),
  Basilicata = list("Aglianico del Vulture"),
  Calabria = list(),
  Sicilia = list("Sicilia", "Etna", "Terre Siciliane"),
  Sardegna = list())

it_wine_reviews <- raw_wine_reviews %>% 
  filter(country == "Italy") %>% 
  mutate(region = case_when(
    region_1 %in% it_region_list[[1]] ~ "Piemonte",
    region_1 %in% it_region_list[[2]] ~ "Valle d'Aosta/Vallée d'Aoste",
    region_1 %in% it_region_list[[3]] ~ "Lombardia",
    region_1 %in% it_region_list[[4]] ~ "Trentino-Alto Adige/Südtirol",
    region_1 %in% it_region_list[[5]] ~ "Veneto",
    region_1 %in% it_region_list[[6]] ~ "Friuli-Venezia Giulia",
    region_1 %in% it_region_list[[7]] ~ "Liguria",
    region_1 %in% it_region_list[[8]] ~ "Emilia-Romagna",
    region_1 %in% it_region_list[[9]] ~ "Toscana",
    region_1 %in% it_region_list[[10]] ~ "Umbria",
    region_1 %in% it_region_list[[11]] ~ "Marche",
    region_1 %in% it_region_list[[12]] ~ "Lazio",
    region_1 %in% it_region_list[[13]] ~ "Abruzzo",
    region_1 %in% it_region_list[[14]] ~ "Molise",
    region_1 %in% it_region_list[[15]] ~ "Campania",
    region_1 %in% it_region_list[[16]] ~ "Puglia",
    region_1 %in% it_region_list[[17]] ~ "Basilicata",
    region_1 %in% it_region_list[[18]] ~ "Calabria",
    region_1 %in% it_region_list[[19]] ~ "Sicilia",
    region_1 %in% it_region_list[[20]] ~ "Sardegna"))  

it_geojson <- geojsonio::geojson_read("json/italy_regions.geojson", what = "sp")
it_topo <- readLines("json/italy_regions_topo.json") %>% paste(collapse = "\n")

it_region <- tibble(it_geojson$reg_name) %>% setNames("region") 

it_region_count <- it_wine_reviews %>% 
  filter(country == "Italy") %>% 
  group_by(region) %>% 
  summarise(count = n()) %>% 
  right_join(it_region, by = "region") %>% 
  mutate(count = ifelse(is.na(count), 0, count))

wine_heat(it_region_count$region, it_region_count$count, it_topo, it_geojson)
