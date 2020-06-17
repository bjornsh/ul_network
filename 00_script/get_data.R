#---------------------------------------------------------------------------------------------------
# Syfte
#---------------------------------------------------------------------------------------------------

# The script gathers coordinates for public transport stations and adds geometadata for each stop. 
# Data stores as sf object and dataframe for use in analyses. Filename includes timetableperiod (summer, autumn, winter) 
# to enable tracking of changes in network over time.


#---------------------------------------------------------------------------------------------------
# Städa
#---------------------------------------------------------------------------------------------------
rm(list = ls())
invisible(gc())



#---------------------------------------------------------------------------------------------------
# Libraries
#---------------------------------------------------------------------------------------------------

library(jsonlite)
library(dplyr)
library(sf)
library(mapview)

options(scipen=999)


#-------------------------------------------------
# Hämta data
#-------------------------------------------------

#### SCB tätort shapefil (58.0 MB)
download.file("https://www.scb.se/contentassets/3ee03ca6db1e48ff808b3c8d2c87d470/to2018_swe99tm_arcview.zip",
              destfile= "01_input_data/shp/to2018_swe99tm_arcview.zip")

unzip("01_input_data/shp/to2018_swe99tm_arcview.zip", exdir="01_input_data/shp", overwrite=TRUE)


#### kommun shapefil (zip inom zip file, 484 KB)
download.file("https://www.scb.se/contentassets/3443fea3fa6640f7a57ea15d9a372d33/shape_svenska.zip",
              destfile= "01_input_data/shp/shape_svenska.zip")

# unzip("01_input_data/shp/shape_svenska.zip", 
#       exdir="01_input_data/shp", overwrite=TRUE)

unzip("01_input_data/shp/KommunSweref99TM.zip", 
      exdir="01_input_data/shp", overwrite=TRUE)

unzip("01_input_data/shp/LanSweref99TM.zip", 
      exdir="01_input_data/shp", overwrite=TRUE)


kommun = st_read("01_input_data/shp/Kommun_Sweref99TM_region.shp", options = "ENCODING=WINDOWS-1252")
tatort = st_read("01_input_data/shp/To2018_Swe99TM.shp", options = "ENCODING=WINDOWS-1252")
lan = st_read("01_input_data/shp/Lan_Sweref99TM_region.shp", options = "ENCODING=WINDOWS-1252")



## säkerställ CRS = 3006
kommun_3006 = st_transform(kommun, crs = 3006) 
tatort_3006 = st_transform(tatort, crs = 3006)
lan_3006 = st_transform(lan, crs = 3006)

#### import API hållplats data för UL linjer 1 till 999 
baseurl = "https://api.ul.se/api/v3/line/"
linje = list()

for(i in 1:999){ 
  ul = fromJSON(paste0(baseurl, i), flatten=TRUE)
  linje[[i+1]] <- cbind(ul$name, ul$pointsOnRoute)
}

alla_linjer <- rbind_pages(linje)

colnames(alla_linjer) = c("linje", "hpl_id", "hpl_namn", "area", "lat", "long")

## hpl ID och koordinater (WGS84)
hpl_id_koord = alla_linjer %>% 
  dplyr::select(hpl_id, lat, long) %>% 
  arrange(hpl_id) %>%
  group_by(hpl_id) %>%
  filter(row_number()==1)


## alla linjer som trafikera en hpl
hpl_linje = alla_linjer %>% 
  group_by(hpl_id) %>% 
  mutate(linje = paste0(linje, collapse = ", ")) %>%
  distinct(hpl_id, linje) %>%
  select(hpl_id, linje)

## slå ihopp
hpl_id_koord_linje = hpl_id_koord %>% 
  left_join(., hpl_linje, by = "hpl_id") %>% 
  ungroup() # för att sf fungera


datum = substr(Sys.time(), 1, 10)
termin = ifelse(datum >= "2020-01-01" & datum <= "2020-06-30", "winter",
                       ifelse(datum >= "2020-07-01" & datum <= "2020-09-30", "summer", 
                                     "autumn"))

write.csv2(hpl_id_koord_linje, paste0("01_input_data/hpl_id_koord_linje_", termin, "_", datum, ".csv"), row.names = F)


#-------------------------------------------------
# lägg till geometadata
#-------------------------------------------------

## skapa sf object från koordinat df 
hpl_sf = sf::st_as_sf(hpl_id_koord_linje, coords = c("long", "lat"))
class(hpl_sf)
st_crs(hpl_sf)
st_crs(hpl_sf) = 4326 # assign crs eftersom sf objekt har inget
st_crs(hpl_sf)

hpl_sf_sweref = st_transform(hpl_sf, crs = 3006) # transform från WGS84 till Sweref99 TM
st_crs(hpl_sf_sweref)


## extrahera sweref koordinater från objekt
sweref_koordinat = do.call(rbind, st_geometry(hpl_sf_sweref)) %>% 
  as_tibble() %>% setNames(c("x_sweref99tm","y_sweref99tm"))


## lägg till geometadata till sf object
hpl_metadata_sf = st_join(hpl_sf_sweref, lan_3006, join = st_within) %>%
  st_join(., kommun_3006, join = st_within) %>%
  st_join(., tatort_3006, join = st_within) %>%
  dplyr::select(-OBJECTID, -AR, - LANSKOD, -AREA_HA, -KOMMUNKOD, -KOMMUNNAMN, -LAN_NAMN, -X_koord, -Y_koord) %>% 
  left_join(., hpl_id_koord_linje[,c("hpl_id", "lat", "long")], by = "hpl_id") %>%
  rename(y_wgs84 = lat,  x_wgs84 = long,
         lan_kod = LnKod, lan = LnNamn, 
         kommun_kod = KnKod, kommun = KnNamn, 
         tatort_pop = BEF, tatort_kod = TATORTSKOD, tatort = TATORT) %>%
  cbind(., sweref_koordinat)

## skapa df med sd objekt data
hpl_metadata_df = hpl_metadata_sf %>% as.data.frame() %>% dplyr::select(-geometry)


## karta
mapview(hpl_metadata_sf)


## spara sf objekt som esri shapefil och df som .csv 
st_write(hpl_metadata_sf, paste0("02_output_data/hpl_metadata_sf_", termin, "_", datum, ".shp"))
write.csv2(hpl_metadata_df, paste0("02_output_data/hpl_metadata_df_", termin, "_", datum, ".csv"),  row.names = FALSE)




