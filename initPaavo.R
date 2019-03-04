paavo <- readRDS(file=here::here("Data", "paavodata.rds"))

# Postinumerosta alueen nimi, otetaan kullekin uusin
paavo.nimet <- select(filter(paavo$data, pono.level == 5), nimi, pono, vuosi, kuntano) %>% 
  group_by(pono) %>% 
  arrange(desc(vuosi)) %>% 
  slice(1) %>% 
  select(-vuosi) %>%
  ungroup %>% 
  mutate()

# Postinumerosta alueen nimi
#map.pono2nimi <- function(v) {
#  plyr::mapvalues(
#      v,
#      paavo.nimet$pono,
#      paste(
#        paavo.nimet$data$pono,
#        plyr::mapvalues(
#          demografia$postinumero$data$kuntano,
#          geo$kunta.vanhat2uudet$kuntano,
#          as.character(geo$kunta.vanhat2uudet$kunta),
#          warn_missing = FALSE
#        ),
#        demografia$postinumero$data$nimi,
#        sep = "\n"
#      ),
#      warn_missing = FALSE
#    )
#}