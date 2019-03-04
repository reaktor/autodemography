## Compute count statistics for all vehicles 

if (db_has_table(trafi.db$con,"stat_vehicles")) db_drop_table(trafi.db$con,"stat_vehicles")

tbl(trafi.db,"ajoneuvot") %>% 
  select(ajoneuvoluokka,
         ajoneuvoryhma,
         ajoneuvonkaytto,
         kunta, alue,
         kayttoonottoVuosi,
         ensirekVuosi,
         data) %>%
  collect(n=Inf) %>% 
  group_by(ajoneuvoluokka, 
           ajoneuvoryhma, 
           ajoneuvonkaytto, 
           kunta, 
           alue, 
           kayttoonottoVuosi, 
           ensirekVuosi, 
           data) %>% 
  summarise(N=n()) %>% ungroup %>% 
  copy_to(trafi.db,., "stat_vehicles", temporary=FALSE)

