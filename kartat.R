# kartat

kartta <-
  function(df,
           aluejako = "pono.3",
           title.label = NA,
           geo_ = geo,
           color.map = "PuBu",
           color.limits = c(NA, NA),
           cut.NA = TRUE,
           tooltip = NA) {
    # yhdistää kartan ja datan; plottaa ensimmäisen muuttujan jonka nimi ei ole "alue"
    geodata <- list()
    geodata[["pono.5"]] <-
      function(geo_)
        mutate(geo_$pono.duukkis, alue = pono)
    geodata[["pono.3"]] <-
      function(geo_)
        mutate(geo_$pono.duukkis, alue = substr(pono, 1, 3))
    geodata[["pono.2"]] <-
      function(geo_)
        mutate(geo_$pono.duukkis, alue = substr(pono, 1, 2))
    geodata[["pono.1"]] <-
      function(geo_)
        mutate(geo_$pono.duukkis, alue = substr(pono, 1, 1))
    geodata[["kuntanimi"]] <-
      function(geo_)
        mutate(geo_$kunta$"2017", alue = kuntanimi)
    geodata[["kunta"]] <-
      function(geo_)
        mutate(geo_$kunta$"2017", alue = kuntano)
    geodata[["kartogrammi.kuntanimi"]] <-
      function(geo_)
        mutate(geo_$kunta$kartogrammi, alue = kuntanimi)
    geodata[["kartogrammi.kunta"]] <-
      function(geo_)
        mutate(geo_$kunta$kartogrammi, alue = kunta)
    
    
    if (any(names(df) %in% names(geodata[[aluejako]])))
      warning("df:n kenttänimissä on karttadata kenttänimiä")
    geodata <- left_join(geodata[[aluejako]](geo_), df, by = "alue")
    
    attr = setdiff(names(df), c("alue", tooltip))[1]
    
    if (is.na(tooltip))
      if (aluejako %in% c("pono.5", "pono.3", "pono.2", "pono.1"))
        tooltip <- "nimi"
    else
      tooltip <- "kuntanimi"
    
    if (is.na(title.label))
      title.label <- attr
    if (cut.NA)
      geodata <- geodata[!is.na(geodata[[attr]]), ]
    p <- ggplot(data = arrange(geodata, order), aes(x = long, y = lat)) +
      geom_polygon_interactive(aes_string(
        fill = attr,
        group = "group",
        tooltip = tooltip,
        data_id = "alue"
      ),
      colour = NA) +
      theme_void() + theme(legend.title = element_blank()) + ggtitle(title.label)
    
    p <- p + scale_fill_gradientn(
      colours = brewer.pal(8, color.map),
      values = NULL,
      space = "Lab",
      na.value = "grey50",
      guide = "colourbar",
      limits = color.limits
    )
    
    if (!grepl("^pono", aluejako))
      p <- p + coord_equal(ratio = 1)
    else
      p <- p + coord_equal(ratio = 2.1)
    return(p)
  }

kartta.animaatio <-
  function(df,
           aluejako = "pono.3",
           geo_ = geo,
           color.map = "PuBu",
           title.label = NA) {
    # yhdistää kartan ja datan; plottaa ensimmäisen muuttujan, ei muita
    
    geodata <- list()
    geodata[["pono.5"]] <-
      function(geo_)
        mutate(geo_$pono.duukkis, alue = pono)
    geodata[["pono.3"]] <-
      function(geo_)
        mutate(geo_$pono.duukkis, alue = substr(pono, 1, 3))
    geodata[["pono.2"]] <-
      function(geo_)
        mutate(geo_$pono.duukkis, alue = substr(pono, 1, 2))
    geodata[["pono.1"]] <-
      function(geo_)
        mutate(geo_$pono.duukkis, alue = substr(pono, 1, 1))
    geodata[["kuntanimi"]] <-
      function(geo_)
        mutate(geo_$kunta$"2017", alue = kuntanimi)
    geodata[["kunta"]] <-
      function(geo_)
        mutate(geo_$kunta$"2017", alue = kuntano)
    geodata[["kartogrammi.kuntanimi"]] <-
      function(geo_)
        mutate(geo_$kunta$kartogrammi, alue = kuntanimi)
    geodata <- left_join(geodata[[aluejako]](geo_), df, by = "alue")
    attr = names(select(df,-alue,-aika))[1]
    if (is.na(title.label))
      title.label <- attr
    
    p <-
      ggplot(data = arrange(geodata, order), aes(x = long, y = lat, frame = aika)) +
      geom_polygon_interactive(aes_string(
        fill = attr,
        group = "group",
        tooltip = "alue"
      ),
      colour = NA) +
      scale_fill_gradientn(
        colours = brewer.pal(6, color.map),
        values = NULL,
        space = "Lab",
        na.value = "grey50",
        guide = "colourbar"
      ) + theme_void() +
      theme(legend.title = element_blank()) + ggtitle(title.label)
    return(p)
  }
