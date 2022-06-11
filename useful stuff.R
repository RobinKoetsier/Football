colour: pink: "#D75998"
        lightblue:
          
ggtext::geom_richtext(
            aes(y = xGOT, x = xG, label = glue::glue("<img src='~/Documents/ScraperWhoScored/TDLXG/Clubs/{team}.png' width='30'/>")),
            size = 1,
            fill = NA, label.color = NA, # remove background and outline
            label.padding = grid::unit(rep(0, 4), "pt") # remove padding
) 
        
        