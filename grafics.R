library(reshape2)
library(ggplot2)


# Aquest és el plot mare per tal de fer els plots de barres

grafic_base <- function(punts, curs, titol, tipus, nom_plot, escola, i){
  
  myColors <- c("#56B4E9", "#009E73", "#E69F00");
  otherColors <- c("#D55E00", "#999999", "#0072B2");
  
  punts = transform(punts, Noms = factor(Noms, levels= unique(Noms)));
  
  if (tipus=='norm'| tipus == 'norm_intra'){
    colors <- myColors;
  } else {
    colors <- otherColors;
  }
  
  punts[['value.1']] <- factor(punts[['value.1']]);
  names(colors) <- levels(unique(punts[['value.1']]));
  
  
  ggplot(punts[punts$variable %in% levels(unique(punts$variable))[i],]) + 
    geom_bar(stat='identity', aes(x = Noms, y = value, fill = value.1)) + 
    labs(title = titol, 
         subtitle= ifelse(tipus=='norm','Comparació barem universal',
                          ifelse(tipus== 'norm_intra','Comparació intra-classe',
                                 ifelse(tipus == 'comp','Resultat predit',
                                        'Resultat predit intraclasse')))) +
    theme(axis.title.x=element_blank(), 
          axis.title.y=element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size=16), 
          axis.text.y = element_text(size=14), 
          title=element_text(size = 20),
          #subtitle = element_text(size=15),
          #text = element_text(size=20), 
          plot.title = element_text(lineheight=.8, face="bold"), 
          legend.position="none") + 
    #ggtitle(titol) + 
    scale_fill_manual(name='value.1', values=colors) + 
    geom_hline(yintercept=0.5) + 
    ylim(0, 1) + 
    ggsave(file = paste("figures/", escola[2], "/", curs, "/", nom_plot, ".pdf", sep = ""), 
           dpi = 600, width = 8, height = 6, units = "in") 
}

ggpbar <- function(nens, pos, punts, curs, tipus, escola){
  
  myColors <- c("#56B4E9", "#009E73", "#E69F00");
  otherColors <- c("#D55E00", "#999999", "#0072B2");
  
  punts = transform(punts, Noms = factor(Noms, levels= unique(Noms)));
  if (tipus=='norm'|tipus == 'norm_intra'){
    colors <- myColors;
  } else {
    colors <- otherColors;
  }
  
  punts[['value.1']] <- factor(punts[['value.1']]);
  names(colors) <- levels(punts[['value.1']]);
  
  ggplot(punts[punts$Noms %in% levels(punts$Noms)[nens],]) + 
    geom_bar(stat='identity', 
             aes(x = variable, y = value, fill=value.1)) + 
    facet_wrap( ~ Noms, ncol=1, scales="free") +  
    labs(title = ifelse(tipus=='norm','Resultat experimental',
                        ifelse(tipus== 'norm_intra', 'Resultat experimental intraclasse',
                               ifelse(tipus == 'comp', 'Resultat predit',
                                      'Resultat predit intraclasse'))))  +
    theme(axis.title.x=element_blank(), 
          axis.title.y=element_blank(), 
          axis.title = element_blank(),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20),
          title = element_text(size=20),
          legend.position="none", 
          strip.text.x = element_text(face = "bold")
    ) +
    scale_fill_manual(name = 'value.1', values = colors) + 
    geom_hline(yintercept=0.5) + 
    ylim(0, 1) +
    ggsave(file = paste("figures/", escola[2], "/", curs, "/",pos,"-", tipus, ".pdf", sep = ""), 
           dpi = 600, width = 8, height = 9, units = "in");
}

grafics_nens <- function(punts, curs, tipus, escola){
  
  for(i in seq(1,length(levels(unique(punts$Noms))),1)){
    ggpbar(i, i, punts, curs, tipus, escola);
  }
  
}
  
## plots de les proves conjuntes per tota la classe

grafics_classe <- function(punts, curs, tipus, escola, i){
  
  grafic_base(punts = punts, curs = curs, titol = "Lectura",tipus, nom_plot = paste("lectura-", i,"-",tipus, sep = ""), escola, 1);
  grafic_base(punts = punts, curs = curs,  titol = "Memòria de Treball",tipus, nom_plot = paste("mtp-", i,"-",tipus, sep =""),escola, 2); 
  grafic_base(punts = punts, curs = curs,  titol = "Velocitat de Processament",tipus, nom_plot = paste("vp-", i,"-",tipus, sep =""),escola,3);
  grafic_base(punts = punts, curs = curs,  titol = "Fluïdesa Matemàtica",tipus, nom_plot = paste("fluidesa-", i,"-",tipus, sep =""),escola,4);
  grafic_base(punts = punts, curs = curs, titol = "Memòria a Llarg Termini",tipus, nom_plot = paste("mltp-", i,"-",tipus, sep =""),escola,5);
  grafic_base(punts = punts, curs = curs,  titol = "Raonament",tipus, nom_plot = paste("raonament-", i,"-",tipus, sep =""),escola,6);
  if (curs == 5 | curs == 6){
    grafic_base(punts = punts, curs = curs,  titol = "Càlcul",tipus, nom_plot = paste("calcul-", i,"-", tipus, sep =""), escola,7);
  }
}
