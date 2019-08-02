Sys.setlocale(category="LC_ALL", locale = "Catalan")
require(reshape2)
require(ggplot2)
require(RColorBrewer)
require(stringr)

directoris = config::get(config = "directoris")  # Importem la configuració
temp_ = directoris$temp
figures_ = directoris$figures

wd = getwd()

## Gràfics de classes:

grafics_classe <- function(punts, curs, tipus){
  
  punts$Noms = factor(punts$Noms, levels= unique(punts$Noms))
  
  grafic_base(punts = punts, curs = curs[1], 
              titol = "Lectura", tipus, nom_plot = paste0("lectura-", curs[2],"-",tipus), 1)
  grafic_base(punts = punts, curs = curs[1],  
              titol = "Memòria de Treball",tipus, nom_plot = paste0("mtp-", curs[2],"-",tipus), 2) 
  grafic_base(punts = punts, curs = curs[1],  
              titol = "Velocitat de Processament",tipus, nom_plot = paste0("vp-", curs[2],"-",tipus), 3)
  grafic_base(punts = punts, curs = curs[1],  
              titol = "Fluïdesa Matemàtica",tipus, nom_plot = paste0("fluidesa-", curs[2],"-",tipus), 4)
  grafic_base(punts = punts, curs = curs[1], 
              titol = "Memòria a Llarg Termini",tipus, nom_plot = paste0("mltp-", curs[2],"-",tipus), 5)
  grafic_base(punts = punts, curs = curs[1],  
              titol = "Raonament",tipus, nom_plot = paste0("raonament-", curs[2],"-",tipus), 6)
  if (curs[2] == 5 | curs[2] == 6){
    grafic_base(punts = punts, curs = curs[1],  
                titol = "Càlcul",tipus, nom_plot = paste0("calcul-", curs[2],"-", tipus), 7)
  }
}

grafic_base <- function(punts, curs, titol, tipus, nom_plot, i){
  
  myColors <- c("#56B4E9", "#009E73", "#E69F00");
  otherColors <- c("#D55E00", "#999999", "#0072B2");
  
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
                                 ifelse(tipus == 'comp','Resultat esperat',
                                        'Resultat esperat intraclasse')))) +
    theme_minimal() +
    theme(axis.title.x=element_blank(), 
          axis.title.y=element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size=16), 
          axis.text.y = element_text(size=14), 
          title=element_text(size = 20),
          #subtitle = element_text(size=15),
          #text = element_text(size=20), 
          plot.title = element_text(lineheight=.8, face="bold"),
          #plot.title = element_blank(),
          legend.position="none") + 
    #ggtitle(titol) + 
    scale_fill_manual(name='value.1', values=colors) + 
    geom_hline(yintercept=0.5) + 
    ylim(0, 1) + 
    ggsave(file = file.path(wd, temp_, figures_, curs, paste0(nom_plot, ".pdf")), 
           dpi = 600, width = 8, height = 6, units = "in") 
}

## Gràfics helpers d'individuals

ggpbar <- function(nens, punts, curs, tipus){
  
  myColors <- c("#56B4E9", "#009E73", "#E69F00");
  otherColors <- c("#D55E00", "#999999", "#0072B2");
  
  punts$Noms = factor(punts$Noms, levels= unique(punts$Noms))
  
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
    labs(title = ifelse(tipus=='norm','Resultat observat',
                        ifelse(tipus== 'norm_intra', 'Resultat observat intraclasse',
                               ifelse(tipus == 'comp', 'Resultat esperat',
                                      'Resultat esperat intraclasse'))))  +
    theme_minimal() +
    theme(axis.title.x=element_blank(), 
          axis.title.y=element_blank(), 
          axis.title = element_blank(),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20),
          title = element_text(size=20),
          legend.position="none", 
          strip.background = element_blank(),
          strip.text.x = element_blank()
          #strip.text.x = element_text(size = 22, face = "bold")
    ) +
    scale_fill_manual(name = 'value.1', values = colors) + 
    geom_hline(yintercept=0.5) + 
    ylim(0, 1) +
    ggsave(file = file.path(wd, temp_, figures_, curs, paste0(nens, "-", tipus, ".pdf")), 
           dpi = 600, width = 8, height = 9, units = "in")
}

ggpbar_individual <- function(nens, punts, curs, tipus){
  
  myColors <- c("#56B4E9", "#009E73", "#E69F00");
  otherColors <- c("#D55E00", "#999999", "#0072B2");
  
#  punts = transform(punts, Noms = factor(Noms, levels= unique(Noms)));
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
    labs(title = ifelse(tipus=='norm','Resultat obtingut',
                        ifelse(tipus== 'norm_intra', 'Resultat obtingut intraclasse',
                               ifelse(tipus == 'comp', 'Resultat esperat',
                                      'Resultat esperat intraclasse'))))  +
    theme_minimal() +
    theme(axis.title.x=element_blank(), 
          axis.title.y=element_blank(), 
          axis.title = element_blank(),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20),
          title = element_text(size=20),
          legend.position="none", 
          strip.text.x = element_text(size = 22, face = "bold")
    ) +
    scale_fill_manual(name = 'value.1', values = colors) + 
    geom_hline(yintercept=0.5) + 
    ylim(0, 1) +
    ggsave(file = file.path(wd, temp_, figures_, "informes_individuals",  
                            paste0(punts[nens,1], "-", tipus, ".pdf")), 
           dpi = 600, width = 8, height = 9, units = "in");
}

## Gràfics d'individuals

grafics_nens <- function(punts, curs, tipus){
  
  for(i in seq(1,length(unique(punts$Noms)),1)){
    ggpbar(i, punts, curs[1], tipus);
  }
  
}

grafics_nens_individual <- function(punts, curs, tipus){
  
  myColors <- c("#56B4E9", "#009E73", "#E69F00");
  otherColors <- c("#D55E00", "#999999", "#0072B2");
  
  #  punts = transform(punts, Noms = factor(Noms, levels= unique(Noms)));
  if (tipus=='norm'|tipus == 'norm_intra'){
    colors <- myColors;
  } else {
    colors <- otherColors;
  }
  
  punts[['value.1']] <- factor(punts[['value.1']]);
  names(colors) <- levels(punts[['value.1']]);
  
  ggplot(punts) + 
    geom_bar(stat='identity', 
             aes(x = variable, y = value, fill=value.1)) + 
    facet_wrap( ~ Noms, ncol=1, scales="free") +  
    labs(title = ifelse(tipus=='norm','Resultat observat',
                        ifelse(tipus== 'norm_intra', 'Resultat observat intraclasse',
                               ifelse(tipus == 'comp', 'Resultat esperat',
                                      'Resultat esperat intraclasse'))))  +
    theme_minimal() +
    theme(axis.title.x=element_blank(), 
          axis.title.y=element_blank(), 
          axis.title = element_blank(),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20),
          title = element_text(size=20),
          legend.position="none", 
          strip.text.x = element_text(size = 22, face = "bold")
    ) +
    scale_fill_manual(name = 'value.1', values = colors) + 
    geom_hline(yintercept=0.5) + 
    ylim(0, 1) +
    ggsave(file = file.path(temp_, figures_, "informes_individuals", punts[1,1], paste0(tipus, ".pdf")), 
           dpi = 600, width = 8, height = 9, units = "in");
  
}

## gràfics per la part adaptativa

grafic_adaptatiu = function(index, curs, df_emocional, nom){
  # emocional és una matriu amb 7 columes: noms dels nens, les 5 pregunes d'emocional i 
  # una altra columna buida al final que no sé què hi fa, suposo que ve amb les dades

  paleta_personalitzada = c("#FFFFCC", "#FFEDA0", "#FEB24C", "#FEB24C", "#E31A1C", "#E31A1C", "#E31A1C", "#BD0026", "#800026")
  
  if (curs[2] == 1 | curs[2] == 2){
    limy = c(0,3)
    paleta = brewer.pal(3, "YlOrRd")
    names(paleta) = seq(1,3) 
  }
  else {limy = c(0,7)
  
  paleta = paleta_personalitzada
  names(paleta) = seq(1,9) 
  }
  
  ggplot(df_emocional, aes(x = factor(arees, levels = arees[c(5,4,3,2,1)]), 
                           y = as.numeric(as.character(valors)))) + 
    geom_bar(stat="identity", aes(fill = df_emocional$valors)) + 
 #   scale_fill_gradientn(colours = paleta[]) + 
    scale_fill_manual(values = paleta) + 
    theme_bw() +
    theme(legend.position="none", 
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          title = element_text(size=20),
          plot.subtitle=element_text(size=18),
          axis.text.y = element_text(size = 18)) +
    coord_flip() +
    ylab("") +
    ylim(limy) +
    xlab("Àrea") +
    labs(title = "Riscs emocionals") 
 #        subtitle = nom) +
    ggsave(file = file.path(temp_, figures_,curs[1], paste0("emocional-", index, ".pdf")), 
           dpi = 600, width = 8, height = 6, units = "in") 
}

### Altres

# gràfic gaussiana, per tenir-lo en algun lloc

grafic_gaussiana_pintada = function(limit_inferior, limit_superior){
limitRange <- function(fun, min, max) {
  function(x) {
    y <- fun(x)
    y[x < min  |  x > max] <- NA
    return(y)
  }
}

dlimit <- limitRange(dnorm, -3.5, qnorm(limit_inferior))
ulimit <- limitRange(dnorm, qnorm(limit_superior), 3.5)
tlimit = limitRange(dnorm, qnorm(limit_inferior), qnorm(limit_superior))

pp = ggplot(data.frame(x=c(-3.5, 3.5)), aes(x=x)) +
  stat_function(fun = dnorm,
                geom="area", fill="#009E73", alpha=1) +
  stat_function(fun = dlimit,
                geom="area", fill="#E69F00", alpha=1) + 
  stat_function(fun = ulimit,
                geom="area", fill="#56B4E9", alpha=1) + 
  theme_void() + 
  labs(title = "Distribució normal",
       subtitle = "En taronja marquem baix rendiment i en blau alt rendiment") +
  annotate(geom="text", x = qnorm(limit_inferior), y = -0.01, label = "30%", colour = "azure4") +
  annotate(geom="text", x = qnorm(limit_superior), y = -0.01, label = "85%", colour = "azure4") +
  ggsave(file = "gaussiana.pdf", dpi = 600, width = 10, height = 6, units = "in")

  #return(pp)
}

#grafic_gaussiana_pintada(.3, .85)
