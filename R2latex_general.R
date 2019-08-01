Sys.setlocale(category="LC_ALL", locale = "Catalan")

informe_general = function(nom_carpeta_escola, tipus = "classe", nom_escola){
  
  print("> Inicialitzant")
  
  source('./informes.R', encoding = "UTF-8");           # fa els càlculs i els gràfics
  source('./variables-text.R', encoding = "UTF-8");     # fa el latex amb la info d'informes
  source('./text-intro.R', encoding = "UTF-8");         # text de la introducció
  source('./inicialitzadors.R', encoding = "UTF-8")     # funcions d'ajuda d'informes
  source('./tier_2.R', encoding = "UTF-8");             # escriu la part de tier 2 de làtex
  source('./informe_matrius.R', encoding = "UTF-8");   
  # source('./emocional.R', encoding = "UTF-8");  # necessari per CI, però aviat anirà fora
  source('./adaptatiu.R', encoding = "UTF-8")
  source('./barems.R', encoding = "UTF-8");
  source('./errors.R', encoding = "UTF-8");
  source('./compensacions.R', encoding = "UTF-8");
  source('./grafics.R', encoding = "UTF-8");
  source('./manipulacions_dades.R', encoding = "UTF-8")
  
  # creem el vector d'escola, amb una entrada pel nom i l'altra per les carpetes
  escola = c(nom_escola, gsub(" ", "_", nom_carpeta_escola))
  
  #####
  # agafem la info de la carpeta d'on treurem les dades (i que abans es passava dins la funció)
  #####
  
  noms_fitxers = as.vector(list.files(paste0('dades/', escola[2])))
  num_curs = substr(noms_fitxers, 1, 1)
  curs_classe = substr(noms_fitxers, 1, 2)
  noms_classes = substr(noms_fitxers, 2, 2)
  escola_curs_classe = paste0(escola[2], "-", curs_classe)
  
  cursos = list()
  for (i in 1:length(noms_fitxers)){
    cursos[[i]] = c(escola_curs_classe[i], num_curs[i])
  }
  
  noms_cursos = vector(mode="list", length=6)
  noms_cursos = c("1r de Primària", "2n de Primària", "3r de Primària", 
                  "4rt de Primària", "5è de Primària", "6è de Primària")
  
  names(noms_cursos) = seq(1, 6)
  
  classes = c()
  for (i in 1:length(noms_fitxers)){
    classes[i] = paste(noms_cursos[num_curs[i]], noms_classes[i])
  }
  
  #####
  # aquí acaba la reconstrucció dels arguments que abans es passaven a mà
  #####
  
  # agafem el directori on som i creem les carpetes pertintents on posarem les imatges i els informes
  
  wd <- getwd();
  dir.create(paste(getwd(), "/figures/", escola[2], sep ="" ));
  dir.create(paste(getwd(), "/informes/", escola[2], sep ="" ));
  
  print("> Preparant barems"); # comentem com va el tema
  
  # importem els barems i els netegem
  
  barems = list()
  for (i in 1:6){
    prebarems = read.csv(paste0('./barems/prebarems',i,'.csv'), header = FALSE, encoding = "UTF-8")
    barems[[i]] = preparar_barems(prebarems)
  }
  
  # definim les columnes que voldrem importar
  columnes = list(1:13,1:13,1:21,1:21,1:23,1:23)
  names(columnes) = c(1:6)
  
  #####
  # Aquí comença el loop que va classe per classe:
  #####
  
  for (cl in 1:length(curs_classe)){
    
    print(paste0("> Creant els informes per la classe ", classes[cl]))
    
    if (tipus == "classe"){
      sink(file(paste(getwd(), "/informes/", escola[2],"/informe_", curs_classe[cl], ".tex", sep =""), 
                open = "wt", encoding = "latin1"));#-", #escola[2], sep = ""));
      
      cat(heading_classes);
      cat("\\begin{document}")
      titol_classes(escola, classes[cl])
    }
    
    # Definim algunes variables
    matrius <- NULL;
    indeximps <- NULL;
    curs <- cursos[[cl]];
    classe <- classes[cl];
    nom_fitxer <- noms_fitxers[cl];
    
    # Importem les dades de la classe on siguem:
    punts <- read.csv(paste0("dades/", escola[2],"/", nom_fitxer), header = FALSE, encoding = "latin1");
    
    # Creem els directoris on posarem les figures i els informes d'aquella classe:
    dir.create(paste(getwd(), "/figures/", escola[2], "/", curs[1], sep ="" ));
    
    if (tipus == "individual"){
    dir.create(paste(getwd(), "/informes/", escola[2],"/", curs_classe[cl], sep ="" ));
    }
    
    #####
    ### Aquí hi van els càlculs grans de l'informe:
    ####
    curs_num = as.numeric(curs[[2]])
    matrius <- c(matrius, list(informe(punts[,columnes[[curs_num]]], curs, barems[[curs_num]], escola)))
    indeximps <- c(indeximps, list(errors(punts[,columnes[[curs_num]][-1]])))
    
    # Definim algunes variables:
    indeximp <- indeximps[[1]]; # això segurament s'hauria de netejar en algun moment
    matriu <- matrius[[1]];
    llista_columnes = list(c(1,14:19), c(1,14:19), 
                           c(1,18:38), c(1,18:38),
                           c(1,24:44), c(1,24:44))
    names(llista_columnes) = c(1,2,3,4,5,6)
    
    ################
    ### Comencem a imprimir l'informe
    ################
    
    if(tipus == "classe"){
      
      group_head_classes(classe, escola[1]);
      
      # Aquí introduïm els gràfics col·lectius, cada funció escriu a latex el gràfic universal
      # i intra-classe de cada matèria
      
      grafics_collectius_per_materia(lectura, curs, escola);
      grafics_collectius_per_materia(mt, curs, escola);
      grafics_collectius_per_materia(vp, curs, escola);
      grafics_collectius_per_materia(fm, curs, escola);
      grafics_collectius_per_materia(mlt, curs, escola);
      grafics_collectius_per_materia(r, curs, escola);
      if(curs[2] == 5 | curs[2] == 6){grafics_collectius_per_materia(c, curs, escola);}
      #}
    }
    
    # creem els gràfics emocionals
    creacio_grafics_adaptatiu(punts, curs, escola)
    
    if (tipus == "individual"){
      noms_fitxers_tex = gsub(" ", "_", punts[,1]); # trec els espais entre noms perquè no doni problemes amb el latex
      # només en el nom del fitxer, que si no segueix donant problemes
    }
    
      
    for(i in 1:length(punts[,1]))
      {
      nom = as.character(names(matriu[i]))
      
        if (tipus == "individual"){
          sink(file(paste(getwd(), "/informes/", escola[2],"/", curs_classe[cl],"/",noms_fitxers_tex[i],".tex", sep =""), 
                    open = "wt", encoding = "latin1"));
          heading_alumnes(nom)
        }
      
        
        individual_head(nom, classe, escola[1][1]);
        informe_individual(i, curs, punts[llista_columnes[[curs[2]]]], matriu, indeximp[i], escola, tipus)
        
        if (tipus == "classe"){
        cat("\\newpage");
        }
        
        else if (tipus == "individual"){
          cat("\n\n\\end{document}");
          sink();
        }
    }
    
    if (tipus == "classe"){
      cat("\n\n\\end{document}")
      sink()
      }
  }
}

