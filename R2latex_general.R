Sys.setlocale(category="LC_ALL", locale = "Catalan")
source('./informes.R', encoding = "UTF-8");           # fa els càlculs i els gràfics
source('./variables-text.R', encoding = "UTF-8");     # fa el latex amb la info d'informes
source('./text-intro.R', encoding = "UTF-8");         # text de la introducció
source('./inicialitzadors.R', encoding = "UTF-8")     # funcions d'ajuda d'informes
source('./tier_2.R', encoding = "UTF-8");             # escriu la part de tier 2 de làtex
source('./informe_matrius.R', encoding = "UTF-8");    # Escriu les valoracions de la part cognitiva
# source('./emocional.R', encoding = "UTF-8");  # necessari per CI, però aviat anirà fora
source('./adaptatiu.R', encoding = "UTF-8")           # Crea els gràfics i escriu la valoració adaptativa
source('./barems.R', encoding = "UTF-8");             # Carrega i calcula els barems
source('./errors.R', encoding = "UTF-8");             # Resta els errors dels encerts
source('./compensacions.R', encoding = "UTF-8");      # Calcula el perfil teòric i el compara amb el real
source('./grafics.R', encoding = "UTF-8");            # Gràfics
source('./manipulacions_dades.R', encoding = "UTF-8") # Crea els csv a partir dels excels


informe_general = function(nom_escola, path_llista, tipus = "classe"){
  
  # Aquesta és la funció principal que general els informes, tant col·lectius com individuals
  # 
  # Arguments: nom_escola: nom de l'escola, que s'impirimà en els informes
  #            tipus: classe o individual
  # TODO: fer un tipus  "tot" que es faci els dos a la vegada
  
  print("> Inicialitzant")
  
  # Creem les carpetes on treballarem
  
  dir.create(path_llista$figures);
  dir.create(path_llista$informes);
  
  # creem el vector d'escola, amb una entrada pel nom i l'altra per les carpetes
  # TODO: això no cal, treure-ho
  escola = c(nom_escola, "temp")
  
  #####
  # agafem la info de la carpeta d'on treurem les dades
  #####
  
  noms_fitxers = as.vector(list.files(path_llista$dades))
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
  
  print("> Preparant barems"); # comentem com va el tema
  
  # importem els barems i els netegem
  
  barems = list()
  for (i in 1:6){
    fitxer = file.path(path_llista$barems, paste0('prebarems', i, '.csv'))
    prebarems = read.csv(fitxer, header = FALSE, encoding = "UTF-8")
    barems[[i]] = preparar_barems(prebarems)
  }
  
  # definim les columnes que voldrem importar per cada curs
  columnes = list(1:13,1:13,1:21,1:21,1:23,1:23)
  names(columnes) = c(1:6)
  
  #####
  # Aquí comença el loop que va classe per classe:
  #####
  
  for (cl in 1:length(curs_classe)){
    
    print(paste0("> Creant els informes per la classe ", classes[cl]))
    
    if (tipus == "classe"){
      
      # Obrim el fitxer i escrivim la introducció
      
      sink(file(file.path(path_llista$informes, 
                          paste0("informe_", curs_classe[cl], ".tex")), 
                open = "wt", encoding = "latin1"))
      
      cat(heading_classes)  # Introducció (variables-text, línia 55)
      cat("\\begin{document}")
      titol_classes(escola, classes[cl])  # Intro classe (variables-text, línia 255)
    }
    
    # Definim algunes variables
    matrius <- NULL;
    indeximps <- NULL;
    curs <- cursos[[cl]];
    classe <- classes[cl];
    nom_fitxer <- noms_fitxers[cl];
    
    # Importem les dades de la classe on siguem:
    punts <- read.csv(file.path(path_llista$dades, nom_fitxer), 
                      header = FALSE, encoding = "latin1")
    
    # Creem els directoris on posarem les figures i els informes d'aquella classe:
    dir.create(file.path(path_llista$figures, curs[1]))
    
    if (tipus == "individual"){
      dir.create(file.path(path_llista$informes, curs_classe[cl]))
    }
    
    #####
    ### Aquí hi van els càlculs grans de l'informe:
    ####
    
    curs_num = as.numeric(curs[[2]])
    
    # Aquí hi ha la funció principal de càlculs: 
    
    colors = calculs_previs(punts[,columnes[[curs_num]]], curs, barems[[curs_num]], escola)
    matrius <- c(matrius, list(colors))
    
    # La d'errors:
    
    colerrors = errors(punts[,columnes[[curs_num]][-1]])
    indeximps <- c(indeximps, list(colerrors))
    
    # I la d'emocional:
    creacio_grafics_adaptatiu(punts, curs, escola)
    
    #### Aquí acaben els càlculs previs
    
    ################
    ### Comencem a imprimir l'informe
    ################
    
    # Definim algunes variables de cara a imprimir:
    indeximp <- indeximps[[1]]; # això segurament s'hauria de netejar en algun moment
    matriu <- matrius[[1]];
    llista_columnes = list(c(1,14:19), c(1,14:19), 
                           c(1,18:38), c(1,18:38),
                           c(1,24:44), c(1,24:44))
    names(llista_columnes) = c(1,2,3,4,5,6)
    
    if(tipus == "classe"){
      
      group_head_classes(classe, escola[1]);
      
      # Aquí introduïm els gràfics col·lectius, cada funció escriu el tros de latex que importa
      #  els gràfics en qüestió
      
      grafics_collectius_per_materia(lectura, curs, escola);
      grafics_collectius_per_materia(mt, curs, escola);
      grafics_collectius_per_materia(vp, curs, escola);
      grafics_collectius_per_materia(fm, curs, escola);
      grafics_collectius_per_materia(mlt, curs, escola);
      grafics_collectius_per_materia(r, curs, escola);
      if(curs[2] == 5 | curs[2] == 6){grafics_collectius_per_materia(c, curs, escola);}
      #}
    }
    
    if (tipus == "individual"){
      noms_fitxers_tex = gsub(" ", "_", punts[,1]); # trec els espais entre noms perquè no doni problemes amb el latex
      # només en el nom del fitxer, que si no segueix donant problemes
    }
    
    
    for(i in 1:length(punts[,1]))
    {
      nom = as.character(names(matriu[i]))
      
      if (tipus == "individual"){
        sink(file(file.path(path_llista$informes, curs_classe[cl], paste0(noms_fitxers_tex[i],".tex")), 
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

