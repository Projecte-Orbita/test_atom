Sys.setlocale(category="LC_ALL", locale = "Catalan")

crear_informe_escola <- function(cursos, classes, escola){

source('./informes.R', encoding = "UTF-8");           # fa els càlculs i els gràfics
source('./variables-text.R', encoding = "UTF-8");     # fa el latex amb la info d'informes
source('./text-intro.R', encoding = "UTF-8");         # text de la introducció
source('./inicialitzadors.R', encoding = "UTF-8")     # funcions d'ajuda d'informes
source('./tier_2.R', encoding = "UTF-8");             # escriu la part de tier 2 de làtex
source('./informe_matrius.R', encoding = "UTF-8");   
source('./emocional.R', encoding = "UTF-8");
source('./barems.R', encoding = "UTF-8");
source('./errors.R', encoding = "UTF-8");
source('./compensacions.R', encoding = "UTF-8");
source('./grafics.R', encoding = "UTF-8");
source('./manipulacions_dades.R', encoding = "UTF-8")

# get current directory and create missing directories if needed:
wd <- getwd();

dir.create(paste(getwd(), "/figures/", escola[2], sep ="" ));
dir.create(paste(getwd(), "/informes/", escola[2], sep ="" ));

# importem els barems i els netegem
prebarems_1 = read.csv('./barems/prebarems1.csv', header = FALSE, encoding = "UTF-8");
barems_1 = preparar_barems(prebarems_1)
prebarems_2 = read.csv('./barems/prebarems2.csv', header = FALSE, encoding = "UTF-8");
barems_2 = preparar_barems(prebarems_2)
prebarems_3 = read.csv('./barems/prebarems3.csv', header = FALSE, encoding = "UTF-8");
barems_3 = preparar_barems(prebarems_3)
prebarems_4 = read.csv('./barems/prebarems4.csv', header = FALSE, encoding = "UTF-8");
barems_4 = preparar_barems(prebarems_4)
prebarems_5 = read.csv('./barems/prebarems5.csv', header = FALSE, encoding = "UTF-8");
barems_5 = preparar_barems(prebarems_5)
prebarems_6 = read.csv('./barems/prebarems6.csv', header = FALSE, encoding = "UTF-8");
barems_6 = preparar_barems(prebarems_6)

#LATEX
#sink(paste(getwd(), "/informes/", escola[2],"/informe-main.tex", sep ="");#-", #escola[2], sep = ""));
sink(file(paste(getwd(), "/informes/", escola[2],"/informe-main.tex", sep =""), 
          open = "wt", encoding = "latin1"));#-", #escola[2], sep = ""));

cat(heading);

cat(text_intro);


cat("\\newpage");

intro(escola[1]);

#intro("l'\\textbf{Escola[1][1] Tecnos}", "(cursos 1r B, 2n A i B, 3r B, 4rt A i B, 5\\`{e} B i 6\\`{e} A i B de prim\\`{a}ria)")

matrius <- NULL;
indeximps <- NULL;


intro_part_grup();

for(cl in 1:length(cursos)){

curs <- cursos[[cl]];
classe <- classes[cl];

punts <- read.csv(paste("dades/", escola[2],"/", curs[1], ".csv", sep = ""), header = FALSE, encoding = "UTF-8");
#punts <- hog_punts;

dir.create(paste(getwd(), "/figures/", escola[2], "/", curs[1], sep ="" ));

if(curs[2]==1)
{matrius <- c(matrius, list(informe(punts[,1:13], curs, barems_1, escola)));
indeximps <- c(indeximps, list(errors(punts[,2:13])));}

if(curs[2]==2)
{matrius <- c(matrius, list(informe(punts[,1:13], curs, barems_2, escola)));
indeximps <- c(indeximps, list(errors(punts[,2:13])));}

if(curs[2]==3)
{matrius <- c(matrius, list(informe(punts[,1:17], curs, barems_3, escola)));
indeximps <- c(indeximps, list(errors(punts[,2:17])));}

if(curs[2]==4)
{matrius <- c(matrius, list(informe(punts[,1:17], curs, barems_4, escola)));
indeximps <- c(indeximps, list(errors(punts[,2:17])));}

if(curs[2]==5)
{matrius <- c(matrius, list(informe(punts[,1:23], curs, barems_5, escola)));
indeximps <- c(indeximps, list(errors(punts[,2:23])));}

if(curs[2]==6)
{matrius <- c(matrius, list(informe(punts[,1:23], curs, barems_6, escola)));
indeximps <- c(indeximps, list(errors(punts[,2:23])));}


group_head(classe, escola[1]);

prova(lectura, curs, escola);
prova(mt, curs, escola);
prova(vp, curs, escola);
prova(fm, curs, escola);
prova(mlt, curs, escola);
prova(r, curs, escola);
if(curs[2] == 5 | curs[2] == 6){prova(c, curs, escola);}}


intro_part_individual();

for(cl in 1:length(cursos)){

indeximp <- indeximps[[cl]];
curs <- cursos[[cl]];
classe <- classes[cl]

cat("\\vspace{1.1cm}

\\subsection{", classe, ": part individual}");

punts <- read.csv(paste("./dades/", escola[2], "/", curs[1], ".csv", sep = ""), header = FALSE, encoding = "UTF-8");
#punts <- hog_punts;

for(i in 1:length(punts[,1]))
{
individual_head(names(matrius[[cl]])[i], classe, escola[1][1]);

if(curs[2] == 1 | curs[2] == 2){
individual(i, curs, punts[c(1,14:18,19)], matrius[[cl]], indeximp, escola);}

if(curs[2] == 3 | curs[2] == 4){
individual(i, curs, punts[c(1,18:33,34)], matrius[[cl]], indeximp, escola);}

if(curs[2] == 5 | curs[2] == 6){
individual(i, curs, punts[c(1,24:39,40)], matrius[[cl]], indeximp, escola);}

cat("\\newpage");}
}
cat("\\end{document}");
sink();}

crear_informe_escola2 <- function(pre_escola){
  
  source('./informes.R', encoding = "UTF-8");           # fa els càlculs i els gràfics
  source('./variables-text.R', encoding = "UTF-8");     # fa el latex amb la info d'informes
  source('./text-intro.R', encoding = "UTF-8");         # text de la introducció
  source('./inicialitzadors.R', encoding = "UTF-8")     # funcions d'ajuda d'informes
  source('./tier_2.R', encoding = "UTF-8");             # escriu la part de tier 2 de làtex
  source('./informe_matrius.R', encoding = "UTF-8");   
  source('./emocional.R', encoding = "UTF-8");
  source('./barems.R', encoding = "UTF-8");
  source('./errors.R', encoding = "UTF-8");
  source('./compensacions.R', encoding = "UTF-8");
  source('./grafics.R', encoding = "UTF-8");
  
  # get current directory and create missing directories if needed:
  wd <- getwd();
  
  # creem el vector d'escola, amb una entrada pel nom i l'altra per les carpetes
  escola = c(pre_escola, gsub(" ", "_", pre_escola))
  
  # agafem la info de la carpeta d'on treurem les dades (i que abans es passava dins la funció)
  
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
  
  # aquí acaba la reconstrucció dels arguments que abans es passaven a mà
  
  dir.create(paste(getwd(), "/figures/", escola[2], sep ="" ));
  dir.create(paste(getwd(), "/informes/", escola[2], sep ="" ));
  
  # importem els barems i els netegem
  prebarems_1 = read.csv('./barems/prebarems1.csv', header = FALSE, encoding = "UTF-8");
  barems_1 = preparar_barems(prebarems_1)
  prebarems_2 = read.csv('./barems/prebarems2.csv', header = FALSE, encoding = "UTF-8");
  barems_2 = preparar_barems(prebarems_2)
  prebarems_3 = read.csv('./barems/prebarems3.csv', header = FALSE, encoding = "UTF-8");
  barems_3 = preparar_barems(prebarems_3)
  prebarems_4 = read.csv('./barems/prebarems4.csv', header = FALSE, encoding = "UTF-8");
  barems_4 = preparar_barems(prebarems_4)
  prebarems_5 = read.csv('./barems/prebarems5.csv', header = FALSE, encoding = "UTF-8");
  barems_5 = preparar_barems(prebarems_5)
  prebarems_6 = read.csv('./barems/prebarems6.csv', header = FALSE, encoding = "UTF-8");
  barems_6 = preparar_barems(prebarems_6)
  
  #LATEX
  #sink(paste(getwd(), "/informes/", escola[2],"/informe-main.tex", sep ="");#-", #escola[2], sep = ""));
  sink(file(paste(getwd(), "/informes/", escola[2],"/informe-main2.tex", sep =""), 
            open = "wt", encoding = "latin1"));#-", #escola[2], sep = ""));
  
  cat(heading);
  
  cat(text_intro);
  
  
  cat("\\newpage");
  
  intro(escola[1]);
  
  #intro("l'\\textbf{Escola[1][1] Tecnos}", "(cursos 1r B, 2n A i B, 3r B, 4rt A i B, 5\\`{e} B i 6\\`{e} A i B de prim\\`{a}ria)")
  
  matrius <- NULL;
  indeximps <- NULL;
  
  
  intro_part_grup();
  
  for(cl in 1:length(cursos)){
    
    curs <- cursos[[cl]];
    classe <- classes[cl];
    nom_fitxer <- noms_fitxers[cl];
    
    punts <- read.csv(paste("dades/", escola[2],"/", nom_fitxer, sep = ""), header = FALSE, encoding = "UTF-8");
    #punts <- hog_punts;
    
    dir.create(paste(getwd(), "/figures/", escola[2], "/", curs[1], sep ="" ));
    
    if(curs[2]==1)
    {matrius <- c(matrius, list(informe(punts[,1:13], curs, barems_1, escola)));
    indeximps <- c(indeximps, list(errors(punts[,2:13])));}
    
    if(curs[2]==2)
    {matrius <- c(matrius, list(informe(punts[,1:13], curs, barems_2, escola)));
    indeximps <- c(indeximps, list(errors(punts[,2:13])));}
    
    if(curs[2]==3)
    {matrius <- c(matrius, list(informe(punts[,1:17], curs, barems_3, escola)));
    indeximps <- c(indeximps, list(errors(punts[,2:17])));}
    
    if(curs[2]==4)
    {matrius <- c(matrius, list(informe(punts[,1:17], curs, barems_4, escola)));
    indeximps <- c(indeximps, list(errors(punts[,2:17])));}
    
    if(curs[2]==5)
    {matrius <- c(matrius, list(informe(punts[,1:23], curs, barems_5, escola)));
    indeximps <- c(indeximps, list(errors(punts[,2:23])));}
    
    if(curs[2]==6)
    {matrius <- c(matrius, list(informe(punts[,1:23], curs, barems_6, escola)));
    indeximps <- c(indeximps, list(errors(punts[,2:23])));}
    
    
    group_head(classe, escola[1]);
    
    prova(lectura, curs, escola);
    prova(mt, curs, escola);
    prova(vp, curs, escola);
    prova(fm, curs, escola);
    prova(mlt, curs, escola);
    prova(r, curs, escola);
    if(curs[2] == 5 | curs[2] == 6){prova(c, curs, escola);}}
  
  
  intro_part_individual();
  
  for(cl in 1:length(cursos)){
    
    indeximp <- indeximps[[cl]];
    curs <- cursos[[cl]];
    classe <- classes[cl]
    nom_fitxer <- noms_fitxers[cl];
    
    cat("\\vspace{1.1cm}
        
        \\subsection{", classe, ": part individual}");
    
    punts <- read.csv(paste0("./dades/", escola[2], "/", nom_fitxer), header = FALSE, encoding = "UTF-8");
    #punts <- hog_punts;
    
    for(i in 1:length(punts[,1]))
    {
      individual_head(names(matrius[[cl]])[i], classe, escola[1][1]);
      
      if(curs[2] == 1 | curs[2] == 2){
        individual(i, curs, punts[c(1,14:18,19)], matrius[[cl]], indeximp, escola);}
      
      if(curs[2] == 3 | curs[2] == 4){
        individual(i, curs, punts[c(1,18:33,34)], matrius[[cl]], indeximp, escola);}
      
      if(curs[2] == 5 | curs[2] == 6){
        individual(i, curs, punts[c(1,24:39,40)], matrius[[cl]], indeximp, escola);}
      
      cat("\\newpage");}
  }
  cat("\\end{document}");
  sink();} # s'ha de repassar!

informe_per_classes = function(pre_escola){
  
  print("> Carregant llibreries")
  
  source('./informes.R', encoding = "UTF-8");           # fa els càlculs i els gràfics
  source('./variables-text.R', encoding = "UTF-8");     # fa el latex amb la info d'informes
  source('./text-intro.R', encoding = "UTF-8");         # text de la introducció
  source('./inicialitzadors.R', encoding = "UTF-8")     # funcions d'ajuda d'informes
  source('./tier_2.R', encoding = "UTF-8");             # escriu la part de tier 2 de làtex
  source('./informe_matrius.R', encoding = "UTF-8");   
  source('./emocional.R', encoding = "UTF-8");
  source('./barems.R', encoding = "UTF-8");
  source('./errors.R', encoding = "UTF-8");
  source('./compensacions.R', encoding = "UTF-8");
  source('./grafics.R', encoding = "UTF-8");
  source('./manipulacions_dades.R', encoding = "UTF-8")
  
  
  #############################
  ##### Inicialització: preparem dades, carreguem fitxers, etc.
  ############################
  
  print("> Inicialitzant")
  
  
  # get current directory and create missing directories if needed:
  wd <- getwd();
  
  # creem el vector d'escola, amb una entrada pel nom i l'altra per les carpetes
  escola = c(pre_escola, gsub(" ", "_", pre_escola))
  
  # agafem la info de la carpeta d'on treurem les dades (i que abans es passava dins la funció)
  
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
  
  # aquí acaba la reconstrucció dels arguments que abans es passaven a mà
  
  dir.create(paste(getwd(), "/figures/", escola[2], sep ="" ));
  dir.create(paste(getwd(), "/informes/", escola[2], sep ="" ));
  
  ######################
  #### Preparem els barems
  ######################
  
  print("> Preparant barems");
  
  # importem els barems i els netegem
  prebarems_1 = read.csv('./barems/prebarems1.csv', header = FALSE, encoding = "UTF-8");
  barems_1 = preparar_barems(prebarems_1)
  prebarems_2 = read.csv('./barems/prebarems2.csv', header = FALSE, encoding = "UTF-8");
  barems_2 = preparar_barems(prebarems_2)
  prebarems_3 = read.csv('./barems/prebarems3.csv', header = FALSE, encoding = "UTF-8");
  barems_3 = preparar_barems(prebarems_3)
  prebarems_4 = read.csv('./barems/prebarems4.csv', header = FALSE, encoding = "UTF-8");
  barems_4 = preparar_barems(prebarems_4)
  prebarems_5 = read.csv('./barems/prebarems5.csv', header = FALSE, encoding = "UTF-8");
  barems_5 = preparar_barems(prebarems_5)
  prebarems_6 = read.csv('./barems/prebarems6.csv', header = FALSE, encoding = "UTF-8");
  barems_6 = preparar_barems(prebarems_6)
  
  
  #################
  #### Loop per tots els cursos
  ################
  
  # Aquí comença el loop que crea tots els gràfics per cada curs, així com matrius de resultats que 
  # s'utilitzaran més endavant. 
  
  for (cl in 1:length(curs_classe)){
    
    ### Preparació de les carpetes
    
    print(paste0("> Creant informe per la classe ", classes[cl]))
    
    sink(file(paste(getwd(), "/informes/", escola[2],"/informe_", curs_classe[cl], ".tex", sep =""), 
            open = "wt", encoding = "latin1"));#-", #escola[2], sep = ""));
  
    cat(heading_classes);
    cat("\\begin{document}")
    titol_classes(escola, classes[cl])

    matrius <- NULL;
    indeximps <- NULL;
  
  
  #intro_part_grup();
  
  #for(cl in 1:length(cursos)){
    
    curs <- cursos[[cl]];
    classe <- classes[cl];
    nom_fitxer <- noms_fitxers[cl];
    
    punts <- read.csv(paste("dades/", escola[2],"/", nom_fitxer, sep = ""), header = FALSE, encoding = "latin1");

    dir.create(paste(getwd(), "/figures/", escola[2], "/", curs[1], sep ="" ));
    
    ### Inici de l'elaboració dels informes, separat per cursos (ja que els barems són diferents)
    ### En algun moment es podrien ajuntar
    
    if(curs[2]==1)
    {matrius <- c(matrius, list(informe(punts[,1:13], curs, barems_1, escola)));
    indeximps <- c(indeximps, list(errors(punts[,2:13])));}
    
    if(curs[2]==2)
    {matrius <- c(matrius, list(informe(punts[,1:13], curs, barems_2, escola)));
    indeximps <- c(indeximps, list(errors(punts[,2:13])));}
    
    if(curs[2]==3)
    {matrius <- c(matrius, list(informe(punts[,1:17], curs, barems_3, escola)));
    indeximps <- c(indeximps, list(errors(punts[,2:17])));}
    
    if(curs[2]==4)
    {matrius <- c(matrius, list(informe(punts[,1:17], curs, barems_4, escola)));
    indeximps <- c(indeximps, list(errors(punts[,2:17])));}
    
    if(curs[2]==5)
    {matrius <- c(matrius, list(informe(punts[,1:23], curs, barems_5, escola)));
    indeximps <- c(indeximps, list(errors(punts[,2:23])));}
    
    if(curs[2]==6)
    {matrius <- c(matrius, list(informe(punts[,1:23], curs, barems_6, escola)));
    indeximps <- c(indeximps, list(errors(punts[,2:23])));}
    
    #### Aquí s'acaba el loop de generació de gràfics i material per fer els informes
    
    
    ################
    ### Comencem a imprimir l'informe
    ################
    
    
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
  
  #intro_part_individual();
    
    ################
    ##### Informes individuals
    ###############
  

    cat("\\newpage
        
        \\section*{Informes individuals}");
    
    # Llegim les dades:
    punts <- read.csv(paste0("./dades/", escola[2], "/", nom_fitxer), header = FALSE, encoding = "latin1");

    # Creem els gràfics d'emocional 

    creacio_grafics_emocional(punts, curs, escola)
    
    
    # Creem els gràfics i els informes individuals

    indeximp <- indeximps[[1]];
    matriu <- matrius[[1]];
    for(i in 1:length(punts[,1]))
    {
      nom = as.character(names(matriu[i]))
      individual_head(nom, classe, escola[1][1]);
      
      if(curs[2] == 1 | curs[2] == 2){
        informe_individual(i, curs, punts[c(1,14:18,19)], matriu, indeximp[i], escola);}
      
      if(curs[2] == 3 | curs[2] == 4){
        informe_individual(i, curs, punts[c(1,18:33,34)], matriu, indeximp[i], escola);}
      
      if(curs[2] == 5 | curs[2] == 6){
        informe_individual(i, curs, punts[c(1,24:39,40)], matriu, indeximp[i], escola);}
      
      cat("\\newpage");}
  
  cat("\n\n\\end{document}");
  print("> Finalitzant")
  sink();
}
}

informe_per_alumnes = function(pre_escola){
  
  print("> Inicialitzant")
  
  source('./informes.R', encoding = "UTF-8");           # fa els càlculs i els gràfics
  source('./variables-text.R', encoding = "UTF-8");     # fa el latex amb la info d'informes
  source('./text-intro.R', encoding = "UTF-8");         # text de la introducció
  source('./inicialitzadors.R', encoding = "UTF-8")     # funcions d'ajuda d'informes
  source('./tier_2.R', encoding = "UTF-8");             # escriu la part de tier 2 de làtex
  source('./informe_matrius.R', encoding = "UTF-8");   
  source('./emocional.R', encoding = "UTF-8");
  source('./barems.R', encoding = "UTF-8");
  source('./errors.R', encoding = "UTF-8");
  source('./compensacions.R', encoding = "UTF-8");
  source('./grafics.R', encoding = "UTF-8");
  source('./manipulacions_dades.R', encoding = "UTF-8")
  
  # creem el vector d'escola, amb una entrada pel nom i l'altra per les carpetes
  escola = c(pre_escola, gsub(" ", "_", pre_escola))
  
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
  
  # agafem el directori on som i creem les carpetes pertintents on posarem les imatges
  # i els informes
  
  wd <- getwd();
  dir.create(paste(getwd(), "/figures/", escola[2], sep ="" ));
  dir.create(paste(getwd(), "/informes/", escola[2], sep ="" ));
  
  print("> Preparant barems"); # comentem com va el tema
  
  # importem els barems i els netegem
  
  prebarems_1 = read.csv('./barems/prebarems1.csv', header = FALSE, encoding = "UTF-8");
  barems_1 = preparar_barems(prebarems_1)
  prebarems_2 = read.csv('./barems/prebarems2.csv', header = FALSE, encoding = "UTF-8");
  barems_2 = preparar_barems(prebarems_2)
  prebarems_3 = read.csv('./barems/prebarems3.csv', header = FALSE, encoding = "UTF-8");
  barems_3 = preparar_barems(prebarems_3)
  prebarems_4 = read.csv('./barems/prebarems4.csv', header = FALSE, encoding = "UTF-8");
  barems_4 = preparar_barems(prebarems_4)
  prebarems_5 = read.csv('./barems/prebarems5.csv', header = FALSE, encoding = "UTF-8");
  barems_5 = preparar_barems(prebarems_5)
  prebarems_6 = read.csv('./barems/prebarems6.csv', header = FALSE, encoding = "UTF-8");
  barems_6 = preparar_barems(prebarems_6)
  
  #####
  # Aquí comença el loop que va classe per classe:
  #####
  
  for (cl in 1:length(curs_classe)){
    
    print(paste0("> Creant els informes per la classe ", classes[cl]))
    
    # Definim algunes variables
    
    matrius <- NULL;
    indeximps <- NULL;
    curs <- cursos[[cl]];
    classe <- classes[cl];
    nom_fitxer <- noms_fitxers[cl];
    
    # Importem les dades de la classe on siguem:
    
    punts <- read.csv(paste("dades/", escola[2],"/", nom_fitxer, sep = ""), header = FALSE, encoding = "latin1");
    
    # Creem els directoris on posarem les figures i els informes d'aquella classe:
    
    dir.create(paste(getwd(), "/figures/", escola[2], "/", curs[1], sep ="" ));
    dir.create(paste(getwd(), "/informes/", escola[2],"/", curs_classe[cl], sep ="" ));
    
    # Calculem:

    if(curs[2]==1)
    {matrius <- c(matrius, list(informe(punts[,1:13], curs, barems_1, escola)));
    indeximps <- c(indeximps, list(errors(punts[,2:13])));}
    
    if(curs[2]==2)
    {matrius <- c(matrius, list(informe(punts[,1:13], curs, barems_2, escola)));
    indeximps <- c(indeximps, list(errors(punts[,2:13])));}
    
    if(curs[2]==3)
    {matrius <- c(matrius, list(informe(punts[,1:17], curs, barems_3, escola)));
    indeximps <- c(indeximps, list(errors(punts[,2:17])));}
    
    if(curs[2]==4)
    {matrius <- c(matrius, list(informe(punts[,1:17], curs, barems_4, escola)));
    indeximps <- c(indeximps, list(errors(punts[,2:17])));}
    
    if(curs[2]==5)
    {matrius <- c(matrius, list(informe(punts[,1:23], curs, barems_5, escola)));
    indeximps <- c(indeximps, list(errors(punts[,2:23])));}
    
    if(curs[2]==6)
    {matrius <- c(matrius, list(informe(punts[,1:23], curs, barems_6, escola)));
    indeximps <- c(indeximps, list(errors(punts[,2:23])));}
    
    # Definim algunes variables:
    
    indeximp <- indeximps[[1]]; # això segurament s'hauria de netejar en algun moment
    matriu <- matrius[[1]];
    noms_fitxers_tex = gsub(" ", "_", punts[,1]); # trec els espais entre noms perquè no doni problemes amb el latex
                                      # només en el nom del fitxer, que si no segueix donant problemes
    
    
    # creem els gràfics emocionals
    
    creacio_grafics_emocional(punts, curs, escola)
    
    #####
    # Aquí comença el loop que va alumne per alumne:
    #####
    
    for(i in 1:length(punts[,1]))
    {
      # Creem les subcarpetes 
      
      
      sink(file(paste(getwd(), "/informes/", escola[2],"/", curs_classe[cl],"/",noms_fitxers_tex[i],".tex", sep =""), 
                open = "wt", encoding = "latin1"));
      
      
      ################
      ### Aquí segurament s'hi haurà d'afegir la introducció i tapa i tot això
      ################
      
      cat(heading_alumnes);
      cat("\\begin{document}")
      
      nom_alumne = as.character(punts[i,1])
      
      individual_head(nom_alumne, # <- això són els noms
                      classe, 
                      escola[1][1]);
      
      if(curs[2] == 1 | curs[2] == 2){
        informe_individual_alumnes(i, curs, punts[c(1,14:18,19)], matriu, indeximp[i], escola);}
      
      if(curs[2] == 3 | curs[2] == 4){
        informe_individual_alumnes(i, curs, punts[c(1,18:33,34)], matriu, indeximp[i], escola);}
      
      if(curs[2] == 5 | curs[2] == 6){
        informe_individual_alumnes(i, curs, punts[c(1,24:39,40)], matriu, indeximp[i], escola);}
      
      cat("\n\n\\end{document}");
      sink();
      }
  }
  print("> Finalitzant");
}

informe_per_un_nen = function(dades, curs = 1){
  nom = dades[,1]
  
  print("> Inicialitzant")
  
  source('./informes.R');           # fa els càlculs i els gràfics
  source('./variables-text.R');     # fa el latex amb la info d'informes
  source('./text-intro.R');         # text de la introducció
  source('./inicialitzadors.R')     # funcions d'ajuda d'informes
  source('./tier_2.R');             # escriu la part de tier 2 de làtex
  source('./informe_matrius.R');   
  source('./emocional.R');
  source('./barems.R');
  source('./errors.R');
  source('./compensacions.R');
  source('./grafics.R');
  
  # agafem el directori on som i creem les carpetes pertintents on posarem les imatges
  # i els informes
  
  wd <- getwd();
  
  nom = gsub(" ", "_", dades[,1])
  
  dir.create(paste(getwd(), "/informes_individuals/", nom , sep ="" ));
  
    print(paste0("> Creant l'informe per en/na ", nom));
    
    # Definim algunes variables
    
    matrius <- NULL;
    indeximps <- NULL;

    # petita xapussa a arreglar:
  
    curs = c(curs, curs);
    escola = c("Individual", "Individual")
    
    # Calculem:
    
    if(curs[2]==1)
    {
      prebarems = read.csv('./barems/prebarems1.csv', header = FALSE);
      barems = preparar_barems(prebarems)
      
      matrius <- c(matrius, list(informe_individual_intern(dades[,1:13], curs, barems, escola)));
      indeximps <- c(indeximps, list(errors(dades[,2:13])));
      nom_curs = "1r de Primària";}
    
    if(curs[2]==2)
    {prebarems = read.csv('./barems/prebarems2.csv', header = FALSE);
    barems = preparar_barems(prebarems)
      
      matrius <- c(matrius, list(informe_individual_intern(dades[,1:13], curs, barems, escola)));
    indeximps <- c(indeximps, list(errors(dades[,2:13])));
      nom_curs = "2n de Primària";}
    
    if(curs[2]==3)
    {prebarems = read.csv('./barems/prebarems3.csv', header = FALSE);
    barems = preparar_barems(prebarems)
    matrius <- c(matrius, list(informe_individual_intern(dades[,1:17], curs, barems_3, escola)));
    indeximps <- c(indeximps, list(errors(dades[,2:17])));
    nom_curs = "3r de Primària";}
    
    if(curs[2]==4)
    {prebarems = read.csv('./barems/prebarems4.csv', header = FALSE);
    barems = preparar_barems(prebarems)
    matrius <- c(matrius, list(informe_individual_intern(dades[,1:17], curs, barems_4, escola)));
    indeximps <- c(indeximps, list(errors(dades[,2:17])));
    nom_curs = "4rt de Primària";}
    
    if(curs[2]==5)
    {prebarems = read.csv('./barems/prebarems5.csv', header = FALSE);
    barems = preparar_barems(prebarems)
    matrius <- c(matrius, list(informe_individual_intern(dades[,1:23], curs, barems_5, escola)));
    indeximps <- c(indeximps, list(errors(dades[,2:23])));
    nom_curs = "5è de Primària";}
    
    if(curs[2]==6)
    {prebarems = read.csv('./barems/prebarems6.csv', header = FALSE);
    barems = preparar_barems(prebarems)
    matrius <- c(matrius, list(informe_individual_intern(dades[,1:23], curs, barems_6, escola)));
    indeximps <- c(indeximps, list(errors(dades[,2:23])));
    nom_curs = "6è de Primària";}
    
    # Definim algunes variables:
    
    indeximp <- indeximps[[1]]; # això segurament s'hauria de netejar en algun moment
    matriu <- matrius[[1]];
    
    #####
    # Aquí comença l'elaboració de l'informe:
    #####
      
      sink(file(paste(getwd(), "/informes_individuals/", nom,"/",nom, ".tex", sep =""), 
                open = "wt", encoding = "latin1"));
      
      cat(heading_alumnes);
      cat("\\begin{document}")
      
      individual_sol_head(nom, nom_curs);
      
      if(curs[2] == 1 | curs[2] == 2){
        informe_individual_alumnes_sol(1, curs, punts[c(1,14:18,19)], matriu, indeximp[1], escola);}
      
      if(curs[2] == 3 | curs[2] == 4){
        informe_individual_alumnes_sol(1, curs, punts[c(1,18:33,34)], matriu, indeximp[1], escola);}
      
      if(curs[2] == 5 | curs[2] == 6){
        informe_individual_alumnes_sol(1, curs, punts[c(1,24:39,40)], matriu, indeximp[1], escola);}
      
      cat("\n\n\\end{document}");
      sink();
  
}
