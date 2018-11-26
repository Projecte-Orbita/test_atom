Sys.setlocale(category="LC_ALL", locale = "Catalan")

pretractar_excels <-function(path, nom_carpeta){
  noms = excel_sheets(path = path)
  classes = lapply(excel_sheets(path), as.data.frame(read_excel), path = path)
  cursos = sapply(strsplit(noms, ""), head, 1)
  numeros_classe = sapply(strsplit(noms, ""), tail, 1)
  noms_fitxers = paste0(cursos, numeros_classe)
  
  # mirem quins tenen dades:  
  numero_nens_per_classe = sapply(classes, function(x) length(x$value.Nom))
  
  # posem un filtre pels valors que no tenen dades o en tenen poques, perquè aniran a part:
  
  classes_bones = which(numero_nens_per_classe>5)
  
  # Formategem els sheets per tenir-los de la nostra manera:
  
  # Hem d'agafar els noms i la primera lletra del cognom de tots els nens
  formatejar_noms = function(tres_columnes_noms){
    noms_nens = paste0(tres_columnes_noms[,1]," ", substr(tres_columnes_noms[,2],1,1),".")
    
    #en cas de que hi hagi nens amb el mateix nom i la mateixa inicial del cognom agafem la inicial del segon cognom:
    noms_nens[which(duplicated(noms_nens))] = paste0(noms_nens[noms_nens]," ", substr(tres_columnes_noms[noms_nens,3],1,1),".")
    return(noms_nens)
  }
  
  # s'han de triar les columnes, que no són les mateixes per tots els cursos
  
  
  columnes = list(c(9:25),c(9:25),c(9:40),c(9:40),c(9:46),c(9:46))
  names(columnes) = c(1,2,3,4,5,6)
  
  directori = getwd()
  
  dir.create(file.path(directori, "dades/", nom_carpeta), showWarnings = FALSE)
  # s'han de crear els fitxers
  
  for (i in classes_bones){
    fitxer = classes[[i]]
    fitxer = fitxer[-1,]
    cols = unlist(columnes[cursos[i]], use.names = F)
    cols = c(cols, 8) # afegim els comentaris, que els posem al final
    df = cbind.data.frame(formatejar_noms(as.data.frame(fitxer[,4:6])), fitxer[,cols])
    write.table(df, paste0("dades/",nom_carpeta, "/", noms_fitxers[i],".csv"), 
                sep = ",",
              row.names=F, 
              col.names = F,
              quote = 1)
  }
  
}



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
  sink();}

informe_per_classes = function(pre_escola){
  
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
  
  for (cl in 1:length(curs_classe)){
    
  print(paste0("> Creant informe per la classe ", classes[cl]))
    
  #LATEX
 # sink(paste(getwd(), "/informes/", escola[2],"/informe_", curs_classe[cl], sep =""));
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
    
    punts <- read.csv(paste("dades/", escola[2],"/", nom_fitxer, sep = ""), header = FALSE, encoding = "UTF-8");

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
    
    
    group_head_classes(classe, escola[1]);
    
    prova(lectura, curs, escola);
    prova(mt, curs, escola);
    prova(vp, curs, escola);
    prova(fm, curs, escola);
    prova(mlt, curs, escola);
    prova(r, curs, escola);
    if(curs[2] == 5 | curs[2] == 6){prova(c, curs, escola);}
    #}
  
  intro_part_individual();
  
 # for(cl in 1:length(cursos)){
    
    indeximp <- indeximps[[1]];
    matriu <- matrius[[1]];
  
#    curs <- cursos[[cl]];
#    classe <- classes[cl]
#    nom_fitxer <- noms_fitxers[cl];
    
    cat("\\newpage
        
        \\section*{Informes individuals}");
    
    punts <- read.csv(paste0("./dades/", escola[2], "/", nom_fitxer), header = FALSE, encoding = "UTF-8");
    #punts <- hog_punts;
    
    for(i in 1:length(punts[,1]))
    {
      individual_head(names(matriu[[i]]), classe, escola[1][1]);
      
      if(curs[2] == 1 | curs[2] == 2){
        individual(i, curs, punts[c(1,14:18,19)], matriu, indeximp[i], escola);}
      
      if(curs[2] == 3 | curs[2] == 4){
        individual(i, curs, punts[c(1,18:33,34)], matriu, indeximp[i], escola);}
      
      if(curs[2] == 5 | curs[2] == 6){
        individual(i, curs, punts[c(1,24:39,40)], matriu, indeximp[i], escola);}
      
      cat("\\newpage");}
  
  cat("\n\n\\end{document}");
  print("> Finalitzant")
  sink();
}
}

