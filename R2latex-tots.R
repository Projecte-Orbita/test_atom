
crear_informe_escola <- function(cursos, classes, escola){

source('./informes.R');           # fa els càlculs i els gràfics
source('./inicialitzadors.R')     # funcions d'ajuda d'informes
source('./barems.R');             # barems  
source('./variables-text.R');     # fa el latex amb la info d'informes
source('./text-intro.R');         # text de la introducció
source('./tier_2.R');             # escriu la part de tier 2 de làtex
source('./informe_matrius.R');    # fa les matrius de l'informe (té integran l'antic taula-informe-matrius)
source('./emocional.R');          # parts emocionals de petits i grans
source('./errors.R');             # serveix per calcular l'índex d'impulsivitat
source('./compensacions.R');      # implementa el "Model Òrbita" i aplica les prediccions per cada nen
source('./grafics.R');            # gràfics

# get current directory and create missing directories if needed:
wd <- getwd();

dir.create(paste(getwd(), "/figures/", escola[2], sep ="" ));
dir.create(paste(getwd(), "/informes/", escola[2], sep ="" ));

# importem els barems i els netegem
prebarems_1 = read.csv('./barems/prebarems1.csv', header = FALSE);
barems_1 = preparar_barems(prebarems_1)
prebarems_2 = read.csv('./barems/prebarems2.csv', header = FALSE);
barems_2 = preparar_barems(prebarems_2)
prebarems_3 = read.csv('./barems/prebarems3.csv', header = FALSE);
barems_3 = preparar_barems(prebarems_3)
prebarems_4 = read.csv('./barems/prebarems4.csv', header = FALSE);
barems_4 = preparar_barems(prebarems_4)
prebarems_5 = read.csv('./barems/prebarems5.csv', header = FALSE);
barems_5 = preparar_barems(prebarems_5)
prebarems_6 = read.csv('./barems/prebarems6.csv', header = FALSE);
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

punts <- read.csv(paste("dades/", escola[2],"/", curs[1], ".csv", sep = ""), header = FALSE);
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

punts <- read.csv(paste("./dades/", escola[2], "/", curs[1], ".csv", sep = ""), header = FALSE);
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
