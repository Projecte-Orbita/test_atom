
crear_informe_escola <- function(cursos, classes, escola){


source('./f_informes_proves.R');
source('./variables-text.R');
source('./text-intro.R');
source('./tier_2.R');
source('./informe_matrius.R');
source('./taula-informe-matrius.R');
source('./emocional.R');
source('./emocional_petits.R');  
source('./barems-01.R');
source('./errors.R');
source('./compensacions.R');
source('./grafics.R');

# get current directory and create missing directories if needed:
wd <- getwd();

dir.create(paste(getwd(), "/figures/", escola[2], sep ="" ));
dir.create(paste(getwd(), "/informes/", escola[2], sep ="" ));

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
{prebarems <- read.csv('./barems/prebarems1.csv', header = FALSE);
matrius <- c(matrius, list(informe(punts[,1:13], curs[1], prebarems, escola)));
indeximps <- c(indeximps, list(errors(punts[,2:13])));}

if(curs[2]==2)
{prebarems <- read.csv('./barems/prebarems2.csv', header = FALSE);
matrius <- c(matrius, list(informe(punts[,1:13], curs, prebarems, escola)));
indeximps <- c(indeximps, list(errors(punts[,2:13])));}

if(curs[2]==3)
{prebarems <- read.csv('./barems/prebarems3.csv', header = FALSE);
matrius <- c(matrius, list(informe3(punts[,1:17], curs[1], prebarems, escola)));
indeximps <- c(indeximps, list(errors(punts[,2:17])));}

if(curs[2]==4)
{prebarems <- read.csv('./barems/prebarems4.csv', header = FALSE);
matrius <- c(matrius, list(informe4(punts[,1:17], curs[1], prebarems, escola)));
indeximps <- c(indeximps, list(errors(punts[,2:17])));}


if(curs[2]==5)
{prebarems <- read.csv('./barems/prebarems5.csv', header = FALSE);
matrius <- c(matrius, list(informe5(punts[,1:23], curs[1], prebarems, escola)));
indeximps <- c(indeximps, list(errors(punts[,2:23])));}


if(curs[2]==6)
{prebarems <- read.csv('./barems/prebarems6.csv', header = FALSE);
matrius <- c(matrius, list(informe6(punts[,1:23], curs[1], prebarems, escola)));
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
