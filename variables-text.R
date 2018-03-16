heading <- "\\documentclass[a4paper, 12pt, oneside]{book}%{article}
\\usepackage{framed}
\\usepackage[left=3cm,right=3cm,top=2cm]{geometry}
\\usepackage[sfdefault]{cabin}
\\usepackage{graphicx,longtable}
%\\usepackage[latin1]{inputenc}
\\usepackage{amsmath}
\\usepackage{color}
\\usepackage{multicol}
\\usepackage{flushend}
\\usepackage{balance}
\\usepackage{float}
%\\usepackage{subfig}
\\usepackage{subcaption}
\\usepackage{enumitem}
\\usepackage{titlesec}
\\usepackage[final]{pdfpages}
\\usepackage[T1]{fontenc}
\\usepackage[latin1]{inputenc}   %paquet que serveix per poder escriure
                %els accents de forma normal en Linux
                %en Windows canvieu-ho per: \\usepackage[ansinew]{inputenc}
\\usepackage[catalan]{babel}

\\definecolor{orbita}{rgb}{0.0235, 0.8275, 0.5921}

\\usepackage{fancyhdr}

\\pagestyle{fancy}
\\fancyhf{}  
\\lfoot{\\includegraphics[scale=0.3]{informe-atom-peu}}
\\rfoot{\\small \\thepage}

\\setlength\\parindent{0pt}
\\captionsetup[subfigure]{labelformat=empty}
\\fancyfootoffset[LO,LE]{2cm}

\\titleformat{\\chapter}[display]
  {\\normalfont\\huge\\bfseries}{}{0pt}{\\Huge}
\\titlespacing*{\\chapter}
  {0pt}{10pt}{40pt}


\\begin{document}
\\tableofcontents
";


lectura <- c("Lectura", "La tasca de lectura pret\\'{e}n avaluar la flu\"{i}desa lectora dels alumnes, tant a nivell de descodificaci\\'{o} com de comprensi\\'{o}.", "lectura");

vp <- c("Velocitat de processament", "La velocitat de processament \\'{e}s l'agilitat que mostra la persona en explorar, ordenar i discriminar informaci\\'{o} visual simple de forma r\\`{a}pida i efica\\c{c}.", "vp");

mt <- c("Memòria de Treball", "La memòria de treball és l'habilitat per mantenir una informació a la memòria sense que aquesta sigui afectada per la interferència d'altres processos de manipulació d'informació. \\", "mtp")

fm <- c("Fluïdesa matemàtica", "La fluïdesa matemàtica ens informa del procés d'automatització de les operacions numèriques bàsiques i l'habilitat per resoldre-les correctament. \\", "fluidesa")

mlt <- c("Memòria a Llarg Termini", "La memòria a llarg termini es refereix a la capacitat d'evocació d'informació presentada i processada amb anterioritat. \\", "mltp")

r <- c("Raonament", "El raonament lògic fa referència a la destresa per inferir patrons i deduir la seva continuïtat.  \\", "raonament")

c <- c("Càlcul", "La prova de càlcul mesura la capacitat de raonament quantitatiu a través d'operacions matemàtiques no automatitzades. \\", "calcul")


#nom_escola sense article

titol_barem <- function(nom_escola){cat("
\\begin{center}
{\\Huge{RESULTATS TIER 1 \\\\ ", nom_escola, "}}
\\end{center}", sep = "")};


#nom_escola inclou l'article l'\textbf{Escola Tecnos}
#cursos \\'{e}s de la forma  "(cursos 1r A, 3r A i 5\\`{e} A de prim\\`{a}ria)"

intro <- function(nom_escola){cat("\\chapter{Informe Test Àtom \\\\", nom_escola, "}


A continuaci\\'{o} presentem els resultats de la aplicaci\\'{o} del \\textbf{Tier I} del Test \\`{A}tom a ", nom_escola, ". Aquests resultats permeten identificar fortaleses i debilitats dels perfils cognitius dels alumnes, despr\\'{e}s d'haver estat comparats amb un barem de refer\\`{e}ncia extret d'una mostra de 2095 alumnes de primària de diverses regions de Catalunya. Presentem, per una banda, els resultats dels nens i les nenes obtinguts a través de tota la mostra que hem recollit, per tal de saber en quina situació estan en el marc d'una mostra universal. Per altra banda, mostrem també els resultats interns en el marc de cada classe, ja que aquests poden ser de molta utilitat per als mestres per tal de poder utilitzar els seus recursos educatius de la forma més profitosa possible per a tots els nens i nenes.  

Tal i com s'explica en els següents capítols, el Tier I és la primera part del procediment de cribatge universal per al qual està dissenyat el Test Àtom. Per tant, és necessària l'administració del Tier II (i el Tier III en certs casos) per tal d'obtenir els perfils cognitius complets de tots els infants.  \\\\ 

Els resultats exposats a continuació poden haver estat interferits per diversos factors personals i contextuals (cansament, motivació, experiència prèvia amb tasques similars...). Cal que s'interpretin com a indicadors dels factors avaluats però mai com a mesura definitiva i estable de les aptituds o adaptació dels alumnes.


", sep = "")};


intro_part_grup<-function(){cat("

\\newpage

\\section{PART I: Resultats intra-grup}

En aquest apartat presentem els resultats de les diverses proves de forma conjunta, presentant tant els resultats de cada alumne comparat amb un barem de refer\\`{e}ncia com els del grup-classe avaluat internament, \\'{e}s a dir, amb comparacions fetes dins la pr\\`{o}pia classe. En ambd\\'{o}s casos, els resultats estan presentats de tal manera que els alumnes amb resultats inferiors al \\textbf{30\\%} estan marcats en \\textbf{taronja} i els que pertanyen al \\textbf{85\\%} superior en \\textbf{blau}.  \\\\

Aquests dos conjunts estan destacats ja que estan formats pels alumnes amb m\\'{e}s risc potencial de desadaptaci\\'{o} educativa a causa de la seva desviaci\\'{o} respecte a la mitjana del grup de refer\\`{e}ncia (el barem de refer\\`{e}ncia o el grup-classe). \\newpage", sep = "")};




intro_part_individual<-function(){cat("

\\newpage

\\section{PART II: Resultats individuals} 

En aquest apartat presentem els resultats individuals, presentant els resultats de cada alumne comparat amb un barem de refer\\`{e}ncia i els resultats compensats explicats anteriorment.

Recordem que mostrem dos gràfics per alumne, l'experimental i el predit, i les llegendes són les següents respectivament: 

\\begin{enumerate}[label=\\alph*.]
\\item Taronja: rendiment per sota del 30\\% de la població.
\\item Verd: rendiment dins la normalitat.
\\item Blau: rendiment per sobre del 85\\% de la població. 
\\end{enumerate} 


\\begin{enumerate}[label=\\alph*.]
\\item Vermell: el valor predit és significativament (una desviació estàndard) més alt que el mesurat, i per tant detectem una dificultat en aquella àrea. 
\\item Gris: el valor predit és similar al mesurat,
\\item Blau: el valor predit és significativament (una desviació estàndard) més baix que el mesurat, i per tant detectem una potencialitat. 
\\end{enumerate} 

Abreviatures: \\\\
\\begin{itemize}
\\item L: Lectura
\\item VP: Velocitat de processament
\\item MT: Memòria de Treball
\\item FM: Fluïdesa matemàtica
\\item MLT: Memòria a Llarg Termini
\\item R: Raonament
\\item C: Càlcul
\\end{itemize}

", sep = "")};



#nom_classe = 1r de Prim\\`{a}ria, classe A

classe_grup_head <- function(nom_classe){cat("

\\textbf{", nom_classe, "} \\\\
Nota: a tots els gr\\`{a}fics la mitjana corresponent est\\`{a} indicada amb una ratlla horitzontal. \\\\",
 sep = "")};



prova <- function(prova, curs, escola){cat("
%\\textbf{", prova[1], "}
\\begin{figure}[H]
\\centering
\\includegraphics[width=13cm]{figures/",escola[2],"/", curs[1], "/", prova[3], "-", curs[2], "-norm}
\\end{figure}",

if(1==0){cat(observacions-1)},
"%La prova no \\'{e}s v\\`{a}lida pel Josep ja que no va seguir b\\'{e} les instruccions de la tasca. \\
%La Nerea no va passar p\\`{a}gina, de manera que els seus resultats reals podrien ser superiors als evidenciats en aquesta tasca. \\\\

\\begin{figure}[H]
\\centering
\\includegraphics[width=13cm]{figures/",escola[2],"/", curs[1], "/", prova[3], "-", curs[2], "-norm_intra}
\\end{figure}",
 
sep = "")};



group_head <- function(classe, escola){
cat("
\\vspace{1.1cm}
\\subsection{Classe ", classe, ": part col·lectiva}

%\\begin{framed}
%\\centering
%\\textbf{Classe ", classe, "} \\\\
%\\textbf{", escola, "} \\\\
%\\end{framed}

", sep = "")}


individual_head <- function(nom, classe, escola){
cat("
\\newpage

\\begin{framed}
\\textbf{", nom, "} \\\\
\\textbf{Classe ", classe, "} \\\\
\\textbf{", escola, "} \\\\
\\end{framed}

", sep = "")}


individual <- function(index, curs, punts, matrius, indeximp, escola){

  
cat("
\\vspace{1.2cm}


\\begin{figure}[H]
\\captionsetup[subfigure]{labelformat=empty}
\\begin{subfigure}{.5\\textwidth}
\\centering
\\includegraphics[width=7.5cm]{figures/",escola[2],"/", curs[1], "/", index, "-norm}
\\end{subfigure}
\\begin{subfigure}{.5\\textwidth}
\\centering
\\includegraphics[width=7.5cm]{figures/",escola[2],"/", curs[1], "/", index, "-comp}
\\end{subfigure}
\\caption{En el gràfic de l'esquerra veiem els resultats \\emph{mesurats} i a la dreta els \\emph{esperats}. Si hi ha resultats en vermell al gràfic de la dreta és perquè mesurem aquella habilitat més \\emph{baixa} que la predida, i per tant parlem d'una possible \\emph{dificultat específica}. En canvi, si estan en blau és perquè són més \\emph{alts} dels predits i per tant parlem d'un possible \\emph{talent}.}
\\end{figure}", sep="")

  #punts[is.na(punts[,ncol(punts)])]<-"";
  if(!is.na(punts[index,ncol(punts)]) && as.character(punts[index,ncol(punts)])!=""){
    text = as.character(punts[index,ncol(punts)]);
    cat("\\textbf{Nota:} durant l'administració o la correcció del test s'ha anotat que ", text, ", fet pot afectar els resultats que observem i les conclusions que en traiem.", sep="")
  }

cat("
\\begin{center}
\\Large{\\textbf{Valoració Cognitiva i de Rendiment}} \\\\ 
\\end{center}
", sep = "");


informe_matrius(matrius[[as.character(names(matrius)[index])]], names(matrius)[index]);

if(!is.na(indeximp[index]) & indeximp[index]>0.18){cat("
Es valora que l'estil de resposta fortament tendent a la \\emph{impulsivitat} pot haver interferit negativament en els resultats obtinguts, havent acumulat una quantitat d'errors superior a la seva capacitat real d'execució.")}

if(!is.na(indeximp[index]) & indeximp[index]<0.09){cat("
Es valora que l'estil de resposta fortament tendent a la \\emph{reflexivitat} pot haver interferit negativament en els resultats obtinguts, havent buscat minimitzar la quantitat d'errors i mostrant un rendiment inferior a la seva capacitat real d'execució.")}

cat("

\\begin{center}
\\Large{\\textbf{Valoraci\\'{o} Adaptativa}} \\\\
\\end{center}

");

if(curs[2] > 2){futur_em <- informe_emocional(index, punts);}
else{futur_em <- informe_emocional_petits(index, punts)};

cat("



\\begin{center}
\\Large{\\textbf{Orientacions}} \\\\
\\end{center} 

");
futur_tier <- informe_tier2(matrius[[index]], names(matrius)[index]);
if(futur_em == TRUE){cat("Es constaten alteracions emocionals que poden haver interferit en l'execució cognitiva i de rendiment exposada anteriorment. Per tant recomanem que es procedeixi a aprofundir en les causes d'aquestes alteracions emocionals abans de continuar l'exploració del perfil cognitiu.\\\\ ") 
}else{cat("No es constata interferència de l'estat emocional en els resultats obtinguts. \\\\")};

if(futur_tier == TRUE){cat("Els resultats obtinguts ens permeten determinar que ", names(matrius)[index], " pot beneficiar-se de mesures metodològiques destinades a abordar les àrees prèviament comentades. ");
}

}
