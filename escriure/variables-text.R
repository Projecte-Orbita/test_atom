Sys.setlocale(category="LC_ALL", locale = "Catalan")


wd = getwd()

##### headings

heading <- paste0("\\documentclass[a4paper, 12pt, oneside]{book}%{article}
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

\\newenvironment{normalize}{\\leftmargin}{\\par}

\\definecolor{orbita}{rgb}{0.0235, 0.8275, 0.5921}

\\usepackage{fancyhdr}
\\usepackage{graphicx}
\\pagestyle{fancy}
\\fancyhf{}  
\\lfoot{\\includegraphics[scale=0.3]{", wd, "/imatges/informe-atom-peu}}
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
")


heading_classes <- paste0("\\documentclass[a4paper, 12pt, oneside]{book}%{article}
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

\\newenvironment{normalize}{\\leftskip-\\leftmargin}{\\par}

\\definecolor{orbita}{rgb}{0.0235, 0.8275, 0.5921}

\\usepackage{fancyhdr}
\\usepackage{graphicx}
\\pagestyle{fancy}
\\fancyhf{}  
\\lfoot{\\includegraphics[scale=0.3]{", wd, "/imatges/informe-atom-peu}}
\\rfoot{\\small \\thepage}

\\setlength\\parindent{0pt}
\\captionsetup[subfigure]{labelformat=empty}
\\fancyfootoffset[LO,LE]{2cm}

\\titleformat{\\chapter}[display]
{\\normalfont\\huge\\bfseries}{}{0pt}{\\Huge}
\\titlespacing*{\\chapter}
{0pt}{10pt}{40pt}
")

heading_alumnes <- function(nom){ 

  wd = getwd()

cat("
\\documentclass[a4paper, 12pt, oneside]{article}
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

\\newenvironment{normalize}{\\leftskip-\\leftmargin}{\\par}

\\definecolor{orbita}{rgb}{0.0235, 0.8275, 0.5921}

\\usepackage{pdfpages} % per poder posar la tapa en pdf
\\usepackage{tikz} % per pode posar el nom sobre la tapa
\\usepackage{fancyhdr}
\\usepackage{graphicx}
\\pagestyle{fancy}
\\fancyhf{}  
\\lfoot{\\includegraphics[scale=0.3]{", wd, "/imatges/informe-atom-peu}}
\\rfoot{\\small \\thepage}

%\\setlength\\parindent{0pt}
\\captionsetup[subfigure]{labelformat=empty}
\\fancyfootoffset[LO,LE]{2cm}
\\title{Informe de resultats del Test Àtom}
\\date{}
\\titleformat{\\chapter}[display]
{\\normalfont\\huge\\bfseries}{}{0pt}{\\Huge}
\\titlespacing*{\\chapter}
{0pt}{10pt}{40pt}

\\begin{document}
\\includepdf[pages=-,pagecommand={\\begin{tikzpicture}[remember picture,overlay]\\node [xshift = 0cm, yshift = 4.5cm] at (current page.center)  {\\textbf{\\huge{",nom,"}}};\\end{tikzpicture}}]{", wd, "/imatges/Portada-resultats}

\\maketitle

\\section{Introducció}

El \\textbf{Test Àtom}, desenvolupat pel Projecte Òrbita, és una bateria d’ampli espectre de proves de \\textbf{rendiment escolar, d’habilitats cognitives i d’adaptació emocional}. Es tracta d’una mesura objectiva, integradora i científicament validada amb una mostra de 2095 alumnes d’escoles públiques, privades i concertades de Catalunya durant el curs 2016-2017. 

L’objectiu  del test és conèixer el perfil cognitiu i d’adaptació emocional dels alumnes, així com oferir o recomanar les eines, metodologies o intervencions educatives i emocionals més adequades per cada nen o nena.

Cal considerar que els resultats corresponen a l’avaluació d’un sol dia i en unes condicions determinades i que per tant cal interpretar-les tenint en compte que els resultats es poden veure afectats per factors circumstancials com la motivació, l’atenció a les instruccions i l’estat personal de l’alumne aquell dia. Els resultats poden ser inferiors a la capacitat real dels alumnes si no s'han donat els factors per promoure el seu màxim rendiment. D'altra banda, aquells que han obtingut uns resultats elevats indiquen que, en algunes condicions, encara que no siguin les més habituals, són capaços de mostrar produccions remarcables.

S’ha de tenir en compte, doncs, que els resultats corresponen a una primera visió general, i per tant en cap cas suposen un diagnòstic i sempre es necessitaran d’altres proves per confirmar possibles afectacions o necessitats específiques. En l’informe de resultats es suggereixen futurs tests que es poden necessitar en cada cas. 

\\section{Conceptes de l’informe}

A continuació descrivim els conceptes clau que apareixen en l’informe per descriure els resultats dels nens i nenes:

\\begin{itemize}
\\item \\textbf{Alt rendiment}: habilitat significativament elevada comparada amb la mitjana de la població.
\\item \\textbf{Baix rendiment}: habilitat significativament inferior comparada amb la mitjana de la població.
\\item \\textbf{Fortalesa}: habilitat significativament elevada comparada amb el propi perfil de capacitats.
\\item \\textbf{Feblesa}: habilitat significativament inferior comparada amb el propi perfil de capacitats.
\\end{itemize}

\\section{Àrees avaluades}

El Test Àtom avalua la capacitat i l’execució de l’alumne en 6 o 7 àrees cognitives:

\\begin{itemize}
\\item \\textbf{Lectura (L)}: avalua la capacitat de descodificació i comprensió lectora dels alumnes.
\\item \\textbf{Memòria de Treball (MT)}: avalua l’habilitat per mantenir una informació a la memòria sense interferència d’altres processos de manipulació d’informació.
\\item \\textbf{Velocitat de Processament (VP)}: avalua l’agilitat que mostra la persona en explorar, ordenar i discriminar informació visual simple.
\\item \\textbf{Fluïdesa matemàtica (FM)}: avalua el procés d’automatització de les operacions numèriques bàsiques i l’habilitat per resoldre-les correctament.
\\item \\textbf{Memòria a llarg termini (MLT)}: avalua la capacitat d’evocació d’informació presentada i processada amb anterioritat.
\\item \\textbf{Raonament (R)}: avalua la destresa per inferir patrons lògics i deduir la seva continuïtat. 
\\item \\textbf{Càlcul (C)} (només a 5è i 6è de primària): avalua la capacitat de raonament quantitatiu a través d’operacions matemàtiques no automatitzades.
\\end{itemize}

\\section{Interpretació de l’informe}

L’informe del Test Àtom descriu el \\textbf{perfil de cada alumne} en habilitats cognitives i adaptació emocional, amb l’objectiu de comprendre les seves fortaleses i febleses i la seva comparació amb la població general. Consta de la informació referent a 4 aspectes: la \\textbf{valoració cognitiva} i de \\textbf{rendiment escolar}, l’índex d’\\textbf{estil de resposta}, la \\textbf{valoració adaptativa} i les \\textbf{orientacions personals i educatives}.

\\subsection{Interpretació dels resultats cognitius i de rendiment escolar}

Podem observar dos gràfics corresponents als resultats de l’alumne: el \\textbf{resultat observat} (comparant el rendiment de l'alumne amb el barem general de la població de la seva edat) i el \\textbf{resultat esperat} (predit segons el Mètode Òrbita d'anàlisi estadístic).

\\subsection{Interpretació del resultat observat o experimental}

En el primer gràfic es mostren els \\textbf{resultats observats o experimentals}, aquells \\textbf{obtinguts pel nen o la nena durant la realització de les diferents sub-proves}.

El codi de colors és el següent:

\\begin{itemize}
\\item Taronja: rendiment per sota del 30\\% de la població (equivalent a un baix rendiment en la prova).
\\item Verd: rendiment dins la normalitat.
\\item Blau: rendiment per sobre del 85\\% de la població (equivalent a un alt rendiment en la prova). 
\\end{itemize}

\\subsection{Interpretació del resultat esperat o predit}

En el segon gràfic mostrem els \\textbf{resultats predits o esperats}, que es basen en el Mètode Òrbita d’anàlisi estadístic. Aquest \\textbf{compara cada perfil amb el conjunt de perfils cognitius i d’adaptació emocional} de més de 2000 alumnes, i.e., es compara el resultat esperat per l’alumne a cadascuna de les proves amb el seu resultat obtingut. La diferència entre el valor esperat i els resultats obtinguts per l’alumne ens indica dificultats (en cas que els resultats predits siguin superiors als obtinguts) o potencialitats (en cas que els resultats predits siguin inferiors als obtinguts).

El codi de colors per aquests resultats és el següent:

\\begin{itemize}
\\item Vermell: el valor predit és significativament més alt que el mesurat (detectem una dificultat específica en aquella àrea).
\\item Gris: el valor predit és similar al mesurat.
\\item Blau: el valor predit és significativament més baix que el mesurat (detectem una fortalesa o potencialitat en aquella àrea).
\\end{itemize}

\\section{Índex d’estil de resposta}

Aporta informació sobre si el patró de resposta del subjecte durant la prova és impulsiu o reflexiu i permet matisar les inferències sobre el seu rendiment, situant cada alumne en un punt intermedi en el contínuum reflexivitat-impulsivitat.

Entenem com a \\textbf{reflexivitat} la tendència a respondre de forma lenta però exacta (alta latència de resposta i baix nombre d’errors), mentre que la \\textbf{impulsivitat} reflecteix un perfil que respon de forma ràpida però inexacta (baixa latència de resposta i elevat nombre d’errors).

Considerem que un estil de resposta tendent als extrems de reflexivitat i impulsivitat poden limitar l’eficiència de l’alumne i interferir en el seu rendiment habitual.

Aquesta informació només es mostra en cas que el valor observat pugui haver afectat els resultats en les altres àrees. 

\\section{Interpretació dels resultats d'adaptació social}

Proporciona un resultat de la \\textbf{percepció d’adaptació personal que té el nen o nena}, considerant les àrees més rellevants del seu dia a dia: personal, escolar, social i familiar. En el cas de Cicle Inicial es plantegen 5 preguntes que avaluen la percepció d’adaptació en els diversos àmbits associant la pregunta a una cara que representa l’estat emocional (cara plorosa, cara trista, cara contenta, cara molt contenta). En el cas de Cicle Mitjà i Cicle Superior es considera un qüestionari de 16 preguntes sobre la percepció d’adaptació a cada àmbit amb opcions de resposta: mai,alguna vegada, moltes vegades, sempre.

En el cas que detectem que l’alumne no es sent ben adaptat en alguna àrea obtindrem un gràfic que mostra el grau de desadaptació de l’individu. La longitud de les barres dels diferents àmbits indica la severitat de la inadaptació en aquella àrea i es divideix en 3 nivells: lleu, moderat i greu.

\\section{Orientacions personals i educatives}

Els resultats d’aquestes avaluacions ens indiquen en quines àrees generals detectem \\emph{necessitats específiques d’aprenentatge}. 

Segons els resultats, es configura una segona fase d’identificació personalitzada adaptada a les característiques de cada perfil. Per a cada nen o nena es suggereixen quin tipus de proves administrar a partir d’ara i quins passos a seguir en un futur considerant les necessitats detectades en aquest primer cribratge.

S’ofereix aquesta informació per donar l’oportunitat de continuar explorant el perfil cognitiu dels nens i nenes d’una forma raonada i útil enfocada a millorar el rendiment escolar i el desenvolupament personal.

Aquestes orientacions estan basades en el coneixement científic actual sobre habilitats cognitives, funcions executives, desenvolupament emocional i habilitats socials i adaptatives. 

\\section{Fonaments}

El Test Àtom és resultat d’investigació en cognició, psicologia infantil i intel·ligència artificial desenvolupada a la Universitat de Barcelona (UB) i la Universitat Pompeu Fabra (UPF), amb col·laboracions a la Universitats de Girona (UdG) i la Universitat Rovira i Virgili (URV). 

La vostra escola us pot proporcionar documentació proporcionada pel Projecte Òrbita sobre les teories que recolzen i configuren la base científica del Test Àtom.
", sep = "")}



#####

titol_classes <- function(escola, classe){
  
  wd = getwd()
  
  cat(paste0(
  "\\begin{titlepage}
  \\newcommand{\\HRule}{\\rule{\\linewidth}{0.5mm}} % Defines a new command for the horizontal lines, change thickness here
  \\center % Center everything on the page
  
  \\vspace*{3cm}
  
  \\textsc{\\LARGE Informe Test Àtom}\\\\[1.5cm] % Name of your university/college
  \\textsc{\\Large ", escola[1], "}\\\\[0.5cm] % Major heading such as course name
  
  \\HRule \\\\[0.4cm]
  { \\huge \\bfseries ", classe, "}\\\\[0.4cm] % Title of your document
  \\HRule \\\\[1.5cm]
  
  \\vspace{5cm}
  \\includegraphics[scale=0.3]{", wd, "/imatges/logo_orbita.png} % Include a department/university logo - this will require the graphicx package
  \\vfill % Fill the rest of the page with whitespace
  
  \\end{titlepage}"));
}

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

intro_part_individual <- function(){cat("

\\newpage

%\\section{PART II: Resultats individuals} 
\\section*{Resultats individuals} 

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

classe_grup_head <- function(nom_classe){cat("

\\textbf{", nom_classe, "} \\\\
Nota: a tots els gr\\`{a}fics la mitjana corresponent est\\`{a} indicada amb una ratlla horitzontal. \\\\",
 sep = "")};

grafics_collectius_per_materia <- function(prova, curs, escola){

wd = getwd()

cat("
%\\textbf{", prova[1], "}
\\begin{figure}[H]
\\centering
\\includegraphics[width=13cm]{", wd, "/temp/figures/", curs[1], "/", prova[3], "-", curs[2], "-norm}
\\end{figure}",

if(1==0){cat(observacions-1)},
"%La prova no \\'{e}s v\\`{a}lida pel Josep ja que no va seguir b\\'{e} les instruccions de la tasca. \\
%La Nerea no va passar p\\`{a}gina, de manera que els seus resultats reals podrien ser superiors als evidenciats en aquesta tasca. \\\\

\\begin{figure}[H]
\\centering
\\includegraphics[width=13cm]{", wd, "/temp/figures/", curs[1], "/", prova[3], "-", curs[2], "-norm_intra}
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

group_head_classes <- function(classe, escola){
  cat("
      \\vspace{1.1cm}
      \\section*{Informes col·lectius}
      
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
\\textbf{", nom, "} \\hfill \\textbf{Classe ", classe, "} \\hfill \\textbf{", escola, "}
\\end{framed}

", sep = "")}

individual_sol_head <- function(nom, nom_curs){
  cat("
      \\newpage
      
      \\begin{framed}
      \\textbf{", nom, "} \\\\
      \\textbf{", nom_curs, "} \\\\
      \\end{framed}
      
      ", sep = "")}

individual_antic <- function(index, curs, punts, matrius, indeximp, escola){

wd = getwd()
  
cat("
\\vspace{1.2cm}


\\begin{figure}[H]
\\captionsetup[subfigure]{labelformat=empty}
\\begin{subfigure}{.5\\textwidth}
\\centering
\\includegraphics[width=7.5cm]{", wd, "/temp/figures/", curs[1], "/", index, "-norm}
\\end{subfigure}
\\begin{subfigure}{.5\\textwidth}
\\centering
\\includegraphics[width=7.5cm]{", wd, "/temp/figures/", curs[1], "/", index, "-comp}
\\end{subfigure}
%\\caption*{En el gràfic de l'esquerra veiem els resultats \\emph{mesurats} i a la dreta els \\emph{esperats}. Si hi ha resultats en vermell al gràfic de la dreta és perquè mesurem aquella habilitat més \\emph{baixa} que la predida, i per tant parlem d'una possible \\emph{dificultat específica}. En canvi, si estan en blau és perquè són més \\emph{alts} dels predits i per tant parlem d'un possible \\emph{talent}.}
\\caption*{Al Gràfic 1 veiem els resultats obtinguts per l'alumne en comparació amb el barem universal de referència. El color verd indica que l'alumne es troba dins la mitjana estadística, mentre que el taronja indica que es troba significativament per sota d'aquesta (indicant una possible dificultat específica) i el blau que es troba a la franja superior (informant d'un possible talent). Al Gràfic 2 observem els resultats esperats per l'alumne segons la seva velocitat de processament, segons els resultats obtinguts a partir de l'Índex de Rapidesa mitjançant el Mètode Òrbita d'anàlisi estadístic. El color gris indica que no hi ha discrepància entre la puntuació obtinguda i la esperada, mentre que el vermell significa que el rendiment en aquesta habilitat és inferior a l'esperat (i per tant parlem d'un punt feble en el seu perfil intern) mentre que el blau informa d'una habilitat superior a la predita (i parlaríem d'un punt fort en el seu perfil intern).}
    
\\end{figure}", sep="")

  #punts[is.na(punts[,ncol(punts)])]<-"";
  if(!is.na(punts[index,ncol(punts)]) && as.character(punts[index,ncol(punts)])!=""){
    text = as.character(punts[index,ncol(punts)]);
    cat("\\textbf{Nota:} durant l'administració o la correcció del test s'ha anotat que ", text, ", fet pot afectar els resultats que observem i les conclusions que en traiem.", sep="")
  }

cat("
\\newpage
\\begin{center}
\\Large{\\textbf{Interpretació dels resultats cognitius i de rendiment}} \\\\ 
\\end{center}
", sep = "");



informe_matrius(matrius[[as.character(names(matrius)[index])]], names(matrius)[index]);

if(!is.na(indeximp[index]) & indeximp[index]>0.18){cat("
Es valora que l'estil de resposta fortament tendent a la \\emph{impulsivitat} pot haver interferit negativament en els resultats obtinguts, havent acumulat una quantitat d'errors que no permet saber la seva capacitat real d'execució.")}

if(!is.na(indeximp[index]) & indeximp[index]<0.09){cat("
Es valora que l'estil de resposta fortament tendent a la \\emph{reflexivitat} pot haver interferit negativament en els resultats obtinguts, havent buscat minimitzar la quantitat d'errors de forma forçada i per tant mostrant un rendiment inferior a la seva capacitat real d'execució.")}

cat("

\\begin{center}
\\Large{\\textbf{Interpretació dels reusltats d'adaptació social}} \\\\
\\end{center}

");

########

# aquí hi ha d'anar el gràfic d'emocional (si cal)
destfile = paste0( wd, "/temp/figures/", curs[1],  "/emocional-", index, ".pdf")
if (file.exists(destfile)){

cat("
    \\begin{figure}[H]
    \\centering
    \\includegraphics[width=7.5cm]{", wd, "/temp/figures/", curs[1], "/emocional-", index, ".pdf}
    \\end{figure}", sep = ""
    )
}

#######

if(curs[2] > 2){futur_em <- informe_adaptatiu(index, punts);}
else{futur_em <- informe_emocional_petits(index, punts)};

cat("



\\begin{center}
\\Large{\\textbf{Orientacions}} \\\\
\\end{center} 

");
futur_tier <- informe_tier2(matrius[[index]], names(matrius)[index]);
if(futur_em == TRUE){cat("Es constaten alteracions emocionals que poden haver interferit en l'execució cognitiva i de rendiment exposada anteriorment. Per tant recomanem que es procedeixi a aprofundir en les causes d'aquestes alteracions emocionals abans de continuar l'exploració del perfil cognitiu.\\\\ ") 
}else{cat("No es constata interferència de l'estat emocional en els resultats obtinguts. \\\\")};

if(futur_tier == TRUE){cat("Els resultats obtinguts ens permeten determinar que ", names(matrius)[index], " pot beneficiar-se de mesures metodològiques destinades a abordar les àrees prèviament comentades. ");
}

}


informe_individual <- function(index, curs, punts, matrius, indeximp, escola, tipus){
  
  wd = getwd()
  
  cat("
      \\begin{center}
      \\Large{\\textbf{Resultats cognitius i adaptatius}} \\\\ 
      \\end{center}
      ", sep = "");
  
  cat("
      %\\vspace{1.2cm}
      
      \\begin{figure}[H]
      \\captionsetup[subfigure]{labelformat=empty}
      \\begin{subfigure}{.5\\textwidth}
      \\centering
      \\includegraphics[width=7.5cm]{", wd,"/temp/figures/", curs[1], "/", index, "-norm}
      \\end{subfigure}
      \\begin{subfigure}{.5\\textwidth}
      \\centering
      \\includegraphics[width=7.5cm]{", wd,"/temp/figures/", curs[1], "/", index, "-comp}
      \\end{subfigure}
      %\\caption*{En el gràfic de l'esquerra veiem els resultats \\emph{mesurats} i a la dreta els \\emph{esperats}. Si hi ha resultats en vermell al gràfic de la dreta és perquè mesurem aquella habilitat més \\emph{baixa} que la predida, i per tant parlem d'una possible \\emph{dificultat específica}. En canvi, si estan en blau és perquè són més \\emph{alts} dels predits i per tant parlem d'un possible \\emph{talent}.}
      \\caption*{\\emph{Trobareu informació sobre els gràfics al document d'introducció dels informes.}}
      \\end{figure}", sep="")
  
  if(!is.na(punts[index,ncol(punts)]) && as.character(punts[index,ncol(punts)])!=""){
    text = as.character(punts[index,ncol(punts)]);
    cat("\\textbf{Nota:} durant l'administració o la correcció del test s'ha anotat que ", text, ", fet pot afectar els resultats que observem i les conclusions que en traiem.", sep="")
  }
  
  proves_cap <- c("Lectura", "Memòria de treball", "Velocitat de processament visual", "Fluïdesa matemàtica", "Memòria a llarg termini", "Raonament", "Càlcul");
  taula_prova(matrius[[as.character(names(matrius)[index])]], proves_cap);
  
  
  # aquí hi ha d'anar el gràfic d'emocional (si cal)
  destfile = paste0(wd, "/temp/figures/", curs[1],  "/emocional-", index, ".pdf")
  if (file.exists(destfile)){

      cat("
        \\begin{figure}[H]
        \\centering
        \\includegraphics[width=7.5cm]{", wd,"/temp/figures/", curs[1], "/emocional-", index, ".pdf}
        \\end{figure}", sep = ""
      )
    
  }
  
cat("
      \\newpage
      \\begin{center}
      \\Large{\\textbf{Interpretació dels resultats}} \\\\
      \\Large{Prova cognitiva i de rendiment} \\\\ 
      \\end{center}
      
      ");
  
  ########
informe_matrius(matrius[[as.character(names(matrius)[index])]], names(matrius)[index]);

inpulsivitat_reflectivitat(indeximp, index)  ## aquesta funció és a informe_matrius.R

cat("
      
      \\begin{center}
      \\Large{Prova adaptativa} \\\\ 
      \\end{center}
      
      ");

  
  #######
  
  if(curs[2] > 2){futur_em <- informe_adaptatiu(index, punts);}
  else{futur_em <- informe_emocional_petits(index, punts)};
  
  cat("
      \\begin{center}
      \\Large{\\textbf{Orientacions}} \\\\
      \\end{center} 
      
      ");

  futur_tier <- informe_tier2(matrius[[index]], names(matrius)[index]);
  if(futur_em == TRUE){cat("Es constaten alteracions emocionals que poden haver interferit en l'execució cognitiva i de rendiment exposada anteriorment. Per tant recomanem que es procedeixi a aprofundir en les causes d'aquestes alteracions emocionals abans de continuar l'exploració del perfil cognitiu.\\\\ ") 
  }else{cat("No es constata interferència de l'estat emocional en els resultats obtinguts. \\\\")};
  
  if(futur_tier == TRUE){cat("\\\\Els resultats obtinguts ens permeten determinar que ", names(matrius)[index], " pot beneficiar-se de mesures metodològiques destinades a abordar les àrees prèviament comentades. ");
  }
  
  }


informe_individual_alumnes_sol <- function(index, curs, punts, matrius, indeximp, escola){
  
  
  cat("
      \\vspace{1.2cm}
      
      
      \\begin{figure}[H]
      \\captionsetup[subfigure]{labelformat=empty}
      \\begin{subfigure}{.5\\textwidth}
      \\centering
      \\includegraphics[width=7.5cm]{./norm.pdf}
      \\end{subfigure}
      \\begin{subfigure}{.5\\textwidth}
      \\centering
      \\includegraphics[width=7.5cm]{./comp.pdf}
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
      \\Large{\\textbf{Interpretació dels resultats cognitius i de rendiment}} \\\\ 
      \\end{center}
      ", sep = "");
  
  
  
  informe_matrius(matrius[[as.character(names(matrius)[index])]], names(matrius)[index]);
  
  if(!is.na(indeximp[index]) & indeximp[index]>0.18){cat("
                                                         Es valora que l'estil de resposta fortament tendent a la \\emph{impulsivitat} pot haver interferit negativament en els resultats obtinguts, havent acumulat una quantitat d'errors superior a la seva capacitat real d'execució.")}
  
  if(!is.na(indeximp[index]) & indeximp[index]<0.09){cat("
                                                         Es valora que l'estil de resposta fortament tendent a la \\emph{reflexivitat} pot haver interferit negativament en els resultats obtinguts, havent buscat minimitzar la quantitat d'errors i mostrant un rendiment inferior a la seva capacitat real d'execució.")}
  
  cat("
      
      \\begin{center}
      \\Large{\\textbf{Interpretació dels resultats d'adaptació social}} \\\\
      \\end{center}
      
      ");
  
  if(curs[2] > 2){futur_em <- informe_adaptatiu(index, punts);}
  else{futur_em <- informe_emocional_petits(index, punts)};
  
  cat("
      
      
      
      \\begin{center}
      \\Large{\\textbf{Orientacions}} \\\\
      \\end{center} 
      
      ");
  futur_tier <- informe_tier2(matrius[[index]], names(matrius)[index]);
  if(futur_em == TRUE){cat("Es constaten alteracions emocionals que poden haver interferit en l'execució cognitiva i de rendiment exposada anteriorment. Per tant recomanem que es procedeixi a aprofundir en les causes d'aquestes alteracions emocionals abans de continuar l'exploració del perfil cognitiu.\\\\ ") 
  }else{cat("No es constata interferència de l'estat emocional en els resultats obtinguts. \\\\")};
  
  if(futur_tier == TRUE){cat("Els resultats obtinguts permeten determinar que ", names(matrius)[index], " pot beneficiar-se de mesures metodològiques destinades a abordar les àrees prèviament comentades. ");
  }
  
      }
