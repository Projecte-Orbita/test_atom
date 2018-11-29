Sys.setlocale(category="LC_ALL", locale = "Catalan")

informe_tier2 <- function(matriu, name){

  require(psych);
  
futur=FALSE;  

  proves <- c("\\textbf{Lectura}", "\\textbf{Memòria de treball}", "\\textbf{Velocitat de processament visual}", "\\textbf{Fluïdesa matemàtica}", "\\textbf{Memòria a llarg termini}", "\\textbf{Raonament}", "\\textbf{Càlcul}");
  paraules <- list()
  paraules[[1]] = c('\\textbf{Lectura}', 'descodificació, de comprensió oral i escrita i de coneixement general de la llengua', 'descodificació, de comprensió oral i escrita i de coneixement general de la llengua, així com d\'hàbits lectors', 'a la Lectura');
  paraules[[2]] = c('\\textbf{Memòria de Treball}', 'capacitat atencional, memòria a curt termini, i més proves específiques de memòria de treball','capacitat atencional, memòria a curt termini, i més proves específiques de memòria de treball', 'a la Memòria de Treball');
  paraules[[3]] = c('\\textbf{Velocitat de Processament Visual}', 'percepció visual i de velocitat de processament però que no requereixin estímuls visuals','percepció visual i de velocitat de processament però que no requereixin estímuls visuals', ' a la Velocitat de Preocessament Visual');
  paraules[[4]] = c('\\textbf{Fluïdesa Matemàtica}', "de coneixement i automatització de les operacions aritmètiques bàsiques, percepció de magnituds i comparacions","  de raonament matemàtic i abstracte i habilitats matemàtiques avançades", 'a la Fluïdesa Matemàtica');
  paraules[[5]] = c('\\textbf{Memòria a Llarg Termini}', 'capacitat atencional, memòria a curt termini, i més proves específiques de memòria a llarg termini','capacitat atencional, memòria a curt termini, i més proves específiques de memòria a llarg termini', 'a la Memòria a Llarg Termini');
  paraules[[6]] = c('\\textbf{Raonament}', 'resolució de problemes i de raonament en diferents formes: verbal, matemàtic i abstracte','resolució de problemes i de raonament en diferents formes: verbal, matemàtic i abstracte', 'al Raonament');
  paraules[[7]] = c('\\textbf{Càlcul}', 'fluïdesa matemàtica i raonament matemàtic i abstracte','fluïdesa matemàtica, raonament matemàtic i abstracte i habilitats matemàtiques avançades', 'al càlcul');
  
  

if(prod(diag(matriu))==2.1**nrow(matriu)){
  
  cat("Tots els paràmetres entren dins la normalitat i per tant no és necessari continuar avaluant a nivell cognitiu. \\\\
      ", sep="")
} else {
    cat("Aquests resultats ens donen una idea general del perfil cognitiu i emocional del/de la ", name ," però per tal de tenir un coneixement més acurat de les seves necessitats educatives necessitem aprofundir en el seu perfil. Els resultats del/de la ", name ," indiquen doncs que les àrees a avaluar més profundament són les següents: 
        
        \\begin{itemize}
        
        ");
      
      # comencem mirant si la velocitat de processament és baixa, ja que això determina
      # tota la resta: 
      
      if(round(matriu[3,3])==1 & round(matriu[4,4])==1){
        cat("\\item Hem observat que l' \\textbf{Índex de Rapidesa} és \\textbf{baix} en el cas del/de la ", name, " i per tant això ens condiciona els altres resultats. Així doncs, l'acció més immediata a prendre és determinar-ne les causes. Per tant, des del Projecte Òrbita recomanem l'administració de proves específiques de funcions executives, en concret de proves atencionals per tal de detectar possibles afectacions de la capacitat de concentració i de la memòria a curt termini. A més a més, també recomanem l'administració de proves d'estat emocional i adaptatiu i de funció motora per tal de descartar que hi hagi factors externs als purament cognitius que ens puguin estar afectant el resultat.
            ", sep="")
        future = TRUE
      }
  
  # fem les memòries per separat:
  if(round(matriu[2,2])==1 & round(matriu[5,5])==1){
    cat("\\item Observem que els resultats en les dues proves de \\textbf{memòria} són substancialment \\textbf{baixos}, així que creiem que podem estar parlant d'una dificultat específica en aquesta àrea. Per tant, des del Projecte Òrbita recomanem l'administració de proves específiques de funcions executives, en concret de proves de capacitat atencional, memòria a curt termini, així com més proves específiques de memòria de treball i a llarg termini.
        ", sep="")
    future = TRUE
  }
  else if(round(matriu[2,2])==1){
    cat("\\item Observem que els resultats en la \\textbf{memòria de treball} són \\textbf{baixos}, però en canvi en la memòria a llarg termini no, per tant pensem que es deu a una manca d'atenció durant la prova i no cal fer-hi més incís.
        ", sep="")
    future = TRUE
  }
  else if(round(matriu[5,5])==1){
    cat("\\item Observem que els resultats en la memòria de treball són normals, però en canvi en la \\textbf{memòria a llarg termini} són baixos, per tant podríem estar parlant d'una dificultat específica en memòria a llarg termini. Per tant, des del Projecte Òrbita recomanem l'administració de proves específiques de memòria a llarg termini per tal d'aprofundir en aquesta dificultat.
        ", sep="")
    future = TRUE
  }
  else if(round(matriu[2,2])==3 & round(matriu[5,5])==3){
    cat("\\item Observem que els resultats en les dues proves de \\textbf{memòria} són \\textbf{alts}, així que podem estar parlant d'un talent específic en aquesta àrea. Per tant, des del Projecte Òrbita recomanem l'administració de proves de memòria a curt termini, així com més proves específiques de memòria de treball i a llarg termini per acabar de confirmar aquest talent. 
        ", sep="")
  }
  else if(round(matriu[2,2])==3){
    cat("\\item Observem que els resultats en la \\textbf{memòria de treball} són \\textbf{alts}, així que podem estar parlant d'un talent específic en aquesta àrea. Per tant, des del Projecte Òrbita recomanem l'administració de proves de memòria a curt termini, així com més proves específiques de memòria de treball i també a llarg termini per acabar de confirmar aquest talent i tornar a comprovar si s'estén també a memòria a llarg termini.
        ", sep="")}
  else if(round(matriu[5,5])==3){
    cat("\\item Observem que els resultats en la \\textbf{memòria a llarg termini} són \\textbf{alts}, així que podem estar parlant d'un talent específic en aquesta àrea. Per tant, des del Projecte Òrbita recomanem l'administració de proves de memòria a curt termini, així com més proves específiques de memòria de treball i també a llarg termini per acabar de confirmar aquest talent i tornar a comprovar si s'estén també a memòria de treball.
        ", sep="") }
    
  nums = 1:nrow(matriu)
#  nums = nums[-c(2,5)]
     
  for (j in nums){
    
    if(round(matriu[j,j])==0){cat("\\item No tenim resultats l'àrea de ", paraules[[j]][1], "  i per tant s'hauria de re-avaluar.
                                  ", sep = "")}
    
    if (j == 2 | j==5){next}
    
      if (round(matriu[j,j]) == 1){cat("\\item En ", paraules[[j]][1],sep="")};
      if (matriu[j,j] == 1.0){cat(" hem observat que els resultats són més \\textbf{baixos} que els predits per l'anàlisi Òrbita, i per tant podríem estar parlant d'una dificultat específica, i necessitem aprofundir en les seves causes. És per això que recomanem que s'administrin proves de ", paraules[[j]][2],".
                                  ",sep="");
futur=TRUE;
};
      if (matriu[j,j] == 1.1){cat(" els resultats són \\textbf{baixos}, però l'anàlisi del perfil ens indica que probablement no estem parlant d'una dificultat específica, i per tant, tot i que sí que necessita una atenció personalitzada en aquest àmbit, no creiem que l'acció més immediata en aquest punt sigui continuar aprofundint en les habilitats subjacents ", paraules[[j]][4],". 
                                  ",sep="")};
      if (matriu[j,j] == 1.2){cat(" tot i que aquest valor és superior a l'esperat, no es necessita un aprofundiment en el coneixement de les habilitats subjacents, però sí una atenció individualitzada a l'aula.
                                  ",sep="")};
      
      if (matriu[j,j] == 2.0){cat("\\item En el cas de la ", paraules[[j]][1],", tot i observar valors dins la normalitat, veiem que els resultats són \\textbf{significativament inferiors} als predits per l'anàlisi Òrbita, i per tant podríem estar parlant d'una possible dificultat específica en aquest àmbit. És per això que recomanem que s'administrin proves de ", paraules[[j]][2],".", sep="");

futur=TRUE;};

      if (matriu[j,j] == 2.2){cat("\\item En el cas de la ", paraules[[j]][1],", tot i observar valors dins la normalitat, veiem que els resultats són \\textbf{significativament superiors} als predits per l'anàlisi Òrbita, i per tant podríem estar parlant d'un talent en aquest àmbit. És per això que són recomanables proves de ", paraules[[j]][3], ".",
                                  sep="")};
      
      if (round(matriu[j,j]) == 3){cat("\\item En ", paraules[[j]][1]," hem vist que s'observen resultats molt \\textbf{alts}. ", sep="")};
      if (matriu[j,j] == 3.0){cat("Aquests són significativament inferiors als predits per l'anàlisi Òrbita; tot i així aquest fet no és significatiu i no requereix de més acció.
                                  ",sep="")};
      if (matriu[j,j] == 3.1){cat("Aquests són els esperats per l'anàlisi Òrbita, però tot i així podríem estar parlant de rendiment molt alt, i pot ser convenient l'administració de proves de ", paraules[[j]][1],". És per això que reconamen l'administració de proves de ", paraules[[j]][3], " per tal d'aprofundir en les habilitats relacionades amb ", paraules[[j]][4], ".
                                  ",sep="")};
      if (matriu[j,j] == 3.2){cat("A més a més, sospitem un talent en la ", paraules[[j]][1],". És per això que reconamen l'administració de proves de ", paraules[[j]][3], " per tal de confirmar o descartar aquest talent.
                                  ",sep="")};
  }
  
       cat("
           
           \\end{itemize}
           

           ");
       
       
  dm <- diag(matriu);
       
       if (length(dm[dm >= 3]==TRUE) >  3 && length(dm[dm<2]==TRUE) != 0 ){
#if(prod(diag(matriu))>=3**(nrow(matriu)-3)*2**3){
  
  cat("Observem que els resultats del/de la ", name, " són molt \\textbf{alts} en la majoria de les habilitats, fet que ens porta a pensar que podriem trobar-nos davant d'un cas d'altes capacitats. Recomanem doncs que s'aprofundeixi en l'avaluació psicopedagògica global per tal d'entendre les seves necessitats específiques.\\\\", sep="")
}
       
  # if(tr(matriu) <= 5+(nrow(matriu)-3)*2){
       if (length(dm[dm<2]==TRUE) >  3 && length(dm[dm >= 3]==TRUE)==0 ){       
    cat("Observem que els resultats del/de la ", name, " són \\textbf{baixos} en diverses habilitats, fet que ens porta a pensar que podríem trobar-nos davant de dificultats generalitzades en l'aprenentatge. Recomanem doncs que es realitzi una avaluació psicopedagògica global per tal d'aprofundir en les causes d'aquestes dificultats in entendre les seves necessitats específiques.\\\\", sep="")
       }  
      # si TRUE s'han de posar frases al final
       return(futur); 
}; 
  return(futur)}

