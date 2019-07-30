# Poso aquí les funcions de l'adaptatiu emocional, que són la nova versió del que fins ara anomenàvem emocional,
# però que canvio de nom perquè no es confongui amb històries de teoria de la ment i tal


Sys.setlocale(category="LC_ALL", locale = "Catalan")


informe_adaptatiu <- function(index, emocional){  
  #
  futur=FALSE;
  lleus=0;
  
  emocional_nou <- as.character(emocional[index,1]);
  cc=c(1:9,9,10,10:21,21,22,22);
  
  for (i in cc){
    emocional_nou = c(emocional_nou, emocional[index,i+1])
  }
  
  
  autoimatge <- list("\\textbf{d'autoimatge i autoconcepte}", 
                   c("li agrada la seva personalitat", 
                     "està a gust amb el seu cos", 
                     "aconsegueix el que es proposa", 
                     "li passen coses bones"), 
                   list(c(1,2), c(1,2), c(1,2), c(1,2)));
  
  clinica <- list("\\textbf{de simptomatologia clínica}", 
                  c("es preocupa", 
                    "té mal de cap o de panxa", 
                    "està trist/a", 
                    "s'enfada o es baralla"), 
                  list(c(4,3), c(4,3), c(4,3), c(4,3)));
  
  escolar <- list("\\textbf{escolar}", 
                  c("li agrada anar a l'escola", 
                    "li agrada anar a l'escola", 
                    "s'avorreix a classe", 
                    "s'avorreix a classe", 
                    "treu bones notes", 
                    "estudia i s'esforça"), 
                  list(c(NA,1), c(NA,2), c(NA,4), c(NA,3), c(NA,1), c(NA,1)));
  
  
  social <- list("\\textbf{social}", 
                   c("li cauen bé els seus companys", 
                     "els altres el/la molesten", 
                     "li agrada jugar amb els i les altres", 
                     "li costa fer amics o amigues"), 
                 list(c(1,2), c(4,3), c(1,2), c(4,3)));
  
  familiar <- list("\\textbf{familiar}", 
                 c("està a gust a casa", 
                   "hi ha crits i discussions a casa", 
                   "està d'acord amb les normes de casa", 
                   "està d'acord amb les normes de casa", 
                   "se sent bé amb la seva família", 
                   "se sent bé amb la seva família"), 
                 list(c(1,2), c(4,3), c(NA,2), c(NA,1), c(1,5), c(2,NA)));
  
  #c(12,NA) c(12,3)
  
  afectat <- 0
  
  ambits <- list(autoimatge, clinica, escolar, social, familiar);
  
  for(i in c(1,5,9,15,19)){
    
    greu <- NULL;
    lleu <- NULL;
    nb_pregs <- NULL;
    
    if(i != 19 | i != 11){nb_pregs <- seq(1,4,1)} else {nb_pregs <- seq(1,6,1)};
    
    for(j in nb_pregs){  
      
      if (!is.na(ambits[[(i-1)/4+1]][[3]][[j]][1]) && !is.na(emocional_nou[i+j]) 
          && emocional_nou[i+j] == ambits[[(i-1)/4+1]][[3]][[j]][1])
      {greu <- c(greu, list(paraula(ambits[[(i-1)/4+1]][[3]][[j]][1]), j));}
      
      else if (!is.na(ambits[[(i-1)/4+1]][[3]][[j]][2]) && !is.na(emocional_nou[i+j]) 
               && emocional_nou[i+j]== ambits[[(i-1)/4+1]][[3]][[j]][2])
      {lleu <- c(lleu, list(paraula(ambits[[(i-1)/4+1]][[3]][[j]][2]), j));}}
    
    if(length(greu) != 0 | length(lleu) != 0)
    {cat("
         
         \\textbf{\\`{A}mbit} ", ambits[[(i-1)/4+1]][[1]], ": 
         \\begin{itemize}
         ", sep = "");
      afectat <- afectat + 1}
    
    if(length(greu) != 0){cat("\\item Factors de \\emph{risc greu}: El/la ", 
                              as.character(emocional_nou[1]), " ha indicat que ", sep = "");               
      if(length(greu) != 2){sapply(1:(floor(length(greu)/2)-1), 
                                   function (x) cat(greu[[2*x-1]], 
                                                    ambits[[(i-1)/4+1]][[2]][as.numeric(greu[[2*x]])], ", ", sep = ""));}
      
      cat(if(length(greu) != 2)"i ", greu[[as.numeric(length(greu)-1)]], 
          ambits[[(i-1)/4+1]][[2]][as.numeric(greu[[length(greu)]])], ". ", sep = ""); 
      futur = TRUE;
    }
    
    
    if(length(lleu) != 0){cat("\\item Factors de \\emph{risc lleu}: El/la ", 
                              as.character(emocional_nou[1]), " ha indicat que ", sep = "");               
      if(length(lleu) != 2){sapply(1:(floor(length(lleu)/2)-1), 
                                   function (x) cat(lleu[[2*x-1]], 
                                                    ambits[[(i-1)/4+1]][[2]][as.numeric(lleu[[2*x]])], ", ", sep = ""));}
      
      cat(if(length(lleu) != 2)"i ", lleu[[as.numeric(length(lleu)-1)]], 
          ambits[[(i-1)/4+1]][[2]][as.numeric(lleu[[length(lleu)]])], ". ", sep = ""); 
      lleus <- lleus + 1;
    }
    

    
    if(length(greu) != 0 | length(lleu) != 0)
      cat("
          \\end{itemize}")
    
    if (length(greu) == 0 && length(lleu)/2 > 2)
      cat("
    Donat que hi ha 3 o més factors afectats de forma lleu en aquest àmbit, considerem que l'hem de tractar com un \\emph{risc greu}.
    \\newline
")
  }
  
  if(afectat == 0){cat("No es constaten factors de risc de desadaptació emocional.")}
  
  if(lleus>2){futur=TRUE};
  return(futur);
}

creacio_grafics_adaptatiu = function(punts, curs, escola){
  for(i in 1:length(punts[,1])){
    
    nom = as.character(punts[i,1])
    
    if(curs[2] == 1 | curs[2] == 2){
      
      # TODO: Primer i segon, encara no estan implementats
      
      dades = as.numeric(as.vector(t(punts[i, c(14:18)])))
      dades[is.na(dades)]=4  # Els no respostos és com si estessin bé.
      # poseu a 1 i no volem que no ho estiguin.
      valors = c(if(dades[1]==1) 3 else {if(dades[1]==2) 1 else 0},
                 if(dades[2]==1) 3 else {if(dades[2]==2) 1 else 0},
                 if(dades[3]==1) 3 else {if(dades[3]==2) 1 else 0},
                 min(if(dades[4]==1) 3 else {if(dades[4]==2) 1 else 0} + 
                       if(dades[5]==1) 3 else {if(dades[5]==2) 1 else 0}, 3)
      )
    }
    
    else {
      if(curs[2] == 3 | curs[2] == 4){
        dades = unname(unlist(punts[i, c(18:37)]))
        }
      else if(curs[2] == 5 | curs[2] == 6){
        dades = unname(unlist(punts[i, c(24:43)]))
      }
      else {
        print("No entenc el curs. Abortant.")
        break
      }
    
      dades[is.na(dades)]=0
      
      pre_valors = c(
        # 1. Autoimatge i autoconcepte
        
        if(dades[1]==1) 3 else {if(dades[1]==2) 1 else 0},        # M'agrada la meva personalitat
        if(dades[2]==1) 3 else {if(dades[2]==2) 1 else 0},        # M'agrada el meu cos
        if(dades[3]==1) 3 else {if(dades[3]==2) 1 else 0},        # Aconsegueixo el que em proposo
        if(dades[4]==1) 3 else {if(dades[4]==2) 1 else 0},        # Em passen coses bones
        
        # 2. Simptomatologia clínica
        
        if(dades[5]==4) 3 else {if(dades[5]==3) 1 else 0},        # Em preocupo
        if(dades[6]==4) 3 else {if(dades[6]==3) 1 else 0},        # Tinc mal de cap o mal de panxa
        if(dades[7]==4) 3 else {if(dades[7]==3) 1 else 0},        # Estic trist
        if(dades[8]==4) 3 else {if(dades[8]==3) 1 else 0},        # M’enfado o em barallo
        
        # 3. Satisfacció escolar
        
        if(dades[9]==1) 1 else {if(dades[9]==2) 1 else 0},        # m'agrada anar a l'escola
        if(dades[10]==3) 1 else {if(dades[10]==4) 1 else 0},      # m'avorreixo a classe
        if(dades[11]==1) 1 else 0,                                # Trec bones notes
        if(dades[12]==4) 1 else 0,                                # Estudio poc i m'esforço poc 
        
        # 4. Satisfacció social
        
        if(dades[13]==1) 3 else {if(dades[13]==2) 1 else 0},      # Em cauen bé els meus companys
        if(dades[14]==4) 3 else {if(dades[14]==3) 1 else 0},      # Els altres em molesten
        if(dades[15]==1) 3 else {if(dades[15]==2) 1 else 0},      # M'agrada jugar amb els altres
        if(dades[16]==4) 3 else {if(dades[16]==3) 1 else 0},      # Em costa fer amics
        
        # 5. Satisfacció familiar
        
        if(dades[17]==1) 3 else {if(dades[17]==2) 1 else 0},       # Estic a gust a casa
        if(dades[18]==4) 3 else {if(dades[18]==3) 1 else 0},       # hi ha crits i discussions a casa
        if(dades[19]==1) 1 else {if(dades[19]==2) 1 else 0},      # estic d'acord amb les normes de casa
        if(dades[20]==1 | dades[20]==2) 3 else {if(dades[20]==3) 1 else 0}  # Em sento bé amb la meva família
         
      )
      
      valors = c()
      for (j in 1:5){
        valors[j] = sum(pre_valors[((j-1)*4 + 1):(j*4)])
      }
      
    }
    
    arees = c("Autoimatge i autoconcepte", "Simptomatologia clínica", "Satisfacció escolar", 
              "Satisfacció social", "Satisfacció familiar")
    
    df_emocional = as.data.frame(cbind(arees, valors))
    
    valors_nets = valors
    valors_nets[is.na(valors)]=0 # Aquesta línia i l'anterior són per tractar missings
    # TODO: arreglar-ho més amunt i millor
    if (sum(valors_nets)==0) next
    
    grafic_emocional(i, curs, df_emocional, escola, nom)
    
  }
}