Sys.setlocale(category="LC_ALL", locale = "Catalan")


taula_prova <- function(matriu, proves){
  
  cat("\\begin{center}
      \\begin{tabular}{|l|c|c|}
      \\hline
      Habilitat & Barem universal & Comparativa amb el resultat esperat \\\\
      \\hline
      \\hline
      ")
  
  for (j in 1:nrow(matriu)){
    cat("\\textbf{", proves[j], "} & ");   
    if (round(matriu[j,j])==0){cat("no l'ha fet & no l'ha fet \\\\")}
    
    if (round(matriu[j,j]) == 1){cat("\\textbf{valors baixos} & ")};
    
    if (matriu[j,j] == 1.0){cat("\\textbf{valors baixos} \\\\ ")};
    if (matriu[j,j] == 1.1){cat("valors esperats \\\\")};
    if (matriu[j,j] == 1.2){cat("\\textbf{valors alts} \\\\ ")};
    
    if (round(matriu[j,j]) == 2){cat("valors mitjans & ")};
    
    if (matriu[j,j] == 2.0){cat("\\textbf{valors baixos} \\\\ ")};
    if (matriu[j,j] == 2.1){cat("valors esperats \\\\")};
    if (matriu[j,j] == 2.2){cat("\\textbf{valors alts} \\\\ ")};
    
    
    if (round(matriu[j,j]) == 3){cat("\\textbf{valors alts} & ")};
    
    if (matriu[j,j] == 3.0){cat("\\textbf{valors baixos} \\\\ ")};
    if (matriu[j,j] == 3.1){cat("valors esperats \\\\")};
    if (matriu[j,j] == 3.2){cat("\\textbf{valors alts} \\\\ ")};
    cat(" \\hline
        ");}
  cat("\\end{tabular}
      \\end{center}
      ");   
  }

informe_matrius <- function(matriu, name){

proves <- c("Lectura", "Memòria de Treball", "Velocitat de Processament Visual", "Fluïdesa Matemàtica", "Memòria a Llarg Termini", "Raonament", "Càlcul");

cat("Considerant els resultats del/la", name, ":
\\begin{itemize}
");
    for (j in 1:nrow(matriu)){   
        if (round(matriu[j,j]) == 1){cat("
\\item S'observen valors \\textbf{baixos} respecte la mitjana en l'àrea de \\textbf{", proves[j], "}; ", sep = "")};
        if (matriu[j,j] == 1.0){cat("tenint en compte l'anàlisi Òrbita, s'evidencien \\textbf{febleses} significatives respecte els resultats esperats. ")};
        #if (matriu[j,j] == 1.1){cat("tenint en compte l'anàlisi Òrbita, els resultats són dins els valors esperats. ")};
        if (matriu[j,j] == 1.2){cat("tenint en compte l'anàlisi Òrbita, s'evidencia un resultat superior respecte els valors esperats. ")};

        if (round(matriu[j,j]) == 2)
{cat("\\item S'observen valors dins la normalitat en l'àrea de \\textbf{", proves[j], "}; ",  sep = "")};
         if (matriu[j,j] == 2.0){cat("tenint en compte l'anàlisi Òrbita, s'evidencien \\textbf{febleses} significatives respecte els resultats esperats. ")};
        #if (matriu[j,j] == 2.1){cat("tenint en compte l'anàlisi Òrbita, els resultats són dins els valors esperats. ")};
        if (matriu[j,j] == 2.2){cat("tenint en compte l'anàlisi Òrbita, s'evidencia una \\textbf{fortalesa} respecte els valors esperats. ")};

        if (round(matriu[j,j]) == 3){cat("
\\item S'observen valors \\textbf{alts} respecte la mitjana en l'àrea de \\textbf{", proves[j], "}; ",  sep = "")};
         if (matriu[j,j] == 3.0){cat("tenint en compte l'anàlisi Òrbita, s'evidencien certes febleses respecte els resultats esperats, però no els considerem significatius. ")};
        #if (matriu[j,j] == 3.1){cat("tenint en compte l'anàlisi Òrbita, els resultats són dins els valors esperats. ")};
        if (matriu[j,j] == 3.2){cat("tenint en compte l'anàlisi Òrbita, s'evidencia una \\textbf{fortalesa} respecte els valors esperats. ")};}

cat("
\\end{itemize}
");
}

inpulsivitat_reflectivitat <- function(indeximp, index){
  if(!is.na(indeximp[index]) & indeximp[index]>0.18){
    cat("
      
      \\begin{center}
      \\Large{Índex de reflectivitat - compulsivitat} \\\\ 
      \\end{center}
      
      ");
    
    cat("Es valora que l'estil de resposta fortament tendent a la \\textbf{impulsivitat} pot haver interferit 
        negativament en els resultats obtinguts, havent acumulat una quantitat d'errors que no permet saber 
        la seva capacitat real d'execució.")}
  
  if(!is.na(indeximp[index]) & indeximp[index]<0.09){
    cat("
      
      \\begin{center}
      \\Large{Índex de reflectivitat - compulsivitat} \\\\ 
      \\end{center}
      
      ");
    cat("Es valora que l'estil de resposta fortament tendent a la \\textbf{reflexivitat} pot haver interferit 
        negativament en els resultats obtinguts, havent buscat minimitzar la quantitat d'errors de forma 
        forçada i per tant mostrant un rendiment inferior a la seva capacitat real d'execució.")}
}