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
        if (matriu[j,j] == 1.0){cat("tenint en compte l'anàlisi Òrbita, s'evidencien \\textbf{dificultats} significatives respecte els resultats esperats. ")};
        #if (matriu[j,j] == 1.1){cat("tenint en compte l'anàlisi Òrbita, els resultats són dins els valors esperats. ")};
        if (matriu[j,j] == 1.2){cat("tenint en compte l'anàlisi Òrbita, s'evidencia un resultat superior respecte els valors esperats. ")};

        if (round(matriu[j,j]) == 2)
{cat("\\item S'observen valors dins la normalitat en l'àrea de \\textbf{", proves[j], "}; ",  sep = "")};
         if (matriu[j,j] == 2.0){cat("tenint en compte l'anàlisi Òrbita, s'evidencien \\textbf{dificultats} significatives respecte els resultats esperats. ")};
        #if (matriu[j,j] == 2.1){cat("tenint en compte l'anàlisi Òrbita, els resultats són dins els valors esperats. ")};
        if (matriu[j,j] == 2.2){cat("tenint en compte l'anàlisi Òrbita, s'evidencia un \\textbf{talent} respecte els valors esperats. ")};

        if (round(matriu[j,j]) == 3){cat("
\\item S'observen valors \\textbf{alts} respecte la mitjana en l'àrea de \\textbf{", proves[j], "}; ",  sep = "")};
         if (matriu[j,j] == 3.0){cat("tenint en compte l'anàlisi Òrbita, s'evidencien certes dificultats respecte els resultats esperats, però no els considerem significatius. ")};
        #if (matriu[j,j] == 3.1){cat("tenint en compte l'anàlisi Òrbita, els resultats són dins els valors esperats. ")};
        if (matriu[j,j] == 3.2){cat("tenint en compte l'anàlisi Òrbita, s'evidencia un \\textbf{talent} respecte els valors esperats. ")};}

cat("
\\end{itemize}
");
}
