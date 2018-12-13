Sys.setlocale(category="LC_ALL", locale = "Catalan")
require(stringr)


# Aquest fitxer s'haurà de reorganitzar, per ara hi ha coses de manipulacions de noms i com transformar els excels en 
# csv bonics per fer els informes

### Utilitzat ara en els gràfics col·lectius
# Hem d'agafar els noms i la primera lletra del cognom de tots els nens
formatejar_noms = function(columna_noms){
  # Aquí escurcem els noms perquè si no el de l'esquerra no es veu bé:
  # TODO: noms compostos molt llargs poden portar problemes
  
  np = as.data.frame(str_split_fixed(columna_noms, " ", 3))
  
  noms_nens = paste0(np[,1]," ", substr(np[,2],1,1),".")
  quins_duplicats = which(duplicated(noms_nens))
  
  #en cas de que hi hagi nens amb el mateix nom i la mateixa inicial del cognom agafem la inicial del segon cognom:
  noms_nens[quins_duplicats] = paste0(noms_nens[quins_duplicats]," ", substr(np[quins_duplicats,3],1,1),".")
  return(noms_nens)
}
####

# Agafem tots els noms i filtrarem més endavant; només en el gràfic de la classe conjunta

ajuntar_noms = function(tres_columnes_noms){
  noms_nens = paste0(tres_columnes_noms[,1], # els ifelse són per si hi ha noms buits
                     " ", 
                     ifelse(!is.na(tres_columnes_noms[,2]),tres_columnes_noms[,2],""), 
                     " ",
                     ifelse(!is.na(tres_columnes_noms[,3]),tres_columnes_noms[,3],""))
  return(noms_nens)
}


pretractar_excels <-function(path, nom_carpeta){
  require(readxl)
  noms = excel_sheets(path = path)
  classes = lapply(excel_sheets(path), read_excel, path = path)
  cursos = sapply(strsplit(noms, ""), head, 1)
  numeros_classe = sapply(strsplit(noms, ""), tail, 1)
  noms_fitxers = paste0(cursos, numeros_classe)
  classes = sapply(classes, as.data.frame)
  # mirem quins tenen dades:  
  numero_nens_per_classe = sapply(classes, function(x) length(x$Nom))
  
  # posem un filtre pels valors que no tenen dades o en tenen poques, perquè aniran a part:
  
  classes_bones = which(numero_nens_per_classe>4)
  
  # Formategem els sheets per tenir-los de la nostra manera:

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
    df = cbind.data.frame(ajuntar_noms(as.data.frame(fitxer[,4:6])), fitxer[,cols])
    write.table(df, paste0("dades/",nom_carpeta, "/", noms_fitxers[i],".csv"), 
                sep = ",",
                row.names=F, 
                col.names = F,
                quote = 1)
  }
  
}
