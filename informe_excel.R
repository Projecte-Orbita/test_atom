Sys.setlocale(category="LC_ALL", locale = "Catalan")
source("informe_general.R", encoding = "UTF-8")
source("utils/manipulacions_dades.R", encoding = "UTF-8")

informe_excel = function(path_excel, nom_escola, tipus = "classe"){
  
  # Intentem esborrar figures, informes i csv antics
  # FIXME: no sempre funciona; s"han de mirar històries de permisos en windows
  
  wd <- getwd();
  unlink(file.path(wd, "temp"), recursive = T, force = T)
  
  # Creem ara els paths que anirem fent servir:
  path_dades = file.path(wd, "temp", "dades")
  path_figures = file.path(wd, "temp", "figures")
  path_informes = file.path(wd, "temp", "informes")
  path_barems = file.path(wd, "barems", "fitxers")
  
  # Ajuntem tots els paths en un "diccionari" per tenir-los una mica ordenats i poder-hi accedir fàcilment.
  path_llista = list("dades" = path_dades, 
                     "figures" = path_figures, 
                     "informes" = path_informes, 
                     "barems" = path_barems)
  
  pretractar_excels(path_excel)
  
  informe_general(nom_escola, path_llista, tipus)
  
}

# Utilització:
# informe_excel("dades/hogwarts.xlsx", "Escola Hogwarts", "classe")
# informe_excel("dades/Correccions Tecnos 2018-19.xlsx", "Escola Tecnos", "individual")

