Sys.setlocale(category="LC_ALL", locale = "Catalan")
  

informe_excel = function(path_excel, nom_escola, tipus = "classe"){
  source('R2latex_general.R', encoding = "UTF-8")
  source('manipulacions_dades.R', encoding = "UTF-8")
  
  unlink('temp', recursive = T)
  
  wd <- getwd();
  
  # Creem ara els paths que anirem fent servir:
  path_dades = file.path(wd, 'temp/dades')
  path_figures = file.path(wd, "temp/figures/")
  path_informes = file.path(wd, "temp/informes/")
  path_barems = file.path(wd, "barems")
  
  # Ajuntem tots els paths en un "diccionari" per tenir-los una mica ordenats i poder-hi accedir fàcilment.
  path_llista = list('dades' = path_dades, 
                     'figures' = path_figures, 
                     'informes' = path_informes, 
                     'barems' = path_barems)
  
  pretractar_excels(path_excel)
  
  informe_general(nom_escola, path_llista, tipus)
  
}

# Utilització:
# informe_excel('dades/hogwarts.xlsx', 'Escola Hogwarts', 'classe')
# informe_excel('dades/Correccions Tecnos 2018-19.xlsx', 'Escola Tecnos', 'individual')

