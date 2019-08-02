Sys.setlocale(category="LC_ALL", locale = "Catalan")

config = config::get()  # Importem la configuració
encoding_ = config$encoding
directoris = config::get(config = "directoris")
temp_ = directoris$temp
figures_ = directoris$figures
dades_ = directoris$dades
informes_ = directoris$informes
barems_ = directoris$barems
fitxers_barems_ = directoris$fitxers
resultats_ = directoris$resultats

wd = getwd()

source(file.path(wd, "informe_general.R"), encoding = encoding_)
source(file.path(wd, "utils", "manipulacions_dades.R"), encoding = encoding_)

informe_excel = function(path_excel, nom_escola, tipus = "classe"){
  
  # Intentem esborrar figures, informes i csv antics
  # FIXME: no sempre funciona; s"han de mirar històries de permisos en windows
  
  wd <- getwd();
  unlink(file.path(wd, temp_), recursive = T, force = T)
  
  # Creem ara els paths que anirem fent servir:
  path_dades = file.path(wd, temp_, dades_)
  path_figures = file.path(wd, temp_, figures_)
  path_informes = file.path(wd, temp_, informes_)
  path_barems = file.path(wd, barems_, fitxers_barems_)
  path_resultats = file.path(wd, temp_, resultats_)
  
  # Ajuntem tots els paths en un "diccionari" per tenir-los una mica ordenats i poder-hi accedir fàcilment.
  path_llista = list("dades" = path_dades, 
                     "figures" = path_figures, 
                     "informes" = path_informes, 
                     "barems" = path_barems, 
                     "resultats" = path_resultats)
  
  pretractar_excels(path_excel)
  
  informe_general(nom_escola, path_llista, tipus)
  
}

# Utilització:
# informe_excel("dades/hogwarts.xlsx", "Escola Hogwarts", "classe")
# informe_excel("dades/Correccions Tecnos 2018-19.xlsx", "Escola Tecnos", "individual")

