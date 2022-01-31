
library(shiny)
library(rsconnect)
library(DT)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(shinyauthr)
library(sf)
library(ggplot2)
library(wqTools)
library(ggvis)
library(SwimmeR)
library(leaflet.extras)
library(leaflet.providers)
library(sp)
options(tz="Africa/Dakar")

Sys.setenv(TZ="Africa/Dakar")
Sys.getenv("TZ")
library(wqTools)
library("jsonlite")
#Phase de deploiement
library(rsconnect)
library(jsonlite)
library(sodium)
install.packages("live server")
rsconnect::setAccountInfo(name='mycarto', token='0D187CDE76C851E529528F447E6AA695', 
                          secret='BBFfnh40h9Rzvi+KDgn7ZmxJEQKGwpWamrZYhQP/')
deployApp()

install.packages("openssl")

message('library paths:\n', paste('... ', .libPaths(), sep='', collapse='\n'))

chrome.portable = file.path(getwd(),
                            'GoogleChromePortable/App/Chrome-bin/chrome.exe')

launch.browser = function(appUrl, browser.path=chrome.portable) {
  browser.path = chartr('/', '\\', browser.path)
  message('Browser path: ', browser.path)
  
  CMD = browser.path
  ARGS = sprintf('--app="%s"', appUrl)
  
  system2(CMD, args=ARGS, wait=FALSE)
  NULL
}



# Temporary debugging 


library("openssl")

.First = function(){
  .libPaths(.Library)
}

appwd  = getwd()
applibpath  = file.path( appwd , ' app ' , ' library ' )

.libPaths() # verify that only the local R-Portable library path is available
install.packages('shiny')

.libPaths(c(applibpath, .Library))

message('library paths:\n', paste('... ', .libPaths(), sep='', collapse='\n'))
message('working path:\n', paste('...', appwd))


# créer une application/bibliothèque si elle n'existe pas (par exemple, première exécution)

if (!dir.exists(applibpath)) {
  dir.create(applibpath)
}

.libPaths(c( applibpath , .Library ))

# utility function for ensuring that a package is installed
ensure = function(package, repo = 'http://cran.rstudio.com', load = FALSE) {
  if (!(package %in% rownames(installed.packages()))) {
    install.packages(package, repo = repo, lib = applibpath)
  }
  if (load) {
    library(package, character.only = TRUE)
  }
}


ensure('jsonlite', load = TRUE)
config = fromJSON(file.path(appwd, 'app', 'config.cfg'))


# provide some initialization status updates to assure the user that something
# is happening
pb = winProgressBar(
  title = sprintf('Starting %s ...', config$appname),
  label = 'Initializing ...'
)


appexit_msg = tryCatch({
  
  # ensure all package dependencies are installed before attempting to load them
  packages = read.table(
    file.path(appwd, 'app', 'packages.txt'),
    col.names='package',
    as.is = TRUE
  )$package
  
  message('ensuring packages: ', paste(packages, collapse = ', '))
  setWinProgressBar(pb, 0, label = 'Ensuring package dependencies ...')
  ._ = lapply(packages, ensure, repo = config$packages$cran)
  
  for (i in seq_along(packages)) {
    setWinProgressBar(pb, i/(length(packages)+1), label = sprintf('Loading package-%s', packages[i]))
    library(packages[i], character.only = TRUE)
  }
  
  setWinProgressBar(pb, 1.00, label = 'Starting application')
  close(pb)
  
  # app is launched in the system default browser (if FF or Chrome, should work
  # fine, IE needs to be >= 10)
  source(file.path(appwd, 'app', 'app.R'))
  
  'application terminated normally'
},
error = function(e) {
  msg = sprintf('Startup failed with error(s):\n\n%s', e$message)
  tcltk::tk_messageBox(
    type="ok",
    message=msg,
    icon="error")
  
  msg
},
finally = {
  close(pb)
})



# Importation des fichiers et nettoyage

#1 Impoprtation
courbe <- st_read("courbe_geo.shp",layer = "courbe_geo")
note <- st_read("point_geo.shp",layer = "point_geo")




#2 Nettoyage
note <- st_zm(note, drop = T, what = "ZM")
courbe <- st_zm(courbe, drop = T, what = "ZM")
bloc <- st_zm(bloc, drop = T, what = "ZM")


#Preparation des label avec le package HTML
note$label <- paste("<p>", note$TEXT_1,"</p>",
                   "<p>", note$TEXT_2,"</p>",
                   "<p>", note$TEXT_3,"</p>")


#Data frame de l espace login
user_base <- tibble::tibble(
  user = c("sahelgeo", "user2"),
  password = sapply(c("sahel1951", "pass2"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)


sn_map <- leaflet() %>% 
  addFullscreenControl() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  setView(lng = -17.244, lat = 14.706,zoom = 15.5) %>% 
 
  
  addPolylines(data = courbe,
               color = "yellow",
               weight =1,
               smoothFactor = 1,
               fillColor =  "blank",
               fillOpacity = 0.5,
               popup = ~leafpop::popupTable(courbe,
                                            zcol = "Elevation",
                                            row.numbers = FALSE, feature.id = FALSE))%>% 
  
  addCircleMarkers(data = note,
                   color = "red",
                   radius = 2,
                   label = lapply(note$label, HTML
                   ),
  ) %>% 
  addGeotiff(
      map,
      file = "C:/Users/SAHEL ST_1/Dropbox/Mon PC (DESKTOP-EOB2GE7)/Desktop/STAGE/Soccocim/orthofoto",
      resolution = 96,
      colorOptions = NULL,
      rgb = FALSE,
      pixelValuesToColorFn = NULL,
      autozoom = TRUE,
  
    
  )

sn_map
# Cote Interface avec UI(User Interface)

ui <- fluidPage(
  
  theme = shinytheme("united"
                     
  ),
  headerPanel(title="SIG-WEB", windowTitle ="SahelGeo"
              
  ),
  
  fluidRow(
    
    
  ),
  
  dashboardPage(
    skin = "yellow",
    
    dashboardHeader( title = "SahelGeo"
                     
    ),
    dashboardSidebar( width = 6,
                      sidebarMenu(
                        
                        menuItem("Visualisation des données", tabName = "visualisation",
                                 icon = shiny::icon("angle-double-right")
                                 
                        )
                      )
                      
                      
    ),
    
    
    dashboardBody(
      
      tabItems(
        
        
        
        
        tabItem(tabName = "visualisation",
                h1("Visualisation des données"),
                # login section
                shinyauthr::loginUI(id = "login"),
                # logout button
                div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
                
                
                leafletOutput("mymap"))
        
      )    
      
    )
    
  )
  
)



# Cote Serveur 
server <- function(input, output) {

  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # Logout to hide
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
 
  
  #Affichage des données
  output$mymap <- renderLeaflet({
    
    # Show only when authenticated
    req(credentials()$user_auth)
    
    sn_map <- leaflet() %>% 
      addFullscreenControl() %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      setView(lng = -17.244, lat = 14.706,zoom = 15.5) %>% 
      addPolygons(data = bloc,
                  color = "black",
                  weight =1,
                  smoothFactor = 1,
                  fillColor =  "blank",
                  fillOpacity = 0.5 )%>% 
      
      addPolylines(data = courbe,
                   color = "yellow",
                   weight =1,
                   smoothFactor = 1,
                   fillColor =  "blank",
                   fillOpacity = 0.5,
                   popup = ~leafpop::popupTable(courbe,
                                                zcol = "Elevation",
                                                row.numbers = FALSE, feature.id = FALSE))%>% 
      
      addCircleMarkers(data = note,
                       color = "red",
                       radius = 2,
                       label = lapply(note$label, HTML
                       ),
      )
    
    
    
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

runApp("./app/shiny/", launch.browser=TRUE)




library(leaflet)
library(leafem)
library(stars)

# tst = read_stars("/media/timpanse/d8346522-ef28-4d63-9bf3-19fec6e13aab/bu_lenovo/software/data/global_elevation_0.tif")
tst = read_stars("C:/Users/SAHEL ST_1/Dropbox/Mon PC (DESKTOP-EOB2GE7)/Desktop/STAGE/Soccocim/orthofoto.tif")
st_is_longlat(tst)

if (!st_is_longlat(tst)) {
  tst = st_warp(tst, crs = 4326)
}

tst_ds = stars:::st_downsample(tst, n = 1)
dim(tst_ds)

fl = tempfile(fileext = ".tif")
write_stars(tst_ds, dsn = fl)

options(viewer = NULL)

leaflet() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  leafem:::addGeotiff(
    file = fl
    , group = "test"
    , layerId = "testid"
    , resolution = 96 ,
    autozoom = T
    )
  
