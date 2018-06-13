#' Scraps formulatv.com and downloads spanish TV audience data
#'
#' @param ruta The path where you want to create the csv file
#' @param finicio The date from which you want to scrap
#' @param ffinal The date until you want to scrap
#'
#' @importFrom readr readr::write_csv
#' @importFrom  readr readr::read_csv
#' @import stringr
#' @import xml2
#' @import rvest
#' @import httr
#'
#' @return A data frame will be imported to your workspace and a csv file will be generated in the specified path
#'
#'
#' @export
formulatv <- function(ruta, finicio, ffinal) {

  start <- Sys.time()

  desktop_agents <-  c('Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                       'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                       'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                       'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/602.2.14 (KHTML, like Gecko) Version/10.0.1 Safari/602.2.14',
                       'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36',
                       'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36',
                       'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36',
                       'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36',
                       'Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                       'Mozilla/5.0 (Windows NT 10.0; WOW64; rv:50.0) Gecko/20100101 Firefox/50.0')


  fechas <- seq.Date(from = as.Date(finicio), to = as.Date(ffinal), by = "day")

  links <- paste0("http://www.formulatv.com/audiencias/", fechas)

  lines <- data.frame("Canal",
                      "Programa",
                      "Inicio",
                      "Duracion",
                      "Espectadores",
                      "Share",
                      "Fecha",
                      "Fecha_hora",
                      "Tema",
                      "Franja")

  readr::write_csv(x = lines, path = ruta, append = FALSE, col_names = FALSE)



  for (p in 1:length(links)) {
    x <- GET(url = links[p], add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))

    bloque <- x %>% read_html() %>% html_nodes(".taud div") %>% html_text()

    canal <- x %>% read_html() %>% html_nodes(".taud")
    canal <- canal[-1]

    canal <- as.character(str_extract(string = canal, pattern = "la1|la2|antena3|telecinco|lasexta|cuatro|beIN Sports"))
    canal[canal == "la1"] <- "La 1"
    canal[canal == "la2"] <- "La 2"
    canal[canal == "antena3"] <- "Antena 3"
    canal[canal == "telecinco"] <- "Telecinco"
    canal[canal == "lasexta"] <- "La Sexta"
    canal[canal == "cuatro"] <- "Cuatro"
    canal[length(canal) == 0] <- NA


    remove <- ""
    bloque <- bloque[! bloque %in% remove]
    remove <- " "
    bloque <- bloque[-1]

    share <- str_replace(string = bloque[seq(from = 10, to = 130, by = 5)], pattern = "%", replacement = "")
    share <- str_replace(string = share, pattern = ",", replacement = ".")


    espectadores <- as.numeric(str_replace_all(string = bloque[seq(from = 9, to = 129, by = 5)], pattern = "\\.", ""))

    inicio <- as.POSIXct(x = paste(fechas[p], bloque[seq(from = 7, to = 127, by = 5)]), format = "%Y-%m-%d %H:%M")
    fecha_hora <- as.POSIXct(x = paste(fechas[p], bloque[seq(from = 7, to = 127, by = 5)]), format = "%Y-%m-%d %H:%M")

    duracion <- as.numeric(str_replace(string = bloque[seq(from = 8, to = 128, by = 5)], pattern = " min.", replacement = ""))


    lines <- data.frame(Canal = canal,
                        Programa = bloque[seq(from = 6, to = 126, by = 5)],
                        Inicio = inicio,
                        Duracion =duracion,
                        Espectadores =espectadores,
                        Share = share,
                        Fecha = fechas[p],
                        Fecha_hora = fecha_hora,
                        Tema = NA,
                        Franja = NA)

    lines$Tema[str_detect(string = tolower(lines$Programa), pattern = "telediario|noticias|informativos|teled.") == TRUE] <- "Noticias"
    lines$Tema[str_detect(string = tolower(lines$Programa), pattern = "operación triunfo|operacion triunfo") == TRUE] <- "OT"
    lines$Tema[str_detect(string = tolower(lines$Programa), pattern = "hormiguero") == TRUE] <- "El Hormiguero"
    lines$Tema[str_detect(string = tolower(lines$Programa), pattern = "fútbol|futbol|champions") == TRUE] <- "Fútbol"
    lines$Tema[str_detect(string = tolower(lines$Programa), pattern = "sexta noche|noche en 24h|objetivo de ana pastor") == TRUE] <- "Tertulia"
    lines$Tema[str_detect(string = tolower(lines$Programa), pattern = "la voz") == TRUE] <- "La Voz"
    lines$Tema[str_detect(string = tolower(lines$Programa), pattern = "pelicul|cine|sesion de tarde|sesión de tarde") == TRUE] <- "Cine"
    lines$Tema[str_detect(string = tolower(lines$Programa), pattern = "salvame|sálvame") == TRUE] <- "Sálvame"
    lines$Tema[str_detect(string = tolower(lines$Programa), pattern = "salvados") == TRUE] <- "Salvados"
    lines$Tema[str_detect(string = tolower(lines$Programa), pattern = "pasapalabra") == TRUE] <- "Pasapalabra"
    lines$Tema[str_detect(string = tolower(lines$Programa), pattern = "goya") == TRUE] <- paste("Los Goya", format(lines$Fecha[p], "%Y"))



    lines$Franja[format(lines$Inicio, '%H:%M') >= "02:30" & format(lines$Inicio, '%H:%M') < "07:00"] <- "Madrugada"
    lines$Franja[format(lines$Inicio, '%H:%M') >= "07:00" & format(lines$Inicio, '%H:%M') < "14:00"] <- "Mañana"
    lines$Franja[format(lines$Inicio, '%H:%M') >= "14:00" & format(lines$Inicio, '%H:%M') < "17:00"] <- "Sobremesa"
    lines$Franja[format(lines$Inicio, '%H:%M') >= "17:00" & format(lines$Inicio, '%H:%M') < "20:30"] <- "Tarde"
    lines$Franja[format(lines$Inicio, '%H:%M') >= "20:30" & format(lines$Inicio, '%H:%M') < "23:59"] <- "Prime Time"
    lines$Franja[format(lines$Inicio, '%H:%M') >= "00:00" & format(lines$Inicio, '%H:%M') < "02:30"] <- "Late Night"



    lines$Franja <- factor(x = lines$Franja, levels = c("Madrugada", "Mañana", "Sobremesa", "Tarde", "Prime Time", "Late Night"))
    lines$Inicio <- format(lines$Inicio,"%H:%M")

    print(lines)
    readr::write_csv(x = lines, path = ruta, append = TRUE, col_names = FALSE)

    process <- (p/length(links))*100
    print(paste0("Proceso: ", round(process, digits = 1), "%"))

  }
  stop <- Sys.time()
  diff <- stop - start
  print(diff)

  audiencias <<- readr::read_csv(ruta)

}
