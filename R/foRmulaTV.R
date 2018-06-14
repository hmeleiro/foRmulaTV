#' Scraps formulatv.com and downloads spanish TV audience data
#'
#' @param ruta The path where you want to create the csv file
#' @param finicio The date from which you want to scrap. Must be in "YYYY-MM-DD" format.
#' @param ffinal The date until you want to scrap. Must be in "YYYY-MM-DD" format.
#'
#' @importFrom readr write_csv
#' @importFrom  readr read_csv
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

  df <- data.frame("Canal",
                      "Programa",
                      "Inicio",
                      "Duracion",
                      "Espectadores",
                      "Share",
                      "Fecha",
                      "Fecha_hora",
                      "Tema",
                      "Franja")

  readr::write_csv(x = df, path = ruta, append = FALSE, col_names = FALSE)



  for (p in 1:length(links)) {
    x <- GET(url = links[p], add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))

    bloque <- x %>% read_html() %>% html_nodes(".taud div") %>% html_text()

    canal <- x %>% read_html() %>% html_nodes(".taud")
    canal <- canal[-1]

    canal <- str_extract(string = as.character(canal), pattern = "la1|la2|antena3|telecinco|lasexta|cuatro|beIN Sports")
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

    programa <- bloque[seq(from = 6, to = 126, by = 5)]

    if (length(programa) > length(canal)) {
      programa <- programa[!is.na(programa)]
    }

    share <- str_replace(string = bloque[seq(from = 10, to = 130, by = 5)], pattern = "%", replacement = "")
    share <- str_replace(string = share, pattern = ",", replacement = ".")

    if (length(share) > length(canal)) {
      share <- share[!is.na(share)]
    }

    espectadores <- as.numeric(str_replace_all(string = bloque[seq(from = 9, to = 129, by = 5)], pattern = "\\.", ""))

    if (length(espectadores) > length(canal)) {
      espectadores <- espectadores[!is.na(espectadores)]
    }

    inicio <- as.POSIXct(x = paste(fechas[p], bloque[seq(from = 7, to = 127, by = 5)]), format = "%Y-%m-%d %H:%M")
    if (length(inicio) > length(canal)) {
      inicio <- inicio[!is.na(inicio)]
    }


    hora <- bloque[seq(from = 7, to = 127, by = 5)]
    try(hora[nchar(hora) != 5] <- paste0(hora[nchar(hora) != 5], "0"), silent = TRUE)

    fecha_hora <- as.POSIXct(x = paste(fechas[p], hora), format = "%Y-%m-%d %H:%M")

    if (length(fecha_hora) > length(canal)) {
      fecha_hora <- fecha_hora[!is.na(fecha_hora)]
    }

    duracion <- as.numeric(str_remove_all(string = bloque[seq(from = 8, to = 128, by = 5)], pattern = " min."))

    if (length(duracion) > length(canal)) {
      duracion <- duracion[!is.na(duracion)]
    }


    tryCatch(expr = df <- data.frame(Canal = canal,
                                        Programa = programa,
                                        Inicio = inicio,
                                        Duracion =duracion,
                                        Espectadores =espectadores,
                                        Share = share,
                                        Fecha = fechas[p],
                                        Fecha_hora = fecha_hora,
                                        Tema = NA,
                                        Franja = NA),
             error=function(e) { message(paste("Error el día", fechas[p], ".", paste0("http://www.formulatv.com/audiencias/", fechas[p])))})

    try(df$Tema[str_detect(string = tolower(df$Programa), pattern = "telediario|noticias|informativos|teled.") == TRUE] <- "Noticias", silent = TRUE)
    try(df$Tema[str_detect(string = tolower(df$Programa), pattern = "operaci\u00F3n triunfo|operacion triunfo") == TRUE] <- "OT", silent = TRUE)
    try(df$Tema[str_detect(string = tolower(df$Programa), pattern = "hormiguero") == TRUE] <- "El Hormiguero", silent = TRUE)
    try(df$Tema[str_detect(string = tolower(df$Programa), pattern = "f\u00FAtbol|futbol|champions") == TRUE] <- "F\u00FAtbol", silent = TRUE)
    try(df$Tema[str_detect(string = tolower(df$Programa), pattern = "sexta noche|noche en 24h|objetivo de ana pastor") == TRUE] <- "Tertulia", silent = TRUE)
    try(df$Tema[str_detect(string = tolower(df$Programa), pattern = "la voz") == TRUE] <- "La Voz", silent = TRUE)
    try(df$Tema[str_detect(string = tolower(df$Programa), pattern = "pelicul|cine|sesion de tarde|sesi\u00F3n de tarde") == TRUE] <- "Cine", silent = TRUE)
    try(df$Tema[str_detect(string = tolower(df$Programa), pattern = "salvame|s\u00E1lvame") == TRUE] <- "S\u00E1lvame", silent = TRUE)
    try(df$Tema[str_detect(string = tolower(df$Programa), pattern = "salvados") == TRUE] <- "Salvados", silent = TRUE)
    try(df$Tema[str_detect(string = tolower(df$Programa), pattern = "pasapalabra") == TRUE] <- "Pasapalabra", silent = TRUE)
    try(df$Tema[str_detect(string = tolower(df$Programa), pattern = "goya") == TRUE] <- paste("Los Goya", format(df$Fecha[p], "%Y")), silent = TRUE)



    try(df$Franja[format(df$Inicio, '%H:%M') >= "02:30" & format(df$Inicio, '%H:%M') < "07:00"] <- "Madrugada", silent = TRUE)
    try(df$Franja[format(df$Inicio, '%H:%M') >= "07:00" & format(df$Inicio, '%H:%M') < "14:00"] <- "Ma\u00F1ana", silent = TRUE)
    try(df$Franja[format(df$Inicio, '%H:%M') >= "14:00" & format(df$Inicio, '%H:%M') < "17:00"] <- "Sobremesa", silent = TRUE)
    try(df$Franja[format(df$Inicio, '%H:%M') >= "17:00" & format(df$Inicio, '%H:%M') < "20:30"] <- "Tarde", silent = TRUE)
    try(df$Franja[format(df$Inicio, '%H:%M') >= "20:30" & format(df$Inicio, '%H:%M') < "23:59"] <- "Prime Time", silent = TRUE)
    try(df$Franja[format(df$Inicio, '%H:%M') >= "00:00" & format(df$Inicio, '%H:%M') < "02:30"] <- "Late Night", silent = TRUE)



    try(df$Franja <- factor(x = df$Franja, levels = c("Madrugada", "Ma\u00F1ana", "Sobremesa", "Tarde", "Prime Time", "Late Night")), silent = TRUE)
    try(df$Inicio <- format(df$Inicio,"%H:%M"), silent = TRUE)

    try(print(df), silent = TRUE)
    try(readr::write_csv(x = df, path = ruta, append = TRUE, col_names = FALSE), silent = TRUE)

    process <- (p/length(links))*100
    print(paste0("Proceso: ", round(process, digits = 1), "%"))

    df <- NA

  }
  stop <- Sys.time()
  diff <- stop - start
  print(diff)

  audiencias <- readr::read_csv(ruta)
  audiencias <<- audiencias

}
