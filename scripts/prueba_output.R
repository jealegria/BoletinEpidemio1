
output_file <- paste0("Reporte_", Sys.Date(), ".html")
# 
# # Renderizar el archivo con ese nombre

quarto::quarto_render("Export_HTML_1.qmd", output_file = output_file)