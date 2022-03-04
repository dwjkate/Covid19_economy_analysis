library(geojsonio)

us_states <- geojsonio::geojson_read("../data/states.json", what = "sp")

