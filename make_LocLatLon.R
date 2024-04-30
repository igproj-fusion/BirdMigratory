pacman::p_load( 
  tidyverse,
  rvest)


mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}


URL_base <- "https://www.env.go.jp/nature/dobutsu/bird_flu/migratory/ap_wr_transit23/"
URL_PrefAreaLoc <- "https://raw.githubusercontent.com/igproj-fusion/BirdMigratory/main/PrefAreaLoc.csv"


site_df <- read_html(paste0(URL_base, "index.html"))|> 
  html_nodes("a") %>% 
  html_attr("href") |> 
  as_tibble() |> 
  filter(grepl(pattern = "site_gaiyo", x = value)) |> 
  mutate(link = paste0(URL_base, value))

df <- NULL
for(i in 1:nrow(site_df))
{
  HTML <- read_html(site_df$link[i])
  
  loc <- HTML |> 
    html_nodes("h1") |> html_text2()
  
  list <- HTML |> 
    html_table()
  lonlat <- as.character(list[[1]][3, 4])
  df <- rbind(df, data.frame(loc, lonlat))
}

LocLatLon <- df |> 
  as_tibble() |>
  mutate(lonlat = sub("　", " ", lonlat)) |> 
  mutate(lonlat = sub(",", "", lonlat)) |> 
  separate(col = lonlat, into = c("lat", "lon"), sep = " ")|> 
  mutate_cond(loc == "仙台大沼", loc = "仙台市大沼") |>
  mutate_cond(loc == "風蓮湖 (根室市側)", loc = "風蓮湖 根室市側") |> 
  mutate_cond(loc == "風蓮湖 （別海町側）", loc = "風蓮湖 別海町側") |> 
  mutate_cond(loc == "広島湾西部（御手洗川河口）", loc = "御手洗川河口") |> 
  mutate_cond(loc == "広島湾西部（八幡川河口）", loc = "八幡川河口") |> 
  mutate_cond(loc == "きらら浜・土路石川河口", loc = "きらら浜／土路石川河口") |> 
  mutate_cond(loc == "三蔵宮池（さんぞくいけ）", loc = "三蔵宮池") |> 
  rbind(data.frame(loc = "宇和運動公園", lat = 33.3577, lon = 132.5102)) |> 
  left_join(read.csv(URL_PrefAreaLoc), by = c("loc" = "loc"))

# library(here)
# write_csv(LocLatLon, here("LocLatLon.csv"))
# OR
# save(LocLatLon, file = here("LocLatLon.Rdata"), compress = "xz")
