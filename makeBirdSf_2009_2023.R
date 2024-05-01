pacman::p_load( 
  here,
  tidyverse,
  rvest,
  xlsx,
  sf,
  reshape2,
  ggsci)

#####################################################

URL_base0 <- "https://www.env.go.jp/nature/dobutsu/bird_flu/migratory/ap_wr_transit"
URL_loclatlon <- "https://raw.githubusercontent.com/igproj-fusion/BirdMigratory/main/LocLatLon.csv"
jp_pref_geo <- "https://github.com/igproj-fusion/R-gis/raw/main/japan_ver2.geojson"
JP_PREF <- read_sf(jp_pref_geo)

#####################################################

BIRDS_LOC <- NULL
for(YEAR in 2009:2023)
{
  if(YEAR %in% 2021:2023) {
    ColName1 <- rlang::sym("調査年月日")} else {
      if(YEAR %in% 2013:2020) {
        ColName1 <- rlang::sym("年月日")
    }
  }
  
  URL_base <- paste0(URL_base0, substr(as.character(YEAR), 3, 4), "/")
  bird_df <- read_html(paste0(URL_base, "index.html")) |>
    html_nodes("a") %>% 
    html_attr("href") |> 
    as_tibble() |> 
    filter(grepl(pattern = "sp_csv", x = value)) |> 
    mutate(fname = gsub("sp_csv/", "", value)) |>
    mutate(link = paste0(URL_base, value)) |> 
    select(fname, link)

  tmp_dir <- tempdir()
  Birds <- NULL
  for(i in 1:nrow(bird_df))
  {
    file <- file.path(tmp_dir, bird_df$fname[i])
    download.file(bird_df$link[i], destfile = file, mode = "wb")
    d <- read.xlsx(file, sheetIndex = 1) 
    
    if(YEAR < 2013){
      dt <- d |> 
        filter(!is.na(年)) |> 
        mutate(date = ymd(paste0(年, "-", 月, "-", 日))) 
    } else {
      dt <- d |> 
        filter(!is.na(!!ColName1)) |> 
        mutate(date = ymd(!!ColName1)) 
    }
    Birds <- dt |> 
      select(date, loc = 調査地名, type = 種名, number = 数) |>
      mutate(number = as.integer(number)) |> 
      rbind(Birds)
  }
  BIRDS_LOC <- left_join(Birds, read.csv(URL_loclatlon), 
                        by = c("loc" = "loc")) |> 
    mutate(year = YEAR) |> 
    st_as_sf(coords = c('lon', 'lat'), crs = st_crs(JP_PREF)) |> 
    rbind(BIRDS_LOC)
}

## save(BIRDS_LOC, file = here("data", "BIRDS_LOC_2009_2023.Rdata"), compress = "xz")

#####################################################
# Examples
#####################################################

BIRDS_LOC |>
  as_tibble() |> 
  filter(type == "カルガモ") |> 
  group_by(year, area) |> 
  summarize(total = sum(number)) |> 
  ggplot(aes(x = year, y = total, fill = area)) +
  geom_bar(stat = "identity") +
  scale_fill_nejm() +
  theme_light() +
  labs(title = paste0("カルガモ飛来数 2009~2023"),
       x = "",
       y = "No. of Observations") +
  theme(plot.margin= unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3), 
                                  lineheight = 0.3),
        legend.title = element_text(size = rel(0)),
        legend.position = "bottom")


BIRDS_LOC |>
  as_tibble() |> 
  filter(type == "カルガモ") |>
  filter(pref == "千葉県") |> 
  group_by(year) |> 
  summarize(total = sum(number)) |> 
  ggplot(aes(x = year, y = total)) +
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  theme_light() +
  labs(title = paste0("カルガモ飛来数 2009~2023 @千葉県谷津"),
       x = "",
       y = "No. of Observations") +
  theme(plot.margin= unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3), 
                                  lineheight = 0.3))
