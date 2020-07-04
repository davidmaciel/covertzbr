get_result <- function(url){
  Sys.sleep(0.5)
  jsonlite::fromJSON(url) %>% purrr::pluck("results")
}

get_npage <- function(url){
  count <- jsonlite::fromJSON(url) %>% purrr::pluck("count")
  round(count/10000)
}

build_urls <- function(url){
  n_page <- get_npage(url)
  c(url,paste0("https://brasil.io/api/dataset/covid19/caso/data/?page=",
         2:n_page, "&page_size=10000"))
}



