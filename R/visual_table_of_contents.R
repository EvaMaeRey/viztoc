#### take webshots of flipbooks #####
html_slide_show_take_webshots <- function(base_url =
                         "https://evamaerey.github.io/data_manipulation/one_stream_wrangle.html",
                       dir = "test",
                       num_pages,
                       pad_digits = 0) {

  if (!dir.exists(dir)) {dir.create(dir)}

  for (i in 1:num_pages) {

    url <- paste0(base_url, "#", i)
    i_padded <- stringr::str_pad(i, 2, pad = "0")
    file <- paste0(dir, "/webshot", i_padded, ".png")
    webshot::webshot(url = url, file = file)

    i <- i + 1

  }

}

# this is brshallo's
rmd_build_gif <- function(rmd,
                          density = 100,
                          delay = 1){

  filepath <- fs::path_ext_remove(rmd)
  filepath_pdf <- fs::path_ext_set(filepath, "pdf")
  filepath_gif <- fs::path_ext_set(filepath, "gif")

  pagedown::chrome_print(rmd)
  images <- magick::image_read_pdf(filepath_pdf,
                                   density = density)
  magick::image_write_gif(images,
                          filepath_gif,
                          delay = delay)
}


##### Build gif #####

dir_pngs_build_gif <- function(dir,
                      pattern = "",
                      gif_file = paste0(dir, "/viztoc", pattern, ".gif"),
                      num_in_gif = NULL,
                      width = 100,
                      height = 100,
                      delay = .3){

  files <- list.files(path = dir, pattern = pattern)

  if (is.null(num_in_gif)) {num_in_gif <- length(files)}

  files <- files[1:num_in_gif]

  file_path <- paste0(dir, files)

  gifski::gifski(png_files = file_path,
                 progress = F,
                 gif_file = gif_file,
                 delay = delay,
                 width = width,
                 height = height)
}


##### Select stataic #####

select_static <- function(dir, pattern, which = 3){

  files <- list.files(path = dir, pattern = pattern)
  file_path <- paste0(dir, files)
  file_path[which]

}



#### create html use static and gif  ####

create_html_use_static_and_gif <- function(static_file,
                                           gif_file,
                                           href = "https://github.com/EvaMaeRey/flipbookr",
                                           width = 200,
                                           height = 200){

  paste0(
    '<a href="',
    href,
    '" target="_blank">',
    '<img class="static" width = ', width, ', height = ', height,  ', src="',
    static_file,
    '"><img class="active" src="',
    gif_file,
    '">',
    '</a>')

}

#### write css static and hover ####

write_css_static_hover <- function(){

"```{css}
.static {
  position:absolute;
  background: white;
}

.static:hover {
  opacity:0;
}
```"

}



#' Title
#'
#' @param dir
#' @param pattern
#' @param static_file
#' @param gif_file
#' @param num_in_gif
#' @param delay
#'
#' @return
#' @export
#'
#' @examples
dir_pngs_build_static_gif_viztoc <- function(
                              dir,
                              pattern = "",
                              href,
                              which = 5,
                              width = 100,
                              height = 100,
                              delay = .2,
                              pagedown = T,
                              static_file = select_static(dir, pattern, which),
                              gif_file = paste0(dir, "/viztoc_", pattern, ".gif"),
                              num_in_gif = NULL,
                              cached_gif = TRUE){

  if (cached_gif == FALSE | !file.exists(gif_file)) {


    dir_pngs_build_gif(dir = dir,
              pattern = pattern,
              gif_file = gif_file,
              num_in_gif = num_in_gif,
              width = width,
              height = height,
              delay = delay)

  }

  create_html_use_static_and_gif(href = href,
                                 static_file = static_file,
                                 gif_file = gif_file,
                                 width = width,
                                 height = height)

}



# rmd_build_static_gif_viztoc <- function(rmd,
#                                         href,
#                                         which = 5,
#                                              width = 100,
#                                              height = 100,
#                                              delay = .2,
#                                              pagedown = T,
#                                              static_file = select_static(dir, pattern, which),
#                                              gif_file = paste0(dir, "/viztoc_", pattern, ".gif"),
#                                              num_in_gif = NULL,
#                                              cached_gif = TRUE){
#
#   if (cached_gif == FALSE | !file.exists(gif_file)) {
#
#     rmd_build_gif(rmd = rmd,
#                   delay = delay,
#                   density = density)
#
#   }
#
#   # How to get the static file?
#   create_html_use_static_and_gif(href = href,
#                                  static_file = static_file,
#                                  gif_file = gif_file,
#                                  width = width,
#                                  height = height)
#
# }




#' Title
#'
#' @param dir
#' @param base_url
#' @param href
#' @param num_pages
#' @param pattern
#' @param which
#' @param width
#' @param height
#' @param delay
#' @param static_file
#' @param gif_file
#' @param num_in_gif
#' @param cached_gif
#'
#' @return
#' @export
#'
#' @examples
flipbook_webshots_build_and_use_gif <- function(dir,
                              base_url,
                              href = base_url,
                              num_pages,
                              pattern = "",
                              which = 5,
                              width = 100,
                              height = 100,
                              delay = .2,
                              static_file = select_static(dir, pattern, which),
                              gif_file = paste0(dir, "/viztoc_", pattern, ".gif"),
                              num_in_gif = NULL,
                              cached_gif = TRUE
                              ){

  if (cached_gif == FALSE | !file.exists(gif_file)) {

    html_slide_show_take_webshots(dir = dir,
                           base_url = base_url,
                           num_pages = num_pages)

    pngs_build_gif(dir = dir,
              pattern = "",
              gif_file = gif_file,
              num_in_gif = num_in_gif,
              width = width,
              height = height,
              delay = delay)

  }

  create_html_use_static_and_gif(href = href,
                                 static_file = static_file,
                                 gif_file = gif_file,
                                 width = width,
                                 height = height)

}


flipbook_webshots_build_and_use_gif(dir = "hi/",
                                    cached_gif = T,
                                    num_pages = 3,
                                    pattern = "",
                                    base_url = "https://evamaerey.github.io/tidyverse_in_action/one_stream_wrangle.html")


#### Save plot from chunk ##########

save_chunk_plot <- function(chunk_name,
                            filename = chunk_name,
                            dir = "html_toc_figures/",
                            type = ".png"){

  if (!dir.exists(dir)) {

    dir.create(dir)

  }

  eval(parse(text = paste(knitr::knit_code$get(chunk_name), collapse = "")))
  ggplot2::ggsave(paste0(dir, filename, type), dpi = 300)

}


#### Make html picture link #####

make_html_picture_link <- function(path,
                                   link,
                                   title = stringr::str_remove(link, "\\..+")){

  cat(paste0('<a href="', link,
             '"><img src="', path,
             '"width="150" height="150" title=' ,
             title, ' alt=', path,'></a>'))

}

