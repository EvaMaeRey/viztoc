##### Build gif #####

build_gif <- function(dir,
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
#'
#' @return
#' @export
#'
#' @examples
build_and_use_gif <- function(href,
                              dir,
                              pattern = "",
                              which = 5,
                              width = 100,
                              height = 100,
                              static_file = select_static(dir, pattern, which),
                              gif_file = paste0(dir, "/viztoc_", pattern, ".gif"),
                              num_in_gif = NULL,
                              cached_gif = TRUE){

  if (cached_gif == FALSE | !file.exists(gif_file)) {

  build_gif(dir = dir,
            pattern = pattern,
            gif_file = gif_file,
            num_in_gif = num_in_gif,
            width = width,
            height = height)

  }

  create_html_use_static_and_gif(href = href,
                                 static_file = static_file,
                                 gif_file = gif_file,
                                 width = width,
                                 height = height)

}

knitr::opts_chunk$set(echo = F, eval = T, message = F, warning = F, cache = F)






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

