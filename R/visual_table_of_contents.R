##### Build gif #####

build_gif <- function(path,
                      pattern,
                      gif_file = paste0(pattern, ".gif")){

  files <- list.files(path = path, pattern = pattern)
  file_path <- paste0(path,files)
  gifski::gifski(png_files = file_path, progress = F,
                 gif_file = gif_file,
                 delay = .2,
                 width = 100, height = 100)

}


##### Select stataic #####

select_static <- function(path, pattern, which = 3){

  files <- list.files(path = path, pattern = pattern)
  file_path <- paste0(path,files)
  file_path[which]

}



#### create html use static and gif  ####

create_html_use_static_and_gif <- function(static_file, gif_file){

  paste0('<img class="static" width = 100, height = 100 src="',
         static_file,
         '"><img class="active" src="',
         gif_file,'">')

}



build_and_use_gif <- function(path,
                              pattern,
                              static_file = select_static(path, pattern),
                              gif_file = paste0(pattern, ".gif")){

  build_gif(path = path, pattern = pattern, gif_file = gif_file)

  create_html_use_static_and_gif(static_file = static_file,
                                 gif_file = gif_file)

}

knitr::opts_chunk$set(echo = F, eval = T, message = F, warning = F, cache = F)




#### Save plot from chunk ####

save_chunk_plot <- function(chunk_name,
                            filename = chunk_name,
                            path = "html_toc_figures/",
                            type = ".png"){

  if (!dir.exists(path)) {

    dir.create(path)

  }
  eval(parse(text = paste(knitr::knit_code$get(chunk_name), collapse = "")))
  ggplot2::ggsave(paste0(path, filename, type), dpi = 300)

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

