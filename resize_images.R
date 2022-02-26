
convert_file <- function(path, input_size, output_size) {

  img <- magick::image_read(here::here("docs", input_size, path))
  img <- magick::image_resize(img, paste0(output_size, "x", output_size))
  magick::image_write(img,
    path = here::here("docs", output_size, path)
  )
  rm(img)
  gc()
  cat(path, "\n")
}

base_size <- 3000
images <- list.files(here::here("docs", base_size))
purrr::walk(images, ~convert_file(.x, base_size, 800))
