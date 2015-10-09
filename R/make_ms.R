make_ms <- function(fn){
  rmarkdown::render(fn, word_document(), "manuscript.docx")
}
