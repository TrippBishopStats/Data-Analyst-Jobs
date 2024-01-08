compute_percentage <- function(field,total) {
  round(sum(field)/total*100,1)
}

compute_p <- function(model) {
  f_stat <- summary(model)$fstatistic
  p_value <- pf(f_stat[1],
                f_stat[2],
                f_stat[3],
                lower.tail=FALSE)
  attributes(p_value) <- NULL
  p_value
}
