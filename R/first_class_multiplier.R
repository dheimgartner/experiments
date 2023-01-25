first_class_multiplier <- function(x, multiplier = 1.7) {
  nu <- remove_unit(x)
  nu$numbers <- round(nu$numbers * multiplier, digits = 2)
  experiments::paste_unit_number(nu)
}
