generate_design <- function(bundle,
                            frml = NULL,
                            n = 8,
                            nTrials,
                            maxIteration = 400,
                            nRepeats = 100) {

  output <- list()

  ## From Deff_fluctuation_tester we conclude that it is not required to take
  ## several random draws...
  desD <-
    AlgDesign::optFederov(frml, data = bundle, nTrials = nTrials, maxIteration = maxIteration, nRepeats = nRepeats)
  output$desD <- desD

  ## NOTE:
  ## does not work because of expand.formula -> deparse: width = 500 -> vector
  # output$desE <- AlgDesign::eval.design(frml, desD$design, confounding = TRUE)

  desB <-
    AlgDesign::optBlock(frml,
                        withinData = desD$design,
                        blocksizes = rep(n, nTrials / n),
                        criterion = "Dp")  ## no centering in our case

  output$desB <- desB

  ## Combine blocks into one design!
  blocks <- desB$Blocks
  final <-
    purrr::map2(1:length(blocks), blocks, function(x, y) {
      y$block <- x
      y
    })
  final <- purrr::reduce(final, rbind)


  output$final <- final

  output
}
