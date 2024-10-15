#' Calculate UMAP and TSNE
#'
#' @param spe spe object
#' @param email_address email address to send updates
#' @param BPPARAM parallelization param
#'
#' @return spe object
#' @export
#'
#' @examples
#'
#' calc_umap_tsne(spe, 't0jone12@louisville.edu')
#'
calc_umap_tsne <- function(spe, email_address, BPPARAM = NULL) {

  if (!"gmail.send" %in% names(gmailr::gm_scopes())) {
    stop("must have permissions to send emails")

  }
  email <- gmailr::gm_mime() |>
    gmailr::gm_to({{ email_address }}) |>
    gmailr::gm_from("bccbiostatsemailer@gmail.com") |>
    gmailr::gm_subject("UMAP/TSNE Has Begun") |>
    gmailr::gm_text_body("Calculating UMAP and TSNE has begun")

  gm_send_message(email)

  begin <- Sys.time()

  if (is.null(BPPARAM)) {
    spe <- scater::runUMAP(spe,
                           subset_row = rowData(spe)$use_channel,
                           exprs_values = "exprs")

    spe <- scater::runTSNE(spe,
                           subset_row = rowData(spe)$use_channel,
                           exprs_values = "exprs")
  } else {

    spe <- runUMAP(spe,
                   subset_row = rowData(spe)$use_channel,
                   exprs_values = "exprs",
                   BPPARAM = {{ BPPARAM }})
    spe <- runTSNE(spe,
                   subset_row = rowData(spe)$use_channel,
                   exprs_values = "exprs",
                   BPPARAM = {{ BPPARAM }})

    # stop parallel backend
    stopCluster({{ BPPARAM }})

  }



  end <- Sys.time()

  email2 <- gmailr::gm_mime() |>
    gmailr::gm_to({{ email_address }}) |>
    gmailr::gm_from("bccbiostatsemailer@gmail.com") |>
    gmailr::gm_subject("UMAP/TSNE Has Finished") |>
    gmailr::gm_text_body(paste0("UMAP and TSNE is done. Time: ",
                                end-begin) )

  gm_send_message(email2)

  return(spe)

  }
