batch_correction <- function(spe, email_address, BPPARAM = NULL) {

  if (!"gmail.send" %in% names(gmailr::gm_scopes())) {
    stop("must have permissions to send emails")

  }
  email <- gmailr::gm_mime() |>
    gmailr::gm_to({{ email_address }}) |>
    gmailr::gm_from("bccbiostatsemailer@gmail.com") |>
    gmailr::gm_subject("Batch Effect Correction Has Begun") |>
    gmailr::gm_text_body("Batch effect correction has begun")

  gm_send_message(email)

  begin <- Sys.time()

  if (is.null(BPPARAM)) {

    classic.out <- mnnCorrect(spe, batch = spe$patient_id,
                              auto.merge = TRUE,
                              subset.row = rowData(spe)$use_channel,
                              assay.type = "exprs")

  } else {

    classic.out <- mnnCorrect(spe, batch = spe$patient_id,
                              auto.merge = TRUE,
                              subset.row = rowData(spe)$use_channel,
                              assay.type = "exprs",
                              BPPARAM = {{ BPPARAM }})

    # stop parallel backend
    stopCluster({{ BPPARAM }})

  }



  end <- Sys.time()

  email2 <- gmailr::gm_mime() |>
    gmailr::gm_to({{ email_address }}) |>
    gmailr::gm_from("bccbiostatsemailer@gmail.com") |>
    gmailr::gm_subject("Batch Effect Correction Has Finished") |>
    gmailr::gm_text_body(paste0("Batch Effect Correction is done. Time: ",
                                end-begin) )

  gm_send_message(email2)

  return(spe)

}
