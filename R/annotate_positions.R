#' Annotate position in a sequence
#'
#' @description This function annotates residues in a given sequence with additional characters. This code is executed when the residue position is distinguishable.
#'
#' @param seq A character vector representing amino acid sequence.
#' @param residue_posn A character vector specifying the positions of residues to annotates.
#'
#' @returns A single character vector representing the annotated sequence.
#'
#' @details This function takes a sequence and a vector of residue positions and annotates those positions with specific characters.
#' The data needs to be cleaned to some extent so that the residues in residues_posn should only be 'M', 'S', 'T', or 'Y'.
#' Each of these residues will be annotated with specific strings: 'S' with '[+80]', 'M' with '[+16]', 'T' with '[+80]', and 'Y' with '[+80]'.
#' The addition of  80 dalton mass [+80] to S, Y, and T indicates phosphorylation while addition of [+16] to M indicates oxidation.
#' The annotated sequence will be returned with the inserted strings at the respective positions.
#'
#' @examples
#' seq <- "MSTY"
#' residue_posn <- "M1; S2; T3; Y4"
#' annotate_positions(seq, residue_posn)
#' # [1] "M[+16]S[+80]T[+80]Y[+80]"
#'
#' @seealso \code{\link{grepl}}, \code{\link[stringr]{str_match_all}}
#'
#' @importFrom stringr str_match_all
#'
#' @export
annotate_positions <- function(seq, residue_posn) {
  # Validate input
  if (!is.character(seq) || !is.character(residue_posn)) {
    stop("Both 'seq' and 'residue_posn' must be character vectors.")
  }


  for (pos in residue_posn) {
    if (any(!grepl("[MSTY]", pos))) {
      stop("Invalid residue position detected. Residue positions should contain only 'M', 'S', 'T', or 'Y'.")
    }
  }


  sub_residues <- residue_posn
  res_id <- stringr::str_match_all(sub_residues, "([MSTY])\\d+")[[1]][,2]
  names(res_id) <- stringr::str_match_all(sub_residues, "[MSTY](\\d+)")[[1]][,2]


  insert_s <- "[+80]"
  insert_m <- "[+16]"
  insert_t <- "[+80]"
  insert_y <- "[+80]"

  extra_chars <- (sum(grepl("S", res_id)) * nchar(insert_s) ) +
    (sum(grepl("M", res_id)) * nchar(insert_m) ) +
    (sum(grepl("T", res_id)) * nchar(insert_t) ) +
    (sum(grepl("Y", res_id)) * nchar(insert_y) )

  final_seq <- seq
  final_seq_length <- nchar(seq[1]) + extra_chars

  i = 1
  for (j in 1:nchar(seq)) {
    if (as.character(j) %in% names(res_id) & substr(seq, j, j) %in% c("S")) {
      final_seq <- paste0(substr(final_seq[1], 1, i), insert_s, substr(final_seq[1], i + 1, nchar(final_seq)))
      i = i + nchar(insert_s)
    } else if (as.character(j) %in% names(res_id) & substr(seq, j, j) %in% c("M")) {
      final_seq <- paste0(substr(final_seq[1], 1, i), insert_m, substr(final_seq[1], i + 1, nchar(final_seq)))
      i = i + nchar(insert_m)
    } else if (as.character(j) %in% names(res_id) & substr(seq, j, j) %in% c("T")) {
      final_seq <- paste0(substr(final_seq[1], 1, i), insert_t, substr(final_seq[1], i + 1, nchar(final_seq)))
      i = i + nchar(insert_t)
    } else if (as.character(j) %in% names(res_id) & substr(seq, j, j) %in% c("Y")) {
      final_seq <- paste0(substr(final_seq[1], 1, i), insert_y, substr(final_seq[1], i + 1, nchar(final_seq)))
      i = i + nchar(insert_y)
    }
    i = i + 1
  }

  return(final_seq)
}
