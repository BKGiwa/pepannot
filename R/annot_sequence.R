#' Modify annotated sequence with indistinguishable residue positions.
#'
#' @description This function modifies an annotated sequence when residue positions are not distinguishable.
#'
#' @param seq A character vector representing amino acid sequence.
#' @param residue_posn A character vector specifying the positions of residues to annotates.
#' @param pattern A character vector specifying the pattern of the residue positions: use "positions not distinguishable" for this function.
#'
#' @returns A character vector representing the modified annotated sequence.
#'
#' @details This function checks if the residue positions in the sequence are not distinguishable.
#' If the positions are not distinguishable, it modifies the annotated sequence (results from the annotate_positions function) accordingly.
#' Specifically, it inserts '[+80]' after the first occurrence of 'S', 'T', or 'Y' if it is not already annotated with '[+80]'.
#' If there are multiple occurrences of 'S', 'T', or 'Y' and the first occurrence coincides with a position already annotated with '[+80]',
#' it inserts '[+80]' after the next occurrence of 'S', 'T', or 'Y'.
#'
#' @examples
#' seq <- "INSAESMELWTSYQK"
#' residue_posn <- "S; S6"
#' pattern <- "positions not distinguishable"
#' annot_sequence(seq, residue_posn, pattern)
#' # [1] "INS[+80]AES[+80]MELWTSYQK"
#'
#' @examples
#' seq <- "INSAESMELWTSYQK"
#' residue_posn <- "S; S3"
#' pattern <- "positions not distinguishable"
#' annot_sequence(seq, residue_posn, pattern)
#' # [1] "INS[+80]AES[+80]MELWTSYQK"
#'
#'
#' @examples
#' seq <- "INELEQSINDLRAEMGVEGTPPPASK"
#' residue_posn <- "M15; T/S"
#' pattern <- "positions not distinguishable"
#' annot_sequence(seq, residue_posn, pattern)
#' # [1] "INELEQS[+80]INDLRAEM[+16]GVEGTPPPASK"
#'
#' @examples
#' seq <- "KTDGSTTPAYAHGQHHSIFSPATGAVSDSSLK"
#' residue_posn <- "Y/T/S"
#' pattern <- "positions not distinguishable"
#' annot_sequence(seq, residue_posn, pattern)
#' # [1] "KT[+80]DGSTTPAYAHGQHHSIFSPATGAVSDSSLK"
#'
#'
#' @seealso \code{\link{annotate_positions}}, \code{\link{regexpr}}, \code{\link{substr}}
#'
#' @importFrom stringr str_match_all
#'
#' @export
annot_sequence <- function(seq, residue_posn, pattern) {
  # Check if seq and residue_posn are of the correct data types
  if (!is.character(seq) || !is.character(residue_posn)) {
    stop("Input sequence and residue positions must be character vectors.")
  }

  if (pattern != "positions not distinguishable") {
    return(annotate_positions(seq, residue_posn))
  } else {
    # First, apply modifications from annotate_positions
    annotated_seq <- annotate_positions(seq, residue_posn)


    # Apply additional modifications
    if (!is.character(annotated_seq)) {
      stop("Annotated sequence must be a character vector.")
    }

    for (i in seq_along(annotated_seq)) {
      # Find the position of the first occurrence of S, T, or Y
      first_STY_position <- min(regexpr("[STY]", annotated_seq[i]))
      # Check if first_STY_position coincides with a position where [+80] is already inserted by annotate_positions
      if (first_STY_position != -1 && grepl("\\[\\+80\\]", substr(annotated_seq[i], first_STY_position, first_STY_position + 5))) {
        # Find the position of the next occurrence of S, T, or Y after the first one
        next_substr <- substr(annotated_seq[i], first_STY_position + 1, nchar(annotated_seq[i]))
        second_STY_position <- min(regexpr("[STY]", next_substr))
        if (second_STY_position != -1) {
          # Insert [+]80 after the found amino acid
          annotated_seq[i] <- paste0(substr(annotated_seq[i], 1, first_STY_position + second_STY_position), "[+80]", substr(annotated_seq[i], first_STY_position + second_STY_position + 1, nchar(annotated_seq[i])))
        }
      } else {
        # Insert [+]80 after the found amino acid
        annotated_seq[i] <- paste0(substr(annotated_seq[i], 1, first_STY_position), "[+80]", substr(annotated_seq[i], first_STY_position + 1, nchar(annotated_seq[i])))
      }
    }
    return(annotated_seq)
  }
}
