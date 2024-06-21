
test_that("annotate_positions function correctly modifies annotated sequence", {

  # Test case 1: Check if the function correctly modifies sequence 'M' when residue positions are distinguishable

  seq1 <- "M"
  residue_posn1 <- "M1"
  expected_output1 <- "M[+16]"
  expect_equal(annotate_positions(seq1, residue_posn1), expected_output1)

  # Test case 2: Check if the function correctly modifies sequence 'S' when residue positions are distinguishable

  seq2 <- "S"
  residue_posn2 <- "S1"
  expected_output2 <- "S[+80]"
  expect_equal(annotate_positions(seq2, residue_posn2), expected_output2)

  # Test case 3: Check if the function correctly modifies sequence 'T' when residue positions are distinguishable

  seq3 <- "T"
  residue_posn3 <- "T1"
  expected_output3 <- "T[+80]"
  expect_equal(annotate_positions(seq3, residue_posn3), expected_output3)

  # Test case 4: Check if the function correctly modifies sequence 'Y' when residue positions are distinguishable

  seq4 <- "Y"
  residue_posn4 <- "Y1"
  expected_output4 <- "Y[+80]"
  expect_equal(annotate_positions(seq4, residue_posn4), expected_output4)


  # Test case 5: Check if the function correctly modifies sequence 'MSTY' when residue positions are distinguishable

  seq5 <- "INSAESMELWTSYQK"
  residue_posn5 <- "M7; S3; T11; Y13"
  expected_output5 <- "INS[+80]AESM[+16]ELWT[+80]SY[+80]QK"
  expect_equal(annotate_positions(seq5, residue_posn5), expected_output5)


  # Test case 6: Invalid input with non-character vectors
  seq6 <- "MSTY"
  residue_posn6 <- c(1, 2, 3)
  expect_error(annotate_positions(seq6, residue_posn6), "Both 'seq' and 'residue_posn' must be character vectors.")

  # Test case 7: Invalid residue position containing non-MSTY letters
  seq7 <- "MSSTYM"
  residue_posn7 <- "R1; A2; H5; P7"  # R,A,H,P are invalid
  expect_error(annotate_positions(seq7, residue_posn7), "Invalid residue position detected. Residue positions should contain only 'M', 'S', 'T', or 'Y'.")

})
