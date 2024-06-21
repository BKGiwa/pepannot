
test_that("annot_sequence function correctly modifies annotated sequence", {

  # Test case 1: Check if the function correctly modifies sequence 'S' when residue positions are not distinguishable

  seq1 <- "INSAESMELWTSYQK"
  residue_posn1 <- "S; S6"
  pattern1 <- "positions not distinguishable"
  expected_output1 <- "INS[+80]AES[+80]MELWTSYQK"
  expect_equal(annot_sequence(seq1, residue_posn1, pattern1), expected_output1)


  # Test case 2: Check if the function correctly modifies sequence 'S' when residue positions are not distinguishable

  seq2 <- "INSAESMELWTSYQK"
  residue_posn2 <- "S; S3"
  pattern2 <- "positions not distinguishable"
  expected_output2 <- "INS[+80]AES[+80]MELWTSYQK"
  expect_equal(annot_sequence(seq2, residue_posn2, pattern2), expected_output2)


  # Test case 3: Check if the function correctly modifies sequence 'MTS' when residue positions are not distinguishable

  seq3 <- "INELEQSINDLRAEMGVEGTPPPASK"
  residue_posn3 <- "M15; T/S"
  pattern3 <- "positions not distinguishable"
  expected_output3 <- "INELEQS[+80]INDLRAEM[+16]GVEGTPPPASK"
  expect_equal(annot_sequence(seq3, residue_posn3, pattern3), expected_output3)


  # Test case 4: Check if the function correctly modifies sequence 'STY' when residue positions are not distinguishable

  seq4 <- "KTDGSTTPAYAHGQHHSIFSPATGAVSDSSLK"
  residue_posn4 <- "Y/T/S"
  pattern4 <- "positions not distinguishable"
  expected_output4 <- "KT[+80]DGSTTPAYAHGQHHSIFSPATGAVSDSSLK"
  expect_equal(annot_sequence(seq4, residue_posn4, pattern4), expected_output4)


  # Test case 5: Check if the function correctly modifies sequence 'STY' when residue positions are not distinguishable

  seq5 <- "KTDGTSTPAYAHMGQHHSIFSPATGAVSDSSLK"
  residue_posn5 <- "Y/T/S; T2; S6; Y10; M13"
  pattern5 <- "positions not distinguishable"
  expected_output5 <- "KT[+80]DGT[+80]S[+80]TPAY[+80]AHM[+16]GQHHSIFSPATGAVSDSSLK"
  expect_equal(annot_sequence(seq5, residue_posn5, pattern5), expected_output5)


  # Test case 6: Invalid input with non-character vectors
  seq6 <- "MSTY"
  residue_posn6 <-c(1, 2, 3)
  pattern6 <- "positions not distinguishable"
  expect_error(annot_sequence(seq6, residue_posn6, pattern6), "Input sequence and residue positions must be character vectors.")


  # Test case 7: Invalid residue position containing non-MSTY letters
  seq7 <- "MSSTYM"
  residue_posn7 <- c("R1; A2; H5;P7")  # R,A,H,P are invalid
  pattern7 <- "positions not distinguishable"
  expect_error(annot_sequence(seq7, residue_posn7, pattern7), "Invalid residue position detected. Residue positions should contain only 'M', 'S', 'T', or 'Y'.")


})

