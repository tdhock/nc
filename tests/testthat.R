data.table::setDTthreads(1)
Sys.setlocale("LC_MESSAGES",locale="C")
if(require(testthat))test_check("nc")
