# test script for midterm2 submission

# this script expects the following variables to already be present:
# true_process: matrix of 104 times 5, 
#                         each column is the true realization of the time series in corresponding question
# flist: a list of file names of all student submissions,
#                         this can be generated by the command list.files() on a folder containing
#                         only student submissions


setwd("/Users/sambamamba/Documents/Cal Spring 2017/STAT_153/MT_2/GoogleTimeSeries")

wd <- getwd(); items <- dir()

dtasets <-  items[grepl(".csv", items) == TRUE]
dataList <- matrix(unlist(lapply(dtasets, 
                                 function(dta) read_csv(file.path(wd, dta))$activity)), nrow = 5)

# dummy entries for now
set.seed(0)
true_process = dataList #matrix(rnorm(104 * 5), 104, 5)
flist = 'Q1_Cindy_Kang_0.txt' #'Q1_Samba_Njie_23075185.txt' #'Q1_Alex_DAmour_123456.txt' 

# th function extract_sub() would process one student submission, identified by its file name
# it returns a list containing
# qid: which question was attempted
# sid: student's SID
# sname: student's name
# predictions: a vector of length 104, this students forecast for this question
# mse: mean squared error between forecast and truth
extract_sub = function(fname){
  robj = NULL
  
  tryCatch({
    stopifnot(file.exists(fname))
    # reading file
    # this command would work if a header is present
    fi = as.numeric(drop(as.matrix(read.csv(fname))))
    stopifnot(length(fi) %in% c(103, 104))
    if(length(fi) == 103) {
      # rerunning without header
      fi = drop(as.matrix(read.csv(fname, header = FALSE)))
      stopifnot(length(fi) == 104)
    }
    # extracting student information
    pos_ = as.vector(gregexpr('_', fname)[[1]])
    pos. = as.vector(gregexpr('\\.', fname)[[1]])
    sid = as.numeric(substr(fname, tail(pos_,1)+1, pos.-1))
    stopifnot(is.numeric(sid))
    qid = as.numeric(substr(fname, 2, 2))
    stopifnot(is.numeric(qid))
    sname = substr(fname, head(pos_,1)+1, tail(pos_,1)-1)
    # returning everything
    robj = list(predictions = fi,
                mse = mean((true_process[,qid] - fi)^2),
                qid = qid, sid = sid, sname = sname)
  }, error = function(e) {print(fname); print(e)})
  return(robj)
  
}

# all student submissions are then processed by
sublist = lapply(flist, extract_sub)
# removing all entries that threw an error
problem_indices = which(sapply(sublist, is.null))
problem_fnames = flist[problem_indices]
if(length(problem_indices)) {sublist = sublist[-problem_indices]}

# now we construct a matrix where rows determine students and columns determine questions
# values in the matrix will denote MSE, an unattempted question defaults to +infinity

# how many students submitted at least one answer?
student_list = unique(sapply(sublist, function(ss) return(ss$sid)))

student_perf = matrix(Inf, length(student_list), 6)
colnames(student_perf) = c('SID', 'Q1', 'Q2', 'Q3', 'Q4', 'Q5')
student_perf[,1] = student_list

nothing_to_see_here = lapply(sublist, function(ss) {
  rowid = which(student_list == ss$sid)
  student_perf[rowid, ss$qid+1] <<- ss$mse
  return(NULL)
})

# student_perf now contains all information we need for grading

student_perf
# in the actual use case, we would now save this matrix, (also process it to get grades)

