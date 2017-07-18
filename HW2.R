# answer 1
n_iter = 100000
n = 1
cn = as.data.frame(matrix(ncol = 3, nrow = n_iter))

while(n <= n_iter) {
  all_rand <- runif(10000, 0, 1)
  head_tail <- ifelse( all_rand > 0.5, 1, 0)
  cn[n,1] <- mean(c(head_tail[1:10]))
  c2_index <- floor(runif(1,1,1001))
  cn[n,2] <- mean(c(head_tail [ ((c2_index - 1)*10 + 1) : ((c2_index - 1)*10 + 10) ]))
  i = 1
  min_index = i
  min_sum_heads = 10
  
  while(i <= 1000) {
    sum_heads = sum(head_tail [ ((i - 1)*10 + 1) : ((i - 1)*10 + 10) ])
    if(min_sum_heads > sum_heads) {
      min_sum_heads = sum_heads
      min_index = i
    }
    i = i + 1
  }
  
  cn[n,3] = min_sum_heads/10
  
  n = n + 1
}

mean(cn[,3])
