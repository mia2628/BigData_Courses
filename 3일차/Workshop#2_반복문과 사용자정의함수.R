#문제1. for문을 이용해 구구단 2~9단 만들기
for(i in 2:9) {
  for(j in 1:9) {
    print(paste(i, "x", j, "=", i*j))
  }
}

#문제2.1부터 100까지의 수 중에서 3의 배수이면서 4의 배수는 아닌 수의 합을 구하라.
j <- 0
for(i in 1:100) {
  if(i %% 3 == 0 & i %% 4 != 0) {
    j <- j + i
  }
}
print(j)

#문제3. x와 n을 입력하면 1부터 n까지의 수 중에서 x의 배수 합을 구해주는 사용자 정의 함수를 만들어라.
multiple <- 0

Func <- function(x,n) {
  for(i in 1:n){
    if(i%%x == 0 & i <= n) {
      j <- i
      multiple = multiple + j
    }
  }
  return(multiple)
}

Func(3,100)