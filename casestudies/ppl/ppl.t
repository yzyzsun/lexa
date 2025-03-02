  $ lexa test.lx -o main &> /dev/null
  $ ./main
  2
  1
  0
  1
  $ lexa simLinRegr.lx -o main &> /dev/null
  $ ./main 10
  16.511500
  $ lexa lwLinRegr.lx -o main &> /dev/null
  $ ./main 10
  2.100943
  $ lexa mhLinRegr.lx -o main &> /dev/null
  $ ./main 10
  2.557474
