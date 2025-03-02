  $ lexa ./option.lx -o main &> /dev/null
  $ ./main
  OK

  $ lexa ./option_get_fail.lx -o main &> /dev/null
  $ ./main
  Error: option is None
  [1]
