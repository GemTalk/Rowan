tests
testWhitespace
  | whitespace |
  whitespace := String
    withAll:
      {(Character space).
      (Character tab).
      (Character cr).
      (Character lf)}.
  self assert: (self materialize: whitespace , '123') = 123