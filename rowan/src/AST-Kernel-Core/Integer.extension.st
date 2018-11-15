Extension { #name : 'Integer' }

{ #category : '*ast-kernel-core' }
Integer class >> _rwFinishFromStream: aStream signFound: aSignFound factor: aFactor radix: radix [
  "BinaryExponent = ( 'e' | 'E' | 'd' | 'D' | 'q' ) ['-' | '+'] Digits
   DecimalExponent = ( 'f' | 'F' ) ['-' | '+'] Digits
   Digit = '0' | '1' | '2' | ... | '9'
   Digits = Digit {Digit}
   Exponent = BinaryExponent | DecimalExponent | ScaledDecimalExponent | FixedPointExponent
   FractionalPart = '.' Digits [Exponent]
   FixedPointExponent = 'p' [ ['-' | '+'] Digits ] 
   Number = RadixedLiteral | NumericLiteral
   Numeric = Digit | 'A' | 'B' | ... | 'Z'
   NumericLiteral = Digits ( [FractionalPart] | [Exponent] )
   Numerics = Numeric { Numeric }
   RadixedLiteral = Digits ( '#' | 'r' ) ['-'] Numerics
   ScaledDecimalExponent = 's' [ ['-' | '+'] Digits ]
"
  "parse Numerics (plus option sign) for RadixedLiteral"

  | number ch signFound factor |
  signFound := aSignFound.
  factor := aFactor.
  number := 0.
  (aStream peek isEquivalent: $-)
    ifTrue: [ 
      signFound
        ifTrue: [ ^ self _errIncorrectFormat: aStream ].
      aStream next.
      signFound := true.
      factor := -1 ]
    ifFalse: [ 
      (aStream peek isEquivalent: $+)
        ifTrue: [ 
          signFound
            ifTrue: [ ^ self _errIncorrectFormat: aStream ].
          aStream next.
          signFound := true ] ].
  [ aStream atEnd not and: [ | ch | ch := aStream peek. radix <= 10 ifTrue: [ ch isDigit ] ifFalse: [ ch isAlphaNumeric ] ] ]
    whileTrue: [ 
      ch := aStream next.
      number := number * radix + (ch digitValueInRadix: radix) ].
  ^ number * factor
]
