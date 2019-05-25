Extension { #name : 'Number' }

{ #category : '*ast-kernel-core' }
Number >> rbStoreOn: aStream [
  self printOn: aStream

]

{ #category : '*ast-kernel-core' }
Number class >> rwFromStream: aStream [
  "BinaryExponent = ( 'e' | 'E' | 'd' | 'D'  'q' | 'Q') ['-' | '+'] Digits
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
  | ch integerPart factor signFound |
  self _checkReadStream: aStream forClass: CharacterCollection.
  ch := aStream next.
  [ ch unicodeIsWhitespace] whileTrue: [ ch := aStream next ].
  aStream skip: -1.
  integerPart := 0.
  factor := 1.
  signFound := false.
  (aStream peek isEquivalent: $-)
    ifTrue: [ 
      aStream next.
      signFound := true.
      factor := -1 ]
    ifFalse: [ 
      (aStream peek isEquivalent: $+)
        ifTrue: [ 
          aStream next.
          signFound := true ] ].
  (aStream atEnd or: [ aStream peek isDigit not ])
    ifTrue: [ ^ self _errIncorrectFormat: aStream ].
  [ aStream atEnd not and: [ (ch := aStream peek) isDigit ] ]
    whileTrue: [ 
      aStream next.
      integerPart := integerPart * 10 + ch digitValue ].

  (ch == $r or: [ ch == $# ])
    ifTrue: [ 
      "found a radix separator"
      aStream next.
      ^ Integer
        _rwFinishFromStream: aStream
        signFound: signFound
        factor: factor
        radix: integerPart ].
  (ch isEquivalent: (Locale decimalPoint at: 1))
    ifTrue: [ 
      | chch |
		chch := aStream peek2. ".?"
		chch notNil
			ifTrue: [
				"not at eof"
				chch isDigit 
					ifFalse: [
						"we're done"
						^ integerPart * factor ] ].
	  "found a decimal point, followed by a numeric character."
      aStream next.
      ^ BinaryFloat
        _finishFromStream: aStream
        signFound: signFound
        factor: factor
        integerPart: integerPart ].
  (#( $e $E $d $D $q $s $p $f $F ) includesIdentical: ch) ifTrue:[ 
      aStream next.
      ^ BinaryFloat
        _finishFromStream: aStream
        signFound: signFound
        factor: factor
        integerPart: integerPart
        fractionalPart: 0
        exponent: ch ].
  ^ integerPart * factor
]
