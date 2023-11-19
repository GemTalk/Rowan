Extension { #name : 'Number' }

{ #category : '*ast-kernel-core' }
Number class >> rwFromStream: aStream [
  "BinaryExponent = ( 'e' | 'E' | 'd' | 'D' | 'q') ['-' | '+'] Digits
   DecimalExponent = ( 'f' | 'F' ) ['-' | '+'] Digits
   Digit = '0' | '1' | '2' | ... | '9'
   Digits = Digit {Digit}
   Exponent = BinaryExponent | DecimalExponent | ScaledDecimalExponent | FixedPointExponent
   FractionalPart = '.' Digits [Exponent]
   FixedPointExponent = 'p' [ ['-' | '+'] Digits ]
   Number = [ - ] RadixedLiteral | [ '-' ] NumericLiteral | RadixedLiteralNegBody
   Numeric = Digit | 'A' | 'B' | ... e 'Z'
   NumericLiteral = Digits ( [FractionalPart] | [Exponent] )
   Numerics = Numeric { Numeric }
   RadixedLiteral = Digits ( '#' | 'r' ) Numerics
   RadixedLiteralNegBody =  Digits ( '#' | 'r' ) '-' Numerics
   ScaledDecimalExponent = 's' [ ['-' | '+'] Digits ]
"
  | ch integerPart sign dCount |
  self _checkReadStream: aStream forClass: CharacterCollection.
  aStream peek unicodeIsWhitespace ifTrue:[
    [ aStream next unicodeIsWhitespace ] whileTrue.
    aStream skip: -1.
  ].
  (aStream peek == $-) 
    ifTrue: [ aStream next.  sign := $- .  ]
    ifFalse: [ (aStream peek isEquivalent: $+) ifTrue: [ aStream next.  sign := $+ ] ].
  dCount := 0 .
  integerPart := 0.
  [ (ch := aStream peek) ~~ nil and: [ ch isDigit ] ] whileTrue: [
    aStream next.
    integerPart := integerPart * 10 + ch digitValue .
    dCount := dCount + 1 
  ].
  dCount == 0 ifTrue:[ ^ self _errIncorrectFormat: aStream ].
  (ch == $r or: [ ch == $# ]) ifTrue: [ "found a radix separator" 
    aStream next.
    ^ Integer _fromStream: aStream radix: integerPart sign: sign .
  ].
  (ch isEquivalent: (Locale decimalPoint at: 1)) 
    ifTrue: [ 
      "found a decimal point" 
      | chch |
      chch := aStream peek2. "check character following deciml point"
      chch notNil
        ifTrue: [
				"eof does not follow the decimal point"
				chch isDigit 
					ifFalse: [
						"decimal point is followed by a non-digit, so decimal point should not be treated as part of number"
                        sign == $- ifTrue:[ ^ 0 - integerPart ].
						^ integerPart ] ].
    aStream next.
    ^ BinaryFloat _finishFromStream: aStream sign: sign integerPart: integerPart 
  ].
  (#( $e $E $s $f $F $q $p $d $D ) includesIdentical: ch) ifTrue:[
    ^ BinaryFloat _finishFromStream: aStream sign: sign integerPart: integerPart .
  ].
  sign == $- ifTrue:[ ^ 0 - integerPart ].
  ^ integerPart
]

{ #category : '*ast-kernel-core' }
Number >> rbStoreOn: aStream [
  self printOn: aStream

]
