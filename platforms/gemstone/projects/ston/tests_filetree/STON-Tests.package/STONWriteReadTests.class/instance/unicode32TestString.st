private
unicode32TestString
  "Cannot embed Unicode16 or Unicode32 strings in 2.x methods"

  ^ self include32BitUnicodeStrings
    ifTrue: [ 'ð£' decodeFromUTF8 ]
    ifFalse: [ 'abc' ]