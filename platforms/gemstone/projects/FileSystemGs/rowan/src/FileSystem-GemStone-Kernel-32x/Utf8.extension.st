Extension { #name : 'Utf8' }

{ #category : '*filesystem-gemstone-kernel-32x' }
Utf8 >> asString [
  "override the *filesystem  ByteArray >> asString"
  ^ self decodeToString   "or maybe  decodeToUnicode ??"
]
