Extension { #name : 'CharacterCollection' }

{ #category : '*filesystem-gemstone-kernel' }
CharacterCollection >> asZnCharacterEncoder [
	"Return a ZnCharacterEncoder instance using the receiver as identifier"
	
	" 'UTF-8' asZnCharacterEncoder "
	
	((self select: [ :each | each isAlphaNumeric ]) asLowercase) = 'utf8' ifFalse: [ self error: 'Only utf8 encoding supported'].
	^ ZnUTF8Encoder new

]
