Class {
	#name : 'ZnIncomplete',
	#superclass : 'ZnCharacterEncodingError',
	#category : 'Zinc-Character-Encoding-Core'
}

{ #category : 'private' }
ZnIncomplete >> defaultResumeValue [
	"$? codePoint"
	
	^ 63
]

{ #category : 'testing' }
ZnIncomplete >> isResumable [
	^ true
]
