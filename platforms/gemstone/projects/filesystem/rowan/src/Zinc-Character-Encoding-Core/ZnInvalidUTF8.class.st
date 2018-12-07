Class {
	#name : 'ZnInvalidUTF8',
	#superclass : 'ZnCharacterEncodingError',
	#category : 'Zinc-Character-Encoding-Core'
}

{ #category : 'private' }
ZnInvalidUTF8 >> defaultResumeValue [
	"$? codePoint"
	
	^ 63
]

{ #category : 'testing' }
ZnInvalidUTF8 >> isResumable [
	^ true
]
