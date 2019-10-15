Extension { #name : 'CharacterCollection' }

{ #category : '*filesystem-gemstone-kernel' }
CharacterCollection >> asZnCharacterEncoder [
	"Return a ZnCharacterEncoder instance using the receiver as identifier"
	
	" 'UTF-8' asZnCharacterEncoder "
	
	((self select: [ :each | each isAlphaNumeric ]) asLowercase) = 'utf8' ifFalse: [ self error: 'Only utf8 encoding supported'].
	^ ZnUTF8Encoder new

]

{ #category : '*filesystem-gemstone-kernel' }
CharacterCollection >> encodeWith: encoding [
	"Produce a ByteArray that encodes the receiver, using a specified encoding.
	Encoding is either a ZnCharacterEncoder instance or an identifier for one."
	
	" 'Les élèves français' encodeWith: #utf8 "
	
	^ encoding asZnCharacterEncoder encodeString: self

]

{ #category : '*filesystem-gemstone-kernel' }
CharacterCollection class >> crlf [
	"Answer a string containing a carriage return and a linefeed."

	^ self with: Character cr with: Character lf
]

{ #category : '*filesystem-gemstone-kernel' }
CharacterCollection >> asFileReference [

	^ FileSystem disk referenceTo: self
]

{ #category : '*filesystem-gemstone-kernel' }
CharacterCollection >> asPath [
	"convert myself to a path"
	"Examples:
		'.' asPath
		'~/Desktop' asPath
		'/home/foo' asPath
		'../../foo.bar' asPath"
	^ FileSystem disk resolve: self
]

{ #category : '*filesystem-gemstone-kernel' }
CharacterCollection >> asPathWith: anObject [ 
	^ anObject pathFromString: self
]

{ #category : '*filesystem-gemstone-kernel' }
CharacterCollection >> asResolvedBy: aFileSystem [
	^ aFileSystem resolveString: self
]
