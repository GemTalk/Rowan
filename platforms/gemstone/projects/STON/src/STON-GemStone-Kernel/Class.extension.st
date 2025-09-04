Extension { #name : 'Class' }

{ #category : '*ston-gemstone-kernel' }
Class class >> fromSton: stonReader [
	| theClassName symbolList |
	theClassName := stonReader parseListSingleton.
	symbolList := System myUserProfile symbolList.
	^(symbolList resolveSymbol:  theClassName)
		ifNil: [LookupError signal: theClassName asString, ' was not found']
		ifNotNil: [:assoc | assoc value]
]

{ #category : '*ston-gemstone-kernel' }
Class >> stonName [
	"Override to encode my instances using a different class name."
	
	^ self name

]

{ #category : '*ston-gemstone-kernel' }
Class >> stonOn: stonWriter [
	stonWriter
		writeObject: self
		listSingleton: self name asSymbol
]
