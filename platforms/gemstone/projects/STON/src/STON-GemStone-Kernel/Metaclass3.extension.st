Extension { #name : 'Metaclass3' }

{ #category : '*ston-gemstone-kernel' }
Metaclass3 class >> fromSton: stonReader [

	| theClassName symbolList |
	theClassName := stonReader parseListSingleton.
	symbolList := System myUserProfile symbolList.
	^(symbolList resolveSymbol:  theClassName)
		ifNil: [LookupError signal: theClassName asString, ' was not found']
		ifNotNil: [:assoc | assoc value class]
]

{ #category : '*ston-gemstone-kernel' }
Metaclass3 class >> stonName [

	^#Metaclass
]

{ #category : '*ston-gemstone-kernel' }
Metaclass3 >> stonName [

	^#Class
]

{ #category : '*ston-gemstone-kernel' }
Metaclass3 >> stonOn: stonWriter [
	stonWriter
		writeObject: self
		listSingleton: self thisClass name asSymbol
]
