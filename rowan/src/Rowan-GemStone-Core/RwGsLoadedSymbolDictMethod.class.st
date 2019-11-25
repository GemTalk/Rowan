"
The MethodRegistry maps GsNMethods to LoadedMethods.
"
Class {
	#name : 'RwGsLoadedSymbolDictMethod',
	#superclass : 'RwLoadedMethod',
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'instance creation' }
RwGsLoadedSymbolDictMethod class >> forMethod: aGsNMethod [

	| instance |
	instance := self newNamed: aGsNMethod selector.
	instance
		handle: aGsNMethod;
		updateFromHandle.
	^ instance
]

{ #category : 'private' }
RwGsLoadedSymbolDictMethod >> definingPackageOfClass [

	^loadedClass loadedPackage
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictMethod >> handleClassDeletion [

	"The class to which I refer has been deleted, so I must unregister myself."

	Rowan image removeLoadedMethodForCompileMethod: handle
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictMethod >> key [

	"Answer some token that will uniquely identify me relative to any other LoadedMethod in the same LoadedClass."

	^ name	"This is inadequate because the same selector can be defined in both class and metaclass?"
]

{ #category : 'printing' }
RwGsLoadedSymbolDictMethod >> printOn: aStream [

	super printOn: aStream.
	name
		ifNotNil: 
			[aStream
				space;
				nextPutAll: handle inClass printString, ' ( ', handle inClass asOop asString, ' )' ]
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictMethod >> source [

	^handle sourceString copy
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictMethod >> updateForProtocolChange [
	"Check to see whether I'm in a different protocol. If I am, that could change which package I'm in and I need to update that."

	| oldProtocol newProtocol |
	oldProtocol := self propertyAt: 'protocol'.
	newProtocol := (handle inClass categoryOfSelector: handle selector)
				asString.

	"Must test for AbsentToken by identity, or a new protocol of '<absent>' would not  be detected."
	oldProtocol == AbsentToken | (oldProtocol ~= newProtocol)
		ifTrue: [	self propertyAt: 'protocol' put: newProtocol ]
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictMethod >> updateFromHandle [

	"update method properties: selector and protocol, and classIsMeta"

	self updateForProtocolChange.
	self propertyAt: 'selector' put: handle selector asSymbol.
	classIsMeta := handle inClass isMeta
]
