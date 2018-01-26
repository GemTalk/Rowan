"
The MethodRegistry maps GsNMethods to LoadedMethods.
"
Class {
	#name : 'RwGsLoadedSymbolDictMethod',
	#superclass : 'RwLoadedMethod',
	#classVars : [
		'MethodRegistry'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'instance creation' }
RwGsLoadedSymbolDictMethod class >> forMethod: aGsNMethod [

	| instance |
	instance := self newNamed: aGsNMethod selector asString.
	instance
		handle: aGsNMethod;
		updateForProtocolChange;
		updateIsMetaFromImage.
	^instance
]

{ #category : 'private' }
RwGsLoadedSymbolDictMethod >> definingPackageOfClass [

	^loadedClass loadedPackage
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictMethod >> key [
	"Answer some token that will uniquely identify me relative to any other LoadedMethod in the same LoadedClass."

	^name	"This is inadequate because the same selector can be defined in both class and metaclass."
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
RwGsLoadedSymbolDictMethod >> updateIsMetaFromImage [

	classIsMeta := handle inClass isMeta
]
