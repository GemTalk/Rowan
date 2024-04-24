Class {
	#name : 'RwGsLoadedSymbolDictTraitMethod',
	#superclass : 'RwGsLoadedSymbolDictMethod',
	#instVars : [
		'source'
	],
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'instance creation' }
RwGsLoadedSymbolDictTraitMethod class >> loadedClassMethodForTrait: aTrait source: sourceString protocol: protocol [
	| instance sel |
	sel := self class extractSelector: sourceString.
	instance := self newNamed: sel.
	instance
		classIsMeta: true;
		handle: aTrait;
		source: sourceString;
		propertyAt: 'protocol' put: protocol;
		updateFromHandle.
	^ instance
]

{ #category : 'instance creation' }
RwGsLoadedSymbolDictTraitMethod class >> loadedMethodForTrait: aTrait source: sourceString protocol: protocol [
	| instance sel |
	sel := self class extractSelector: sourceString.
	instance := self newNamed: sel.
	instance
		classIsMeta: false;
		handle: aTrait;
		source: sourceString;
		propertyAt: 'protocol' put: protocol;
		updateFromHandle.
	^ instance
]

{ #category : 'definitions' }
RwGsLoadedSymbolDictTraitMethod >> asDefinition [
	"Create a Definition corresponding to what I define in the image."

	^RwTraitMethodDefinition withProperties: self propertiesForDefinition
		source: self source
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictTraitMethod >> classIsMeta: aBool [
	classIsMeta := aBool
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictTraitMethod >> source [
	^source
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictTraitMethod >> source: object [
	source := object
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictTraitMethod >> updateFromHandle [
	"update method properties: selector , and classIsMeta"

	self propertyAt: #'selector' put: self name asSymbol
]
