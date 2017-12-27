"
No class-specific documentation for CypLoadedMethod, hierarchy is: 
Object
  CypLoadedThing( properties name handle)
    CypLoadedMethod

"
Class {
	#name : 'CypLoadedMethod',
	#superclass : 'CypLoadedThing',
	#instVars : [
		'loadedClass',
		'classIsMeta'
	],
	#category : 'Cypress-Core'
}

{ #category : 'definitions' }
CypLoadedMethod >> asDefinition [
	"Create a Definition corresponding to what I define in the image."

	^CypMethodDefinition withProperties: self propertiesForDefinition
		source: self source
]

{ #category : 'accessing' }
CypLoadedMethod >> classIsMeta [

	^classIsMeta
]

{ #category : 'accessing' }
CypLoadedMethod >> classIsMeta: aBoolean [

	classIsMeta := aBoolean
]

{ #category : 'queries' }
CypLoadedMethod >> loadedClass [

	^loadedClass
]

{ #category : 'accessing' }
CypLoadedMethod >> loadedClass: aLoadedClass [

	loadedClass := aLoadedClass
]

{ #category : 'queries' }
CypLoadedMethod >> loadedPackage [

	^self loadedClass loadedPackage
]

{ #category : 'private' }
CypLoadedMethod >> markPackageDirty [
	"LoadedClass may still be nil if we're creating a new LoadedMethod."

	loadedClass ifNotNil: [loadedClass markPackageDirty]
]

{ #category : 'definitions' }
CypLoadedMethod >> propertiesForDefinition [

	| props |
	props := super propertiesForDefinition.
	props at: 'selector' put: name.
	^props
]

{ #category : 'accessing' }
CypLoadedMethod >> source [

	self subclassResponsibility
]
