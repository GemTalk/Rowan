"
properties: dictionary of properties
name: name of class or package, selector of method
handle: reference to the class or method object, or to the native package object for dialects that have that.
"
Class {
	#name : 'CypLoadedThing',
	#superclass : 'Object',
	#instVars : [
		'properties',
		'name',
		'handle'
	],
	#classVars : [
		'AbsentToken'
	],
	#category : 'Cypress-Core'
}

{ #category : 'class initialization' }
CypLoadedThing class >> initialize [
	"CypLoadedThing initialize"

	AbsentToken := '<absent>'
]

{ #category : 'instance creation' }
CypLoadedThing class >> newNamed: aName [

	^(self new)
		initialize;
		initializeForName: aName;
		yourself
]

{ #category : 'accessing' }
CypLoadedThing >> absentToken [

	^AbsentToken
]

{ #category : 'private' }
CypLoadedThing >> definitionsFor: loadedThings [

	| result |
	result := Dictionary new: loadedThings size.
	loadedThings
		do: [:loadedThing | result at: loadedThing key put: loadedThing asDefinition].
	^result
]

{ #category : 'accessing' }
CypLoadedThing >> handle [
	"Answer a reference to the platform-specific image object for which I hold information."

	^handle
]

{ #category : 'accessing' }
CypLoadedThing >> handle: anObject [
	"Set my reference to the platform-specific image object for which I hold information."

	handle := anObject
]

{ #category : 'initialization' }
CypLoadedThing >> initialize [

	properties := Dictionary new
]

{ #category : 'initialization' }
CypLoadedThing >> initializeForName: aName [

	name := aName
]

{ #category : 'accessing' }
CypLoadedThing >> name [

	^name
]

{ #category : 'accessing' }
CypLoadedThing >> name: aString [

	self markPackageDirty.
	name := aString
]

{ #category : 'printing' }
CypLoadedThing >> printOn: aStream [

	super printOn: aStream.
	name
		ifNotNil: 
			[aStream
				space;
				nextPutAll: name]
]

{ #category : 'private' }
CypLoadedThing >> propertiesForDefinition [

	^properties copy
]

{ #category : 'accessing' }
CypLoadedThing >> propertyAt: propertyName [

	^properties at: propertyName ifAbsent: [AbsentToken]
]

{ #category : 'accessing' }
CypLoadedThing >> propertyAt: propertyName put: aValue [
	"Value should be a string"

	self markPackageDirty.
	^properties at: propertyName put: aValue
]

{ #category : 'accessing' }
CypLoadedThing >> removeProperty: propertyName [

	self markPackageDirty.
	^properties removeKey: propertyName ifAbsent: [AbsentToken]
]

{ #category : 'accessing' }
CypLoadedThing >> setPropertiesTo: aDictionary [

	properties := aDictionary copy
]
