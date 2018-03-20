"
Abstract.
Defines some ""code"" entity.
The definition object's primary responsibility is to hold information.
It holds exactly the same information as the disk representation in some form of Cypress repository.
Other non-definition classes construct and use the information in definitions to read, write, and compare
code.

properties is a dictionary. Keys and values should be strings. These correspond to the properties in a Cypress repository, and are used in various ways, some of them dialect-specific.
"
Class {
	#name : 'RwDefinition',
	#superclass : 'Object',
	#instVars : [
		'properties'
	],
	#category : 'Rowan-Definitions'
}

{ #category : 'instance creation' }
RwDefinition class >> new [

	^(super new)
		initialize;
		yourself
]

{ #category : 'private' }
RwDefinition >> addDefinition: aDefinition to: aDictionary [

	| key |
	key := aDefinition key.
	(aDictionary includesKey: key)
		ifTrue: [self error: 'Duplicate definition.'].
	aDictionary at: key put: aDefinition
]

{ #category : 'converting' }
RwDefinition >> asDefinition [
  ^ self
]

{ #category : 'comparing' }
RwDefinition >> compareAgainstBase: aDefinition [
	"Diff myself for changes against the given base definition. 
	Answer a Modification, which might be empty if there are no changes."

	self subclassResponsibility
]

{ #category : 'comparing' }
RwDefinition >> compareAgainstBaseForNewClassVersion: aBaseDefinition [

	"Diff myself for changes against the given base definition. 
	Answer a Modification, which might be empty if there are no changes."

	"Filter the definition to include changes that are only applicable to new class versions"

	self subclassResponsibility
]

{ #category : 'comparing' }
RwDefinition >> compareDictionary: myDictionary againstBaseDictionary: baseDictionary into: anElementsModification elementClass: elementClass [

	| keys |
	keys := myDictionary keys copy.
	keys addAll: baseDictionary keys.
	keys do: 
			[:key |
			| before after modification |
			before := baseDictionary at: key ifAbsent: [elementClass new].
			after := myDictionary at: key ifAbsent: [elementClass new].
			modification := after compareAgainstBase: before.
			modification isEmpty
				ifFalse: [anElementsModification addElementModification: modification]]
]

{ #category : 'comparing' }
RwDefinition >> comparePropertiesAgainstBase: aDefinition [

	| keys modification |
	modification := RwPropertiesModification new.
	keys := properties keys copy.
	keys addAll: aDefinition properties keys.
	keys do: 
			[:key |
			| before after |
			before := aDefinition propertyAt: key ifAbsent: [nil].
			after := self propertyAt: key ifAbsent: [nil].
			before = after
				ifFalse: 
					[modification addElementModification: (RwPropertyModification
								key: key
								oldValue: before
								newValue: after)]].
	^modification
]

{ #category : 'initialization' }
RwDefinition >> initialize [

	properties := Dictionary new
]

{ #category : 'testing' }
RwDefinition >> isEmpty [
	"Answer true if this definition does not actually define anything.
	All definitions are expected to have at least a name property."

	^properties isEmpty
]

{ #category : 'properties' }
RwDefinition >> key [
	"Answer an object that can be used to uniquely identify myself in the context of my container."

	self subclassResponsibility
]

{ #category : 'printing' }
RwDefinition >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPutAll: ' for ';
		nextPutAll: (self key ifNil: ['nil'])
]

{ #category : 'accessing' }
RwDefinition >> properties [

	^properties
]

{ #category : 'accessing' }
RwDefinition >> properties: propertiesDictionary [

	properties := propertiesDictionary
]

{ #category : 'accessing' }
RwDefinition >> propertyAt: aKey [

	^properties at: aKey
]

{ #category : 'accessing' }
RwDefinition >> propertyAt: aKey ifAbsent: absentBlock [

	^properties at: aKey ifAbsent: absentBlock
]

{ #category : 'properties' }
RwDefinition >> propertyAt: aKey put: aValue [

	^properties at: aKey put: aValue
]

{ #category : 'private' }
RwDefinition >> removeDefinition: aDefinition from: aDictionary [

	| key |
	key := aDefinition key.
	aDictionary
		removeKey: key
		ifAbsent: [ self error: 'no definition for ' , key printString , ' found' ]
]

{ #category : 'private' }
RwDefinition >> removeKey: key from: aDictionary [

	aDictionary
		removeKey: key
		ifAbsent: [ self error: 'no definition for ' , key printString , ' found' ]
]

{ #category : 'private' }
RwDefinition >> updateDefinition: aDefinition in: aDictionary [

	| key |
	key := aDefinition key.
	aDictionary at: key put: aDefinition
]
