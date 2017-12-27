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
	#name : 'CypDefinition',
	#superclass : 'Object',
	#instVars : [
		'properties'
	],
	#category : 'Cypress-Core'
}

{ #category : 'instance creation' }
CypDefinition class >> new [

	^(super new)
		initialize;
		yourself
]

{ #category : 'private' }
CypDefinition >> addDefinition: aDefinition to: aDictionary [

	| key |
	key := aDefinition key.
	(aDictionary includesKey: key)
		ifTrue: [self error: 'Duplicate definition.'].
	aDictionary at: key put: aDefinition
]

{ #category : 'comparing' }
CypDefinition >> compareAgainstBase: aDefinition [
	"Diff myself for changes against the given base definition. 
	Answer a Modification, which might be empty if there are no changes."

	self subclassResponsibility
]

{ #category : 'comparing' }
CypDefinition >> compareDictionary: myDictionary againstBaseDictionary: baseDictionary into: anElementsModification elementClass: elementClass [

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
CypDefinition >> comparePropertiesAgainstBase: aDefinition [

	| keys modification |
	modification := CypPropertiesModification new.
	keys := properties keys copy.
	keys addAll: aDefinition properties keys.
	keys do: 
			[:key |
			| before after |
			before := aDefinition propertyAt: key ifAbsent: [nil].
			after := self propertyAt: key ifAbsent: [nil].
			before = after
				ifFalse: 
					[modification addElementModification: (CypPropertyModification
								key: key
								oldValue: before
								newValue: after)]].
	^modification
]

{ #category : 'initialization' }
CypDefinition >> initialize [

	properties := Dictionary new
]

{ #category : 'testing' }
CypDefinition >> isEmpty [
	"Answer true if this definition does not actually define anything.
	All definitions are expected to have at least a name property."

	^properties isEmpty
]

{ #category : 'properties' }
CypDefinition >> key [
	"Answer an object that can be used to uniquely identify myself in the context of my container."

	self subclassResponsibility
]

{ #category : 'printing' }
CypDefinition >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPutAll: ' for ';
		nextPutAll: (self key ifNil: ['nil'])
]

{ #category : 'accessing' }
CypDefinition >> properties [

	^properties
]

{ #category : 'accessing' }
CypDefinition >> properties: propertiesDictionary [

	properties := propertiesDictionary
]

{ #category : 'accessing' }
CypDefinition >> propertyAt: aKey [

	^properties at: aKey
]

{ #category : 'accessing' }
CypDefinition >> propertyAt: aKey ifAbsent: absentBlock [

	^properties at: aKey ifAbsent: absentBlock
]

{ #category : 'properties' }
CypDefinition >> propertyAt: aKey put: aValue [

	^properties at: aKey put: aValue
]
