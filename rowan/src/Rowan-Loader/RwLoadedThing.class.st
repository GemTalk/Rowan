"
properties: dictionary of properties
name: name of class or package, selector of method
handle: reference to the class or method object, or to the native package object for dialects that have that.
"
Class {
	#name : 'RwLoadedThing',
	#superclass : 'Object',
	#instVars : [
		'properties',
		'name',
		'handle'
	],
	#classVars : [
		'AbsentToken'
	],
	#category : 'Rowan-Loader'
}

{ #category : 'accessing' }
RwLoadedThing class >> absentToken [

	^AbsentToken
]

{ #category : 'class initialization' }
RwLoadedThing class >> initialize [
  "RwLoadedThing initialize"

  AbsentToken := '<absent>'
]

{ #category : 'instance creation' }
RwLoadedThing class >> newNamed: aName [

	^(self new)
		initialize;
		initializeForName: aName;
		yourself
]

{ #category : 'comparing' }
RwLoadedThing >> _compareProperty: propertyKey propertyVaue: propertyValue againstBaseValue: baseValue [

	^ propertyValue = baseValue
]

{ #category : 'accessing' }
RwLoadedThing >> absentToken [

	^AbsentToken
]

{ #category : 'comparing' }
RwLoadedThing >> compareAgainstBase: aDefinition [
	"Diff myself for changes against the given base definition. 
	Answer a Modification, which might be empty if there are no changes."

	self subclassResponsibility
]

{ #category : 'comparing' }
RwLoadedThing >> compareDictionary: myDictionary againstBaseDictionary: baseDictionary into: anElementsModification elementClass: elementClass [

	| keys |
	keys := myDictionary keys . "keys is already a copy"
	keys addAll: baseDictionary keys.
	keys do: 
			[:key |
			| before after modification |
			before := baseDictionary at: key ifAbsent: [].
			before := before
				ifNil: [ elementClass new ]
				ifNotNil: [ :def | 
					"at this point convert loaded thing to a real definition"
					def asDefinition ].
			after := myDictionary at: key ifAbsent: [].
			after := after
				ifNil: [ elementClass new ]
				ifNotNil: [ :def | 
					"at this point convert loaded thing to a real definition"
					def asDefinition ].
			modification := after compareAgainstBase: before.
			modification isEmpty
				ifFalse: [anElementsModification addElementModification: modification]]
]

{ #category : 'comparing' }
RwLoadedThing >> comparePropertiesAgainstBase: aDefinition [

	| keys modification |
	modification := RwPropertiesModification new.
	keys := self propertiesForCompare keys . "properties is a dictionary, keys is a copy"
  aDefinition propertiesForCompare keysDo:[:k | keys add: k ].
	keys do: 
			[:key |
				| before after |
				before := aDefinition propertyForCompareAt: key ifAbsent: [nil].
				after := self propertyForCompareAt: key ifAbsent: [nil].
				(self _compareProperty: key propertyVaue: before againstBaseValue: after)
					ifFalse: 
						[modification addElementModification: (RwPropertyModification
							key: key
							oldValue: before
							newValue: after)]].
	^modification
]

{ #category : 'private' }
RwLoadedThing >> definitionsFor: loadedThings [

	| result |
	result := Dictionary new: loadedThings size.
	loadedThings
		do: [:loadedThing | result at: loadedThing key put: loadedThing asDefinition].
	^result
]

{ #category : 'accessing' }
RwLoadedThing >> handle [
	"Answer a reference to the platform-specific image object for which I hold information."

	^handle
]

{ #category : 'accessing' }
RwLoadedThing >> handle: anObject [
	"Set my reference to the platform-specific image object for which I hold information."

	self markPackageDirty.
	handle := anObject

]

{ #category : 'initialization' }
RwLoadedThing >> initialize [

	properties := SymbolDictionary new
]

{ #category : 'initialization' }
RwLoadedThing >> initializeForName: aName [

	name := aName
]

{ #category : 'comparing' }
RwLoadedThing >> isEmpty [
	"Used during definition compareAgainstBase:"

	^ self propertiesForCompare isEmpty
]

{ #category : 'accessing' }
RwLoadedThing >> loadedPackage [

	^ self subclassResponsibility: #'loadedPackage'
]

{ #category : 'accessing' }
RwLoadedThing >> loadedProject [

	^ self loadedPackage loadedProject
]

{ #category : 'accessing' }
RwLoadedThing >> name [

	^name
]

{ #category : 'accessing' }
RwLoadedThing >> name: aString [

	self markPackageDirty.
	name := aString
]

{ #category : 'accessing' }
RwLoadedThing >> prettyName [

	^self name

]

{ #category : 'printing' }
RwLoadedThing >> printOn: aStream [

	super printOn: aStream.
	name
		ifNotNil: 
			[aStream
				space;
				nextPutAll: name]
]

{ #category : 'accessing' }
RwLoadedThing >> properties [

	^properties
]

{ #category : 'comparing' }
RwLoadedThing >> propertiesForCompare [
	^ properties
]

{ #category : 'private' }
RwLoadedThing >> propertiesForDefinition [

	^properties copy
]

{ #category : 'accessing' }
RwLoadedThing >> propertyAt: propertyName [
	^ properties at: propertyName asSymbol ifAbsent: [ AbsentToken ]
]

{ #category : 'accessing' }
RwLoadedThing >> propertyAt: propertyName ifAbsent: aBlock [
	^ properties at: propertyName asSymbol ifAbsent: aBlock
]

{ #category : 'accessing' }
RwLoadedThing >> propertyAt: propertyName put: aValue [
	"Value should be a string"

	self markPackageDirty.
	^properties at: propertyName asSymbol put: aValue
]

{ #category : 'comparing' }
RwLoadedThing >> propertyForCompareAt: propertyName ifAbsent: aBlock [

	^self propertiesForCompare at: propertyName asSymbol ifAbsent: aBlock
]

{ #category : 'accessing' }
RwLoadedThing >> removeProperty: propertyName [

	self markPackageDirty.
	^properties removeKey: propertyName ifAbsent: [AbsentToken]
]

{ #category : 'accessing' }
RwLoadedThing >> setPropertiesTo: aDictionary [
	aDictionary _validateClass: SymbolDictionary.
	properties := aDictionary copy
]
