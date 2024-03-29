Class {
	#name : 'RwMethodDefinition',
	#superclass : 'RwDefinition',
	#instVars : [
		'source'
	],
	#category : 'Rowan-Definitions'
}

{ #category : 'instance creation' }
RwMethodDefinition class >> newForSelector: selector protocol: protocol source: source [

	^ self
		withProperties:
			(Dictionary with: 'selector' -> selector with: 'protocol' -> protocol asString)
		source: source
]

{ #category : 'instance creation' }
RwMethodDefinition class >> newForSource: methodSource protocol: protocol [

	| selector |
	selector := Rowan platform parseSelectorFrom: methodSource.
	^ self
		newForSelector: selector asSymbol
		protocol: protocol
		source: methodSource
]

{ #category : 'instance creation' }
RwMethodDefinition class >> withProperties: propertiesDictionary source: sourceString [ 

	^(self basicNew)
		properties: propertiesDictionary;
		source: sourceString;
		yourself

]

{ #category : 'private' }
RwMethodDefinition >> _compareProperty: propertyKey propertyVaue: propertyValue againstBaseValue: baseValue [

	({'_gsFileOffset'. '_gsFileName' } includes: propertyKey)
		ifTrue: [ 
		"_gsFileOffset' '_gsFileName' are informational properties so any differences can be ignored "
		^ true ].
	^ super _compareProperty: propertyKey propertyVaue: propertyValue againstBaseValue: baseValue
]

{ #category : 'testing' }
RwMethodDefinition >> isEmpty [
	"Answer true if this definition does not actually define anything."

	^super isEmpty & source isNil

]

{ #category : 'private' }
RwMethodDefinition >> key [

	^properties at: 'selector' ifAbsent: [nil]

]

{ #category : 'accessing' }
RwMethodDefinition >> offset: anInteger inFile: aFileName [
  "these properties are used when handling a CompileError or
   a tonel parse error."
  properties at:'_gsFileOffset' put: anInteger .
  properties at:'_gsFileName' put: aFileName .
]

{ #category : 'accessing' }
RwMethodDefinition >> protocol [

	^properties at: 'protocol'

]

{ #category : 'accessing' }
RwMethodDefinition >> protocol: aString [

	properties at: 'protocol' put: aString

]

{ #category : 'accessing' }
RwMethodDefinition >> selector [

	^properties at: 'selector'
]

{ #category : 'accessing' }
RwMethodDefinition >> source [

	^source

]

{ #category : 'accessing' }
RwMethodDefinition >> source: sourceString [

	source := sourceString
]
