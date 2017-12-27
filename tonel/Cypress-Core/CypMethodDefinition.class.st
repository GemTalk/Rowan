"
Defines a method.
Properties includes 'selector'.
source is a string that is the source code of the method defined. 
In a well-formed MethodDefinition, the selector property matches
the selector parsed from the source code.

"
Class {
	#name : 'CypMethodDefinition',
	#superclass : 'CypDefinition',
	#instVars : [
		'source'
	],
	#category : 'Cypress-Core'
}

{ #category : 'instance creation' }
CypMethodDefinition class >> withProperties: propertiesDictionary source: sourceString [

	^(self basicNew)
		properties: propertiesDictionary;
		source: sourceString;
		yourself
]

{ #category : 'comparing' }
CypMethodDefinition >> compareAgainstBase: aDefinition [

	| modification |
	modification := CypMethodModification before: aDefinition after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition);
		oldSource: aDefinition source;
		newSource: self source.
	^modification
]

{ #category : 'testing' }
CypMethodDefinition >> isEmpty [
	"Answer true if this definition does not actually define anything."

	^super isEmpty & source isNil
]

{ #category : 'private' }
CypMethodDefinition >> key [

	^properties at: 'selector' ifAbsent: [nil]
]

{ #category : 'accessing' }
CypMethodDefinition >> source [

	^source
]

{ #category : 'accessing' }
CypMethodDefinition >> source: sourceString [

	source := sourceString
]
