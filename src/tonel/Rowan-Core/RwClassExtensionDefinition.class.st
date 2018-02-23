"
Extends an existing class with additional methods.
Properties must include 'name', the name of the class to be extended, but should not include other things like 'super' or 'instvars'.

"
Class {
	#name : 'RwClassExtensionDefinition',
	#superclass : 'RwAbstractClassDefinition',
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwClassExtensionDefinition class >> newForClassNamed: className [

	| propertiesDictionary |
	propertiesDictionary := Dictionary new
		at: 'name' put: className;
		yourself.
	^ self
		withProperties: propertiesDictionary
		classMethodDefinitions: Dictionary new
		instanceMethodDefinitions: Dictionary new
]

{ #category : 'private' }
RwClassExtensionDefinition >> _modificationClass [

	^ RwClassExtensionModification
]
