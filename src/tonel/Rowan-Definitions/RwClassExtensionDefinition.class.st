"
Extends an existing class with additional methods.
Properties must include 'name', the name of the class to be extended, but should not include other things like 'super' or 'instvars'.

"
Class {
	#name : 'RwClassExtensionDefinition',
	#superclass : 'RwAbstractClassDefinition',
	#category : 'Rowan-Definitions'
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
RwClassExtensionDefinition >> _methodsModificationClass [

	^ RwExtensionMethodsModification
]

{ #category : 'private' }
RwClassExtensionDefinition >> _modificationClass [

	^ RwClassExtensionModification
]

{ #category : 'comparing' }
RwClassExtensionDefinition >> compareDictionary: myDictionary againstBaseDictionary: baseDictionary into: anElementsModification elementClass: elementClass isMeta: isClassMeta [

	| keys |
	keys := myDictionary keys copy.
	keys addAll: baseDictionary keys.
	keys
		do: [ :key | 
			| before after modification |
			before := baseDictionary at: key ifAbsent: [ elementClass new ].
			after := myDictionary at: key ifAbsent: [ elementClass new ].
			modification := after compareExtensionMethodsAgainstBase: before.
			modification
				isMeta: isClassMeta;
				classDefinition: self.
			modification isEmpty
				ifFalse: [ anElementsModification addElementModification: modification ] ]
]
