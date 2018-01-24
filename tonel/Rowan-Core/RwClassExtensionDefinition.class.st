"
Extends an existing class with additional methods.
Properties must include 'name', the name of the class to be extended, but should not include other things like 'super' or 'instvars'.

"
Class {
	#name : 'RwClassExtensionDefinition',
	#superclass : 'RwClassDefinition',
	#category : 'Rowan-Core'
}

{ #category : 'private' }
RwClassExtensionDefinition >> _modificationClass [

	^ RwClassExtensionModification
]
