"
Represents modifications to a single package.

Instvars:

propertiesModification			PropertiesModification -- changes to package properties
classesModification				ClassesModification -- changes to classes defined
classExtensionsModification		ClassesModificaiton -- changes to classes extended
"
Class {
	#name : 'CypPackageModification',
	#superclass : 'CypModification',
	#instVars : [
		'classesModification',
		'classExtensionsModification'
	],
	#category : 'Cypress-Core'
}

{ #category : 'visiting' }
CypPackageModification >> acceptVisitor: aVisitor [

	^aVisitor visitPackageModification: self
]

{ #category : 'Accessing' }
CypPackageModification >> classesModification [
	^classesModification

]

{ #category : 'Updating' }
CypPackageModification >> classesModification: newValue [
	classesModification := newValue

]

{ #category : 'Accessing' }
CypPackageModification >> classExtensionsModification [
	^classExtensionsModification

]

{ #category : 'Updating' }
CypPackageModification >> classExtensionsModification: newValue [
	classExtensionsModification := newValue

]

{ #category : 'initialization' }
CypPackageModification >> initialize [
	"Dummy initialization that understands isEmpty for printing."

	super initialize.
	classesModification := #().
	classExtensionsModification := #()
]

{ #category : 'testing' }
CypPackageModification >> isEmpty [

	^super isEmpty & classesModification isEmpty
		& classExtensionsModification isEmpty
]
