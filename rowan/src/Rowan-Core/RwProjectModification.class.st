"
Represents modifications to a single package.

Instvars:

propertiesModification			PropertiesModification -- changes to package properties
classesModification				ClassesModification -- changes to classes defined
classExtensionsModification		ClassesModificaiton -- changes to classes extended
"
Class {
	#name : 'RwProjectModification',
	#superclass : 'RwModification',
	#instVars : [
		'packagesModification'
	],
	#category : 'Rowan-Core'
}

{ #category : 'visiting' }
RwProjectModification >> acceptVisitor: aVisitor [

	^ aVisitor visitProjectModification: self
]

{ #category : 'initialization' }
RwProjectModification >> initialize [

	"Dummy initialization that understands isEmpty for printing."

	super initialize.
	packagesModification := #()
]

{ #category : 'testing' }
RwProjectModification >> isEmpty [

	^ super isEmpty & packagesModification isEmpty
]

{ #category : 'accessing' }
RwProjectModification >> packagesModification [

   ^packagesModification

]

{ #category : 'accessing' }
RwProjectModification >> packagesModification: anObject [

   packagesModification := anObject

]
