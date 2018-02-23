"
Represents modifications to a single package.

Instvars:

propertiesModification			PropertiesModification -- changes to package properties
classesModification				ClassesModification -- changes to classes defined
classExtensionsModification		ClassesModificaiton -- changes to classes extended
"
Class {
	#name : 'RwPackageModification',
	#superclass : 'RwModification',
	#instVars : [
		'classesModification',
		'classExtensionsModification'
	],
	#category : 'Rowan-Core'
}

{ #category : 'visiting' }
RwPackageModification >> acceptVisitor: aVisitor [

	^aVisitor visitPackageModification: self
]

{ #category : 'patching' }
RwPackageModification >> addModificationToPatchSet: aPatchSet inPackage: aPackage [

	self halt
]

{ #category : 'Accessing' }
RwPackageModification >> classesModification [
	^classesModification

]

{ #category : 'Updating' }
RwPackageModification >> classesModification: newValue [
	classesModification := newValue

]

{ #category : 'Accessing' }
RwPackageModification >> classExtensionsModification [
	^classExtensionsModification

]

{ #category : 'Updating' }
RwPackageModification >> classExtensionsModification: newValue [
	classExtensionsModification := newValue

]

{ #category : 'initialization' }
RwPackageModification >> initialize [
	"Dummy initialization that understands isEmpty for printing."

	super initialize.
	classesModification := #().
	classExtensionsModification := #()
]

{ #category : 'testing' }
RwPackageModification >> isEmpty [

	^super isEmpty & classesModification isEmpty
		& classExtensionsModification isEmpty
]
