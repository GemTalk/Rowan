Class {
	#name : 'RwGsClassExtensionPatch',
	#superclass : 'RwGsClassPatch',
	#instVars : [
		'patchClass'
	],
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'actions' }
RwGsClassExtensionPatch >> createOrUpdateLoadedExtensionClass [
	"ensure that a loaded class extension exists for this class"

	RwGsLoadedClassExtension existingOrNewForClass: patchClass inPackageNamed: self packageName

]

{ #category : 'actions' }
RwGsClassExtensionPatch >> ensureClassExists [


	| className assoc |
	className := classDefinition name.
	assoc := self resolveName: className .
	assoc 
		ifNil: [ self error: 'internal error - missing class named ', className printString, ' for class extension patch' ].
	patchClass := assoc value
]

{ #category : 'actions' }
RwGsClassExtensionPatch >> installClassExtensionInSystem [

	"The class must already exist.
        Create or update the LoadedClassExtension for the extended class."

	self
		ensureClassExists
		createOrUpdateLoadedExtensionClass
]

{ #category : 'accessing' }
RwGsClassExtensionPatch >> patchClass [

	^ patchClass
]
