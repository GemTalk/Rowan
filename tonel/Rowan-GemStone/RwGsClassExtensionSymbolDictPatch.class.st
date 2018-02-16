Class {
	#name : 'RwGsClassExtensionSymbolDictPatch',
	#superclass : 'RwGsClassPatch',
	#instVars : [
		'patchClass'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'actions' }
RwGsClassExtensionSymbolDictPatch >> installClassExtensionInSystem [

	"The class must already exist.
        Create or update the LoadedClassExtension for the extended class."

	(self projectDefinition
		useSessionMethodsForExtensionsForPackageNamed: self packageName)
		ifTrue: [ 
			false
				ifTrue: [ self error: 'not yet implemented' ] ]
		ifFalse: [ 
			self packageSymbolDictionary
				ensureExtensionClassNamed: self className
				existsForPackageNamed: self packageName ]
]
