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

	((projectLoadSpecification platformSpec at: 'gemstone')
		useSessionMethodsForExtensionsForPackageNamed: self packageName)
		ifTrue: [ self error: 'not yet implemented' ]
		ifFalse: [ 
			self packageSymbolDictionary
				ensureExtensionClassNamed: self className
				existsForPackageNamed: self packageName ]
]
