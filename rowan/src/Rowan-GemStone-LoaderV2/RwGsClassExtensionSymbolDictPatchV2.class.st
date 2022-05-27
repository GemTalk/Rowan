Class {
	#name : 'RwGsClassExtensionSymbolDictPatchV2',
	#superclass : 'RwGsClassPatchV2',
	#instVars : [
		'patchClass'
	],
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'actions' }
RwGsClassExtensionSymbolDictPatchV2 >> installClassExtensionInSystem: aSymbolList [
	"The class must already exist.
        Create or update the LoadedClassExtension for the extended class."

	(self projectDefinition
		gemstoneUseSessionMethodsForExtensionsForPackageNamed: self packageName)
		ifTrue: [ 
			| resolved |
			Rowan image ensureSessionMethodsEnabled.
			((resolved := aSymbolList objectNamed: self className asSymbol)
				notNil and: [ resolved isBehavior and: [ resolved isMeta not ] ])
				ifFalse: [ 
					self
						error:
							'The extension class named ' , self className printString , ' cannot be found.' ] ]
		ifFalse: [ 
			self symbolDictionaryRegistry
				ensureExtensionClassNamed: self className
				existsForPackageNamed: self packageName
				implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2 ]
]
