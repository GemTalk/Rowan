Class {
	#name : 'RwGsPackageAdditionSymbolDictPatchV2',
	#superclass : 'RwGsPackagePatchV2',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'applying' }
RwGsPackageAdditionSymbolDictPatchV2 >> createLoadedPackage [

	| loadedPackage loadedProject |
	loadedPackage := self symbolDictionaryRegistry
		createLoadedPackageFromDefinition: self packageDefinition
		implementationClass: RwGsSymbolDictionaryRegistry_Implementation.

	projectDefinition ifNil: [ ^ self ].

	loadedProject := Rowan image
		loadedProjectNamed: projectDefinition name
		ifAbsent: [ 
			self
				error:
					'expected a loaded project named ' , projectDefinition name printString ].

	loadedProject addLoadedPackage: loadedPackage

]
