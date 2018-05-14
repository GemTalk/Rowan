"
No class-specific documentation for CypGsPackageAdditionPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsPackageAdditionPatch( packageDefinition)

"
Class {
	#name : 'RwGsPackageAdditionSymbolDictPatch',
	#superclass : 'RwGsPackagePatch',
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'applying' }
RwGsPackageAdditionSymbolDictPatch >> createLoadedPackage [

	| loadedPackage loadedProject |
	loadedPackage := self symbolDictionaryRegistry
		createLoadedPackageFromDefinition: self packageDefinition.

	projectDefinition ifNil: [ ^ self ].

	loadedProject := Rowan image
		loadedProjectNamed: projectDefinition name
		ifAbsent: [ 
			self
				error:
					'expected a loaded project named ' , projectDefinition name printString ].

	loadedProject addLoadedPackage: loadedPackage

]
