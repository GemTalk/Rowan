"
No class-specific documentation for CypGsPackageAdditionPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsPackageAdditionPatch( packageDefinition)

"
Class {
	#name : 'RwGsPackageAdditionSymbolDictPatch',
	#superclass : 'RwGsPackagePatch',
	#category : 'Rowan-GemStone'
}

{ #category : 'applying' }
RwGsPackageAdditionSymbolDictPatch >> createLoadedPackage [

	| loadedPackage loadedProject |
	loadedPackage := self packageSymbolDictionary
		createLoadedPackageFromDefinition: self packageDefinition.

	projectDefinition ifNil: [ ^ self ].

	loadedProject := Rowan
		loadedProjectNamed: projectDefinition name
		ifAbsent: [ 
			self
				error:
					'expected a loaded project named ' , projectDefinition name printString ].

	loadedProject addLoadedPackage: loadedPackage
]
