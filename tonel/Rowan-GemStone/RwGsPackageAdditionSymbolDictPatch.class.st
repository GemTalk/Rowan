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

	| loadedPackage |
	loadedPackage := self packageSymbolDictionary
		createLoadedPackageFromDefinition: self packageDefinition.	"Right here we need to register the loaded package with the loaded project ... what that really means is that we need to have a createLoadedProject message sent to a RwGBwProjectAdditionSymbolDictPatch, etc. which means that we need to wrap a RwPackageSetDefinition with a RwProjectDefinition ... ACTUALLY ... the RwPackageSetDefinition should be renamed/replaced with RwProjectDefinition that has ref to the spec and set of packages involved in the operation ... compare will identify new packages, etc. ... "	"then we will be able to manage the loaded projects"

	self halt
]
