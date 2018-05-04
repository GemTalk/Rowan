Class {
	#name : 'RwPkgAuditTool',
	#superclass : 'RwPackageTool',
	#category : 'Rowan-Tools'
}

{ #category : 'other' }
RwPkgAuditTool >> auditForPackageName: packageName [

	| loadedPackage  res|
	res := Array new.
	loadedPackage := Rowan image loadedPackageNamed: packageName.
	loadedPackage 
			loadedClassesDo: [:aLoadedClass |  
					(GsSession currentSession resolveSymbol: aLoadedClass name) ifNotNil: [:bhv | |aBehavior | 
						((aBehavior := bhv value) selectors reject: [:e | aLoadedClass includesSelector: e]) do: [:aSelector |
								res add: (GsPackagePolicy current compiledMethodAt: aSelector for: aBehavior value )
						]
					].
				]				
			loadedClassExtenstionsDo: [:e | ] 
			loadedInstanceMethodsDo:  [:e |] 
			loadedClassMethodsDo:  [:e |]. 
	^res
	"loadedPackage loadedClasseExtentions do: [:aLoadedClassKV | | aBehavior aLoadedClass aSelector|
		aBehavior := System myUserProfile resolveSymbol: aLoadedClassKV key.
		aLoadedClass := aLoadedClassKV value.
		aSelector:= aBehavior selectors detect: [:e | aLoadedClass loadedInstanceMethods keys include: e] ifAbsent: [nil]. 
		aSelector ifNotNil: [:aSel | res add: aSel].
	].
	^res
	"	
"	loadedProject := loadedPackage loadedProject.
	super specification: loadedProject specification.
	repo := specification repositoryUrl asRwRepository.
	diskSnapshot := (repo readPackageStructureForPackageNamed: packageName)
		snapshot.
	imageSnapshot := (RwCypressPackageStructure
		fromPackage: loadedPackage asDefinition) snapshot.
	^ CypressPatch fromBase: diskSnapshot toTarget: imageSnapshot
"

]
