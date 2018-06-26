Extension { #name : 'RwClassModificationForcingNewClassVersion' }

{ #category : '*rowan-gemstone-loader-extensions' }
RwClassModificationForcingNewClassVersion >> addModificationForcingNewClassVersionToPatchSet: aPatchSet inPackage: aPackage inProject: aProjectDefinition [

	aPatchSet
		addForcingNewClassVersionModification: self 
		toPatchSetInPackage: aPackage 
		inProject: aProjectDefinition
]
