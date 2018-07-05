Extension { #name : 'RwClassModification' }

{ #category : '*rowan-gemstone-loader-extensions' }
RwClassModification >> addModificationToPatchSet: aPatchSet inPackage: aPackage inProject: aProjectDefinition [

	aPatchSet
		addClassModification: self 
		toPatchSetInPackage: aPackage 
		inProject: aProjectDefinition.

]
