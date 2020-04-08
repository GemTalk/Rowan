Extension { #name : 'RwClassModification' }

{ #category : '*rowan-gemstone-loader-extensions-onlyv2' }
RwClassModification >> addModificationToPatchSet: aPatchSet inPackage: aPackage inProject: aProjectDefinition [

	aPatchSet
		addClassModification: self 
		toPatchSetInPackage: aPackage 
		inProject: aProjectDefinition.

]
