Extension { #name : 'RwClassModificationForcingNewClassVersion' }

{ #category : '*rowan-gemstone-loader-extensions' }
RwClassModificationForcingNewClassVersion >> addModificationForcingNewClassVersionToPatchSet: aPatchSet inPackage: aPackage inProject: aProjectDefinition [

	aPatchSet
		addForcingNewClassVersionModification: self 
		toPatchSetInPackage: aPackage 
		inProject: aProjectDefinition
]

{ #category : '*rowan-gemstone-loader-extensions' }
RwClassModificationForcingNewClassVersion >> addModificationToPatchSet: aPatchSet inPackage: aPackage inProject: aProjectDefinition [

	self shouldNotImplement: #addModificationToPatchSet:inPackage:inProject:

]
