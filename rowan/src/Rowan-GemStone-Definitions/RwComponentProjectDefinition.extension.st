Extension { #name : 'RwComponentProjectDefinition' }

{ #category : '*rowan-gemstone-definitions' }
RwComponentProjectDefinition >> setUseSessionMethodsForExtensions: aBool forPackageNamed: packageName [

	self projectRef
		setUseSessionMethodsForExtensions: aBool
		forPackageNamed: packageName
]
