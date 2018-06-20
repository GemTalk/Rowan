Extension { #name : 'RwProjectDefinition' }

{ #category : '*rowan-definitions-deprecated' }
RwProjectDefinition class >> newForGitPackageProjectNamed: aString [

	| spec |
	spec := RwSimpleProjectSpecification new
		specName: aString;
		yourself.
	^ self newForSpecification: spec

]

{ #category : '*rowan-definitions-deprecated' }
RwProjectDefinition >> packageNamesInLoadOrder: packageNamesInLoadOrder [

	packageNamesInLoadOrder
		do: [ :packageName | self addPackage: (RwPackageDefinition newNamed: packageName) ]
]

