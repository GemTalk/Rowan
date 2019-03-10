Extension { #name : 'RwProjectDefinition' }

{ #category : '*rowan-definitions-deprecated' }
RwProjectDefinition >> packageNamesInLoadOrder: packageNamesInLoadOrder [

	self deprecated: 'RwProjectDefinition >> packageNamesInLoadOrder: deprecated in Rowan 1.2.6+'.
	packageNamesInLoadOrder
		do: [ :packageName | self addPackage: (RwPackageDefinition newNamed: packageName) ]
]

{ #category : '*rowan-definitions-deprecated' }
RwProjectDefinition class >> newForGitPackageProjectNamed: aString [

	| spec |
	self deprecated: 'RwProjectDefinition class >> newForGitPackageProjectNamed: deprecated in Rowan 1.2.6+'.
	spec := RwSimpleProjectSpecification new
		specName: aString;
		yourself.
	^ self newForSpecification: spec
]
