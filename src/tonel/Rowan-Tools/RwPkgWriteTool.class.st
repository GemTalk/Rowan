Class {
	#name : 'RwPkgWriteTool',
	#superclass : 'RwPackageTool',
	#category : 'Rowan-Tools'
}

{ #category : 'cypress api' }
RwPkgWriteTool >> writeSpecification: aRwSpecification packageNames: packageNames [

	"CYPRESS API: this messge unconditionally uses #Category to interpret package membership --- useful for saving Rowan code during development ONLY"

	| repo |
	self specification: aRwSpecification.
	repo := CypressAbstractRepository
		onUrl: (CypressUrl absoluteFromText: specification repositoryUrl)
		alias: ''.
	packageNames
		do: [ :packageName | 
			| package |
			package := (CypressPackageDefinition named: packageName)
				lookupSymbolList: RwGsImage symbolList;
				yourself.
			repo writePackageStructure: (CypressPackageStructure fromPackage: package) ].
	^ specification
]

{ #category : 'cypress api' }
RwPkgWriteTool >> writeSpecUrl: aString packageNames: packageNames [

	"CYPRESS API: this messge unconditionally uses #Category to interpret package membership --- useful for saving Rowan code during development ONLY"

	^ self
		writeSpecification: (RwSpecification fromUrl: aString)
		packageNames: packageNames
]
