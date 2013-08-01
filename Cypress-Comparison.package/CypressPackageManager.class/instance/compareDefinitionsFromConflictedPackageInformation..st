comparing
compareDefinitionsFromConflictedPackageInformation: aCypressPackageInformation

	| badDefinitions expectedDefinitions |
	badDefinitions := (CypressPackageStructure
				fromPackage: (CypressPackageDefinition
						named: aCypressPackageInformation name))
					snapshot definitions
				asSet.
	expectedDefinitions := OrderedCollection new.
	aCypressPackageInformation competingPackageNames do: 
			[:each |
			expectedDefinitions
				addAll: (CypressPackageStructure
						fromPackage: (CypressPackageDefinition named: each)) snapshot
						definitions].
	"Coerce the expected definitions into using the conflicted package name."
	expectedDefinitions do: 
			[:each |
			each classDefinition: [:cd | cd category: aCypressPackageInformation name]
				methodDefinition: [:md | ]].
	expectedDefinitions do: [:each | badDefinitions remove: each ifAbsent: []].
	^badDefinitions sortAscending