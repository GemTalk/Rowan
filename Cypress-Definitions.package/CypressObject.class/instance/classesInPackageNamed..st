accessing
classesInPackageNamed: aString

	| packageName candidateName |
	packageName := aString asLowercase.
	^(System myUserProfile symbolList allSatisfying: 
			[:each |
			each isBehavior and: 
					[candidateName := each category asLowercase.
					candidateName = packageName
						or: [candidateName beginsWith: packageName , '-']]])
		sortAscending: #('name')