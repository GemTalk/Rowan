accessing
classesInPackageNamed: aString

	^(System myUserProfile symbolList allSatisfying: 
			[:each |
			each isBehavior and: [each category = aString or: [each category beginsWith: aString, '-']]])
		sortAscending: #('name')