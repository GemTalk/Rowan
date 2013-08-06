"
notice: This work is protected by copyright. All rights reserved.
category: accessing
"
classesInPackageNamed: aString

	^(System myUserProfile symbolList allSatisfying: 
			[:each |
			each isBehavior and: [each category = aString or: [each category beginsWith: aString, '-']]])
		sortAscending: #('name')