"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
classesInPackageNamed: aString

	| packageName |
	packageName := aString asLowercase.
	^(System myUserProfile symbolList allSatisfying: 
			[:each |
			each isBehavior and: [each category asLowercase = packageName]])
		sortAscending: #('name')