"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
classesInPackageNamed: aString

	| packageName classes |
	packageName := aString asLowercase.
	classes := Array new.
	System myUserProfile symbolList do: 
			[:dict |
			classes
				addAll: (dict select: 
							[:each |
							each isBehavior and: 
									[| candidateName |
									candidateName := each category asLowercase.
									candidateName = packageName
										or: [(candidateName indexOfSubCollection: packageName , '-') = 1]]])].
	^classes sortAscending: #('name')
