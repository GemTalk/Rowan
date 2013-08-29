private
addMethodNamed: selector body: bodyString toClass: aClass inCategory: aString

	| newMethod |
	(aClass includesCategory: aString)
		ifFalse: [self categoryToCleanUp: aString from: aClass].
	newMethod := aClass
				compileMethod: selector , '   ' , bodyString
				dictionaries: System myUserProfile symbolList
				category: aString
				environmentId: 0.
	self methodToCleanUp: newMethod.
	^newMethod