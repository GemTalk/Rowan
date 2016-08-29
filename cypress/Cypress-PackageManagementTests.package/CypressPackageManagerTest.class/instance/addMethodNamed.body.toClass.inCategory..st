"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
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