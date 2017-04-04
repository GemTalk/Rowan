"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: patch access
"
superclassNamed: aName

	^createdClasses at: aName
		ifAbsent: 
			[tempSymbols at: aName asSymbol
				ifAbsent: [self error: 'Superclass not found.']]