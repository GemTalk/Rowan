accessing
packageNamed: aString

	^ self
		packageNamed: aString
		ifAbsent: [ self error: 'A package named ' , aString printString , ' was not found.' ]