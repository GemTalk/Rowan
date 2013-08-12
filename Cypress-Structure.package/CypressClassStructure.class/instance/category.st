accessing
category

	^self properties
		at: 'category'
		ifAbsent: [self packageStructure packageName]
