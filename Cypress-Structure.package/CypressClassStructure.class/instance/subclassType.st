accessing
subclassType

	^self properties at: 'subclassType' ifAbsent: ['']
