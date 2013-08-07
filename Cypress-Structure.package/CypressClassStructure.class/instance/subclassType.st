accessing
subclassType

	^self properties at: '_gs_subclassType' ifAbsent: ['']
