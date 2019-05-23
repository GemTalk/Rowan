accessing
superclassName

	^self propertyAt: 'superclass' ifAbsent: [self propertyAt: 'super']