accessing
classType
	"Legal types are normal, variable, and bytes."

	^self propertyAt: 'type' ifAbsent: ['normal']