properties
key
	"Answer an object that can be used to uniquely identify myself in the context of my container."

	^self propertyAt: 'name' ifAbsent: [nil]