accessing
propertyAt: propertyName put: aValue
	"Value should be a string"

	self markPackageDirty.
	^properties at: propertyName put: aValue