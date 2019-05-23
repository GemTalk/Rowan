accessing
removeProperty: propertyName

	self markPackageDirty.
	^properties removeKey: propertyName ifAbsent: [AbsentToken]