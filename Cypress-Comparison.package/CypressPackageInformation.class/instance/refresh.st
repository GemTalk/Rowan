updating
refresh

	self isKnown ifFalse: [^self].
	self updateKnownPackageSavedLocation: self savedLocation
		or: [self error: 'invalid directory path']