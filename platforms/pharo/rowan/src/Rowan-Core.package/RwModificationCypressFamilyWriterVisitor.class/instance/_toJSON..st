private
_toJSON: anObject
	^ (STON toJsonStringPretty: anObject)
		withLineEndings: self _newLine