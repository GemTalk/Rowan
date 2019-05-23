accessing
committish: aString committishType: committishTypeString

	self properties at: 'committish' put: aString.
	self properties at: 'committishType' put: committishTypeString.