accessing
projectAlias: aStringOrNil

	aStringOrNil ifNil: [ ^ self properties removeKey: 'projectAlias' ifAbsent: [] ].
	^ self properties at: 'projectAlias' put: aStringOrNil