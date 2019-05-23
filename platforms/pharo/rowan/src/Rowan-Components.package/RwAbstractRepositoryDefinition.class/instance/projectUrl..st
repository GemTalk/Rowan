accessing
projectUrl: anUrlString

	anUrlString ifNil: [ ^ self properties removeKey: 'projectUrl' ifAbsent: [ ] ].
	self properties at: 'projectUrl' put: anUrlString