accessing
versionString

	self committishType =  'tag'
		ifFalse: [ ^ '' ].
	self committish first = $v
		ifTrue: [ ^self committish copyFrom: 2 to: self committish size ].
	^self committish