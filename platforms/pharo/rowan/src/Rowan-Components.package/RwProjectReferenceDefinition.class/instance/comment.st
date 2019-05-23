accessing
comment

	^ self properties at: 'comment' ifAbsent: [ '' ]