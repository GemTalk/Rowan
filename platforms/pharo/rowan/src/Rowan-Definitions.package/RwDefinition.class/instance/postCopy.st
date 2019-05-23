copying
postCopy

	| oldProperties |
	super postCopy.
	oldProperties := properties.
	properties := Dictionary new.
	oldProperties keysAndValuesDo: [:key :value| properties at: key put: value copy ]