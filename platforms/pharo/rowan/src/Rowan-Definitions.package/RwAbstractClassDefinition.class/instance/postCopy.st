copying
postCopy

	| oldDefs |
	super postCopy.
	oldDefs := classMethodDefinitions.
	classMethodDefinitions := Dictionary new.
	oldDefs keysAndValuesDo: [:key :value | classMethodDefinitions at: key put: value copy ].
	oldDefs := instanceMethodDefinitions.
	instanceMethodDefinitions := Dictionary new.
	oldDefs keysAndValuesDo: [:key :value | instanceMethodDefinitions at: key put: value copy ].