copying
postCopy

	| oldDefs |
	super postCopy.
	oldDefs := classDefinitions.
	classDefinitions := Dictionary new.
	oldDefs keysAndValuesDo: [:key :value | classDefinitions at: key put: value copy ].
	oldDefs := classExtensions.
	classExtensions := Dictionary new.
	oldDefs keysAndValuesDo: [:key :value | classExtensions at: key put: value copy ].