copying
postCopy

	super postCopy.
	components := components copy.
	components keysAndValuesDo: [:key : value | components at: key put: value copy ] .