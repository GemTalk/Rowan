copying
postCopy

	| oldPackages |
	super postCopy.
	oldPackages := packages.
	packages := Dictionary new.
	oldPackages keysAndValuesDo: [:key : value | packages at: key put: value copy ] .