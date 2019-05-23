accessing
conditionalPackages

	^ conditionalPackages ifNil: [ conditionalPackages := Dictionary new ]
