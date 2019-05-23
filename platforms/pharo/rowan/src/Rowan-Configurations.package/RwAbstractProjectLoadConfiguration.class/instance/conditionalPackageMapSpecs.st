accessing
conditionalPackageMapSpecs

	^ conditionalPackageMapSpecs ifNil: [ conditionalPackageMapSpecs := Dictionary new ]
