support
propertiesForClassExtensionNamed: className

	| dict |
	dict := Dictionary new.
	dict
		at: 'name' put: className;
		yourself.
	^ dict