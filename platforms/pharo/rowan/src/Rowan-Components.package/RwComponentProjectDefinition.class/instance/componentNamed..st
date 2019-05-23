accessing
componentNamed: aComponentName

	^ self componentNamed: aComponentName ifAbsent: [ self error: 'The component named ', aComponentName printString, ' was not found' ]