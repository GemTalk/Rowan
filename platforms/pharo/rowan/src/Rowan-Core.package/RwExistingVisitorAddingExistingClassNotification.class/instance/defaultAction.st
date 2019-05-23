handling
defaultAction

	loadedProject ifNil: [ ^ self error: 'Visitor adding a class ', classDefinition name printString, ' that already exists' ].
	self error: 
		'Visitor adding a class ', 
		classDefinition name printString, 
		' that already exists in the project ', 
		loadedProject name printString, 
		', but the project was not included in the load'
