public
projectNamed: aName

	"Answer a project with the given name.  If no project with the given name is found, signals error."

	^ self 
		projectNamed: aName 
			ifPresent: [:loadedProject | loadedProject ]
			ifAbsent: [ self error: 'The project ', aName printString, ' was not found' ]