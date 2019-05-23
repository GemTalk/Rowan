public
projectNamed: aName ifAbsent: absentBlock

	"Lookup a project with the given name, if found return the project. if not found evaluate the <absentBlock>."

	^ self 
		projectNamed: aName 
			ifPresent: [:loadedProject | loadedProject ]
			ifAbsent: absentBlock