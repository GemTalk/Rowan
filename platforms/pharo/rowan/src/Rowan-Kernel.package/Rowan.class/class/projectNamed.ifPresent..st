public
projectNamed: aName ifPresent: presentBlock

	"Lookup a project with the given name, if found evaluate the <presentBlock>. if not found return nil."

	^ self 
		projectNamed: aName 
			ifPresent: presentBlock
			ifAbsent: []