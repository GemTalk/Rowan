accessing
packageExtension

	^ packageExtension 
		ifNil: [ 
			| ext |
			ext := (self _filetreeRepositoryPropertyDictFor: currentProjectReferenceDefinition packagesRoot) at: 'packageExtension'.
			packageExtension := ext copyFrom: 2 to: ext size ]