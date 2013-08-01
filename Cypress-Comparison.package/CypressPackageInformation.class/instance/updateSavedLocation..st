updating
updateSavedLocation: aDirectory

	self savedLocation: aDirectory.
	self updateSavedDefinitions: (aDirectory isEmpty
				ifTrue: [#()]
				ifFalse: 
					[(CypressPackageReader readPackageStructureForPackageNamed: self name
						from: aDirectory) packageStructure
						snapshot definitions])