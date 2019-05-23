testing
existsOnDisk

	^ Rowan image 
		loadedProjectNamed: self name 
			ifPresent: [:loadedProject |
				self repositoryRoot
					ifNil: [ false ]
					ifNotNil: [:fileRef | fileRef exists ] ]
			ifAbsent: [ false ]