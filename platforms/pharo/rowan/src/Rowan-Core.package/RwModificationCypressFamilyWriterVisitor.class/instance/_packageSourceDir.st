package writing
_packageSourceDir

	| filename |
	filename := packageDefFileNameMap 
		at:  self currentPackageDefinition name 
		ifAbsent: [
			"in the case of package removal, need to use the before package map"
			packageDefBeforeFileNameMap at: self currentPackageDefinition name ].
	^ self packageExtension isEmpty
		ifTrue: [ self _projectSourceDir /filename ]
		ifFalse: [ self _projectSourceDir /filename, self packageExtension ]