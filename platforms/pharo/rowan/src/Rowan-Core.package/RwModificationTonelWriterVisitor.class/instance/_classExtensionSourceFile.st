class extension writing
_classExtensionSourceFile

	| filename |
	filename := classExtFileNameMap 
		at:  self currentClassExtension name
		ifAbsent: [
			"in the case of class removal, need to use the before class def map"
			classExtBeforeFileNameMap at: self currentClassExtension name ].
	^ self _packageSourceDir / (filename, '.extension.st')