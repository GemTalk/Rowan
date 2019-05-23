private
_loadedClassNamed: className

	^ self
		_loadedClassNamed: className
		ifAbsent: [ self error: 'The class ' , className printString , ' was not found' ]