private
_loadedClassExtensionsNamed: className

	^ self
		_loadedClassExtensionsNamed: className
		ifAbsent: [ 
			self
				error:
					'No extensions for the class ' , className printString , ' were found' ]