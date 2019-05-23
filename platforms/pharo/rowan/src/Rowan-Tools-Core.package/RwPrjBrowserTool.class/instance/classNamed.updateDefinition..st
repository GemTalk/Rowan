definition updating
classNamed: className updateDefinition: updateBlock

	^ self
		classNamed: className
		updateDefinition: updateBlock
		ifAbsent: [ 
			self
				error:
					'expected class definition for class ' , className printString , ' not found.' ]