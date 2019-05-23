private
_basicClassDefinition: className superclass: superclass type: type

	^ self
		_basicClassDefinition: className
		superclass: superclass
		gs_options: nil
		type: type