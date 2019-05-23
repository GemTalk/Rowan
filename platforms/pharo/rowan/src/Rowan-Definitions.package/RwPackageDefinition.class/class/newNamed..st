instance creation
newNamed: packageName

	^ self
		withProperties: (Dictionary with: 'name' -> packageName)
		classDefinitions: Dictionary new
		classExtensions: Dictionary new