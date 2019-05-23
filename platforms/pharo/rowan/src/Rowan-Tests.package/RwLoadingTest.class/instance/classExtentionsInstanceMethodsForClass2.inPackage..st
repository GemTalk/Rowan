support - classExtensions definitions
classExtentionsInstanceMethodsForClass2: className inPackage: packageName

	^ self
		methodDefsFromSpec:
			{{#'foo'.
			'*', packageName.
			'foo ^ ''foo'''}}