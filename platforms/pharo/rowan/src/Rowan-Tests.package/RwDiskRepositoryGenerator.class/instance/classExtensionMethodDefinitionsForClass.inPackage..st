definitions
classExtensionMethodDefinitionsForClass: className inPackage: packageName

	^ self
		methodDefsFromSpec:
			{{#'testClassExtensionMethod1'.
			('*' , packageName).
			'testClassExtensionMethod1 ^ ClassinstVar2'}}