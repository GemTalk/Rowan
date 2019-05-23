definitions
instanceExtensionMethodDefinitionsForClass: className inPackage: packageName

	^ self
		methodDefsFromSpec:
			{{#'testInstanceExtensionMethod1'.
			('*' , packageName).
			'testInstanceExtensionMethod1 ^ instVar1'}}