accessing
packageExtension

	^self packageStructure
		packageExtensionOr: [self repository packageExtension]