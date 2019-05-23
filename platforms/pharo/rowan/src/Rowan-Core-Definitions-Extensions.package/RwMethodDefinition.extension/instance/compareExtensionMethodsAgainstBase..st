*rowan-core-definitions-extensions
compareExtensionMethodsAgainstBase: aDefinition

	| modification |
	modification := RwExtensionMethodModification before: aDefinition after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition);
		sourceModification: (self compareSourceAgainstBase: aDefinition).
	^ modification