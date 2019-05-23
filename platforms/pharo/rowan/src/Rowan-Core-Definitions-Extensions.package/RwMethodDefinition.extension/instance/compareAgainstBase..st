*rowan-core-definitions-extensions
compareAgainstBase: aDefinition 

	| modification |
	modification := RwMethodModification before: aDefinition after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition);
		sourceModification: (self compareSourceAgainstBase: aDefinition).
	^modification
