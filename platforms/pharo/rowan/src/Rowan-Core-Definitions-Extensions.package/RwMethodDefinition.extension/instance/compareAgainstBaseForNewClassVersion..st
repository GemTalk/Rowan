*rowan-core-definitions-extensions
compareAgainstBaseForNewClassVersion: aDefinition

	| modification |
	modification := RwMethodModificationForNewClassVersion
		before: aDefinition
		after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition);
		sourceModification:
				(self compareSourceAgainstBaseForNewClassVersion: aDefinition).
	^ modification