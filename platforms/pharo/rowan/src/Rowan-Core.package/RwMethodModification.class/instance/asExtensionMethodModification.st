converting
asExtensionMethodModification

	^ (RwExtensionMethodModification before: before after: after)
		isMeta: self isMeta;
		propertiesModification: propertiesModification;
		sourceModification: sourceModification;
		classDefinition: self classDefinition;
		yourself
