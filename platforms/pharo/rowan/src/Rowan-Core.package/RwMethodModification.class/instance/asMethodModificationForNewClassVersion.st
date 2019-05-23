converting
asMethodModificationForNewClassVersion

	^ (RwMethodModificationForNewClassVersion before: before after: after)
		isMeta: self isMeta;
		classDefinition: self classDefinition;
		propertiesModification: self propertiesModification;
		sourceModification: self sourceModification;
		yourself
