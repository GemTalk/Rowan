accessing
addAll: aRwDefinitionOrRwLoadedThingCollection

	aRwDefinitionOrRwLoadedThingCollection
		do: [ :entity | entities at: entity name put: entity ].
	^ aRwDefinitionOrRwLoadedThingCollection