definitions
asProjectDefinitionSet

	| result |
	result := RwProjectSetDefinition new.
	entities do: [ :entity | result addDefinition: entity asDefinition ].
	^ result