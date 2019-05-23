definitions
asDefinitionSet

	| result |
	result := RwDefinitionSetDefinition new.
	entities do: [ :entity | result addDefinition: entity asDefinition ].
	^ result