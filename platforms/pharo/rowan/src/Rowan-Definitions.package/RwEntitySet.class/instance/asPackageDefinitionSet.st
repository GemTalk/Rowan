definitions
asPackageDefinitionSet

	| result |
	result := RwPackageSetDefinition new.
	entities do: [ :entity | result addDefinition: entity asDefinition ].
	^ result