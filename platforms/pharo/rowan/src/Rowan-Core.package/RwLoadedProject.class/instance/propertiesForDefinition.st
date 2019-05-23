definitions
propertiesForDefinition

	| props |
	props := super propertiesForDefinition.
	props at: 'name' put: name.
	props at: self class _projectDefinitionSourceKey put: self class _projectLoadedDefinitionSourceValue.
	^ props