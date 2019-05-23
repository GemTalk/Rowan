*rowan-core-definitions-extensions
_compareProperty: propertyKey propertyVaue: propertyValue againstBaseValue: baseValue

	({ 'spec'. RwLoadedProject _projectDefinitionSourceKey. 'projectRef' } includes: propertyKey)
		ifTrue: [ 
		"projectRef entries are considered to be equal for comparison purposes"
		"spec entries are considered to be equal for comparison purposes"
		"_projectDefinitionSourceKey entries are considered equal for comparison purpposes"
		^ true ].
	^ super _compareProperty: propertyKey propertyVaue: propertyValue againstBaseValue: baseValue