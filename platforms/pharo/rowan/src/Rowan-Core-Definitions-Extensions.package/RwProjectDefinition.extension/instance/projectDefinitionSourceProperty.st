*rowan-core-definitions-extensions
projectDefinitionSourceProperty

	^ properties at: RwLoadedProject _projectDefinitionSourceKey ifAbsent: [ RwLoadedProject _projectUnknownDefinitionSourceValue ]