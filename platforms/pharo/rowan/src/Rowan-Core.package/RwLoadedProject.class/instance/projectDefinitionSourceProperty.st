accessing
projectDefinitionSourceProperty

	^ properties at: RwLoadedProject _projectDefinitionSourceKey ifAbsent: [ RwLoadedProject _projectUnknownDefinitionSourceValue ]