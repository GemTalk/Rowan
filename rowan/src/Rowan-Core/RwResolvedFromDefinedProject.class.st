Class {
	#name : 'RwResolvedFromDefinedProject',
	#superclass : 'RwResolvedProject',
	#category : 'Rowan-Core'
}

{ #category : 'transitions' }
RwResolvedFromDefinedProject >> load [
	^ self
		error:
			'Projects resoved from a defined project are not guaranteed to produce the same loaded state as loaded from disk (via a load spec). Use loadAsDefined to force the project to be loaded AS DEFINED or loadFromSpec to load from disk and ignored the defined project state'
]

{ #category : 'transitions' }
RwResolvedFromDefinedProject >> loadAsDefined [
	"
		The project is loaded exactly as defined, i.e., the packages present in the defined project will be
			loaded into the image exactly as is, without applying any of the conditions defined in the 
			project's components and without regard to the disk-based definitions. If a project is 'loaded 
			as defined', a subsequent reload of the loaded project will reload the disk-based definitions 
			(components and packages).
	"

	^ self _concreteProject load
]

{ #category : 'transitions' }
RwResolvedFromDefinedProject >> loadFromSpec [
	"
		The project is loaded as defined on disk and the in-memory project definition is ignored.
	"

	^ self _concreteProject _loadSpecification resolve load
]
