Class {
	#name : 'RwResolvedFromDefinedProject',
	#superclass : 'RwResolvedProject',
	#category : 'Rowan-Core'
}

{ #category : 'accessing' }
RwResolvedFromDefinedProject >> componentsPath: aString [
	self _projectSpecification componentsPath: aString
]

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
RwResolvedFromDefinedProject >> loadProjectSet [
	"
		refresh the contents of the receiver from disk and create a project set that includes project definitions of
			required projects, also read from disk. Then load the entire project set.
	"

	^ self _concreteProject loadProjectSet
]

{ #category : 'accessing' }
RwResolvedFromDefinedProject >> packageNamed: aString [
	^ self _concreteProject packageNamed: aString
]

{ #category : 'accessing' }
RwResolvedFromDefinedProject >> packagesPath: aString [
	self _projectSpecification packagesPath: aString
]

{ #category : 'accessing' }
RwResolvedFromDefinedProject >> projectsPath: aString [
	self _projectSpecification projectsPath: aString
]

{ #category : 'accessing' }
RwResolvedFromDefinedProject >> projectSpecFile: relativePathString [
	^ self _concreteProject projectSpecFile: relativePathString
]

{ #category : 'accessing' }
RwResolvedFromDefinedProject >> projectSpecPath: aStringOrNil [
	"nil value indicates projectSpec file is in repository root directory"

	self _projectSpecification projectSpecPath: aStringOrNil
]

{ #category : 'accessing' }
RwResolvedFromDefinedProject >> specsPath: aString [
	self _projectSpecification specsPath: aString
]
