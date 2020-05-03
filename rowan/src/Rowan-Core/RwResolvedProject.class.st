Class {
	#name : 'RwResolvedProject',
	#superclass : 'RwAbstractUnloadedProject',
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwResolvedProject class >> fromDefinedProject: aDefinedProject [
	^ (self newNamed: aDefinedProject name)
		_resolvedProject: aDefinedProject _resolvedProject resolve;
		yourself
]

{ #category : 'instance creation' }
RwResolvedProject class >> projectFromUrl: loadSpecUrl [
	| loadSpec resolvedProject |
	loadSpec := RwSpecification fromUrl: loadSpecUrl.
	resolvedProject := loadSpec resolve.
	^ (self newNamed: resolvedProject name)
		_resolvedProject: resolvedProject;
		yourself
]

{ #category : 'instance creation' }
RwResolvedProject class >> projectFromUrl: loadSpecUrl projectsHome: projectsHome [
	| loadSpec resolvedProject |
	loadSpec := (RwSpecification fromUrl: loadSpecUrl)
		projectsHome: projectsHome;
		yourself.
	resolvedProject := loadSpec resolve.
	^ (self newNamed: resolvedProject name)
		_resolvedProject: resolvedProject;
		yourself
]

{ #category : 'instance creation' }
RwResolvedProject class >> projectFromUrl: loadSpecUrl projectsHome: projectsHome componentNames: componentNames [
	| loadSpec resolvedProject |
	loadSpec := (RwSpecification fromUrl: loadSpecUrl)
		projectsHome: projectsHome;
		componentNames: componentNames;
		yourself.
	resolvedProject := loadSpec resolve.
	^ (self newNamed: resolvedProject name)
		_resolvedProject: resolvedProject;
		yourself
]

{ #category : 'instance creation' }
RwResolvedProject class >> projectFromUrl: loadSpecUrl projectsHome: projectsHome componentNames: componentNames customConditionalAttributes: customConditionalAttributes [
	| loadSpec resolvedProject |
	loadSpec := (RwSpecification fromUrl: loadSpecUrl)
		projectsHome: projectsHome;
		componentNames: componentNames;
		customConditionalAttributes: customConditionalAttributes;
		yourself.
	resolvedProject := loadSpec resolve.
	^ (self newNamed: resolvedProject name)
		_resolvedProject: resolvedProject;
		yourself
]

{ #category : 'instance creation' }
RwResolvedProject class >> projectFromUrl: loadSpecUrl projectsHome: projectsHome componentNames: componentNames customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	| loadSpec resolvedProject |
	loadSpec := (RwSpecification fromUrl: loadSpecUrl)
		projectsHome: projectsHome;
		componentNames: componentNames;
		customConditionalAttributes: customConditionalAttributes;
		yourself.
	resolvedProject := loadSpec resolve: platformConditionalAttributes.
	^ (self newNamed: resolvedProject name)
		_resolvedProject: resolvedProject;
		yourself
]

{ #category : 'instance creation' }
RwResolvedProject class >> projectFromUrl: loadSpecUrl projectsHome: projectsHome componentNames: componentNames platformConditionalAttributes: platformConditionalAttributes [
	| loadSpec resolvedProject |
	loadSpec := (RwSpecification fromUrl: loadSpecUrl)
		projectsHome: projectsHome;
		componentNames: componentNames;
		yourself.
	resolvedProject := loadSpec resolve: platformConditionalAttributes.
	^ (self newNamed: resolvedProject name)
		_resolvedProject: resolvedProject;
		yourself
]

{ #category : 'accessing' }
RwResolvedProject >> _resolvedProject: aResolvedProject [
	resolvedProject := aResolvedProject
]

{ #category : 'accessing' }
RwResolvedProject >> repositoryRoot [
	^ self _resolvedProject repositoryRoot
]

{ #category : 'accessing' }
RwResolvedProject >> specsRoot [
	^ self _resolvedProject specsRoot
]

{ #category : 'transitions' }
RwResolvedProject >> write [

	self _resolvedProject
		export;
		exportLoadSpecification
]
