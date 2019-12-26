Class {
	#name : 'RwResolvedProjectComponentVisitorV2',
	#superclass : 'RwAbstractProjectComponentVisitorV2',
	#instVars : [
		'projectNames',
		'projectsRoot',
		'repositoryRoot',
		'projectLoadSpecs',
		'visitedComponents',
		'projectDefinition',
		'resolvedProject'
	],
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'private' }
RwResolvedProjectComponentVisitorV2 >> _addPackageNames: somePackageNames for: aComponent [

	self projectDefinition addPackages: somePackageNames forComponent: aComponent
]

{ #category : 'private' }
RwResolvedProjectComponentVisitorV2 >> _projects: projectDirPath forProject: ignored [

	| urlBase |
	self projectNames isEmpty ifTrue: [ ^ #() ].
	urlBase := 'file:' ,projectDirPath asFileReference pathString, '/'.
	^ self projectNames
		collect: [ :prjName | 
			| url |
			url := urlBase , prjName , '.ston'.
			RwSpecification fromUrl: url ]
]

{ #category : 'private' }
RwResolvedProjectComponentVisitorV2 >> _visited: aComponent [

	super _visited: aComponent.
	visitedComponents at: aComponent name put: aComponent.
]

{ #category : 'initialization' }
RwResolvedProjectComponentVisitorV2 >> initialize [

	super initialize.
	projectNames := Set new.
	projectLoadSpecs := Set new.
	visitedComponents := Dictionary new
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> projectDefinition [
	^ self resolvedProject _projectDefinition
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> projectLoadSpecs [

	^ projectLoadSpecs
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> projectNames [

	^ projectNames
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> projectsRoot [
	^ projectsRoot
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> projectsRoot: aString [
	projectsRoot := aString asFileReference
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> resolvedProject [
	^ resolvedProject
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> resolvedProject: aResolvedProject [
	resolvedProject := aResolvedProject
]

{ #category : 'visiting' }
RwResolvedProjectComponentVisitorV2 >> visitComponentLoadConfiguration: aComponentLoadConfiguration [
	(visitedComponentNames includes: aComponentLoadConfiguration name)
		ifTrue: [ ^ self ].

	self _visited: aComponentLoadConfiguration.

	definedGroupNames := aComponentLoadConfiguration definedGroupNames.
	self _processGroupNames.

	self _processConditionalPackageNames: aComponentLoadConfiguration.

	self componentNames addAll: aComponentLoadConfiguration componentNames.
	(self
		_components: self componentsRoot
		forProject: aComponentLoadConfiguration projectName)
		do: [ :component | component acceptNestedVisitor: self ].

	self projectNames addAll: aComponentLoadConfiguration projectNames.
	(self
		_projects: self projectsRoot
		forProject: aComponentLoadConfiguration projectName)
		do: [ :projectSpec | projectSpec acceptVisitor: self ]
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> visitedComponents [

	^ visitedComponents
]

{ #category : 'visiting' }
RwResolvedProjectComponentVisitorV2 >> visitLoadSpecification: aLoadSpecification [

	self projectLoadSpecs add: aLoadSpecification
]

{ #category : 'visiting' }
RwResolvedProjectComponentVisitorV2 >> visitNestedProjectLoadComponent: aNestedProjectLoadComponent [
	(visitedComponentNames includes: aNestedProjectLoadComponent name)
		ifTrue: [ ^ self ].

	self _visited: aNestedProjectLoadComponent.

	definedGroupNames := aNestedProjectLoadComponent definedGroupNames.
	self _processGroupNames.

	self _processConditionalPackageNames: aNestedProjectLoadComponent.

	self componentNames addAll: aNestedProjectLoadComponent componentNames.
	(self
		_components: self componentsRoot
		forProject: aNestedProjectLoadComponent projectName)
		do: [ :component | component acceptNestedVisitor: self ].

	self projectNames addAll: aNestedProjectLoadComponent projectNames.
	(self
		_projects: self projectsRoot
		forProject: aNestedProjectLoadComponent projectName)
		do: [ :projectSpec | projectSpec acceptVisitor: self ]
]
