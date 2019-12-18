Class {
	#name : 'RwProjectLoadComponentVisitorV2',
	#superclass : 'RwProjectLoadConfigurationVisitorV2',
	#instVars : [
		'projectNames',
		'projectsRoot',
		'repositoryRoot',
		'projectLoadSpecs',
		'visitedComponents',
		'projectDefinition',
		'resolvedProject'
	],
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'private' }
RwProjectLoadComponentVisitorV2 >> _addPackageNames: somePackageNames for: aComponent [

	self projectDefinition addPackages: somePackageNames forComponent: aComponent
]

{ #category : 'private' }
RwProjectLoadComponentVisitorV2 >> _projects: projectDirPath forProject: ignored [

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
RwProjectLoadComponentVisitorV2 >> _visited: aComponent [

	super _visited: aComponent.
	visitedComponents at: aComponent name put: aComponent.
]

{ #category : 'initialization' }
RwProjectLoadComponentVisitorV2 >> initialize [

	super initialize.
	projectNames := Set new.
	projectLoadSpecs := Set new.
	visitedComponents := Dictionary new
]

{ #category : 'accessing' }
RwProjectLoadComponentVisitorV2 >> projectDefinition [
	^ self resolvedProject _projectDefinition
]

{ #category : 'accessing' }
RwProjectLoadComponentVisitorV2 >> projectLoadSpecs [

	^ projectLoadSpecs
]

{ #category : 'accessing' }
RwProjectLoadComponentVisitorV2 >> projectNames [

	^ projectNames
]

{ #category : 'accessing' }
RwProjectLoadComponentVisitorV2 >> projectsRoot [
	^ projectsRoot
]

{ #category : 'accessing' }
RwProjectLoadComponentVisitorV2 >> projectsRoot: aString [
	projectsRoot := aString asFileReference
]

{ #category : 'accessing' }
RwProjectLoadComponentVisitorV2 >> resolvedProject [
	^ resolvedProject
]

{ #category : 'accessing' }
RwProjectLoadComponentVisitorV2 >> resolvedProject: aResolvedProject [
	resolvedProject := aResolvedProject
]

{ #category : 'visiting' }
RwProjectLoadComponentVisitorV2 >> visitComponentLoadConfiguration: aComponentLoadConfiguration [
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

{ #category : 'visiting' }
RwProjectLoadComponentVisitorV2 >> visitComponentSpecification: aComponentSpecification [

	self projectLoadSpecs add: aComponentSpecification
]

{ #category : 'accessing' }
RwProjectLoadComponentVisitorV2 >> visitedComponents [

	^ visitedComponents
]
