Class {
	#name : 'RwProjectLoadComponentVisitor',
	#superclass : 'RwProjectLoadConfigurationVisitor',
	#instVars : [
		'projectNames',
		'projectBasePath',
		'projectLoadSpecs',
		'visitedComponents',
		'projectComponentDefinition'
	],
	#category : 'Rowan-Components'
}

{ #category : 'private' }
RwProjectLoadComponentVisitor >> _addPackageNames: somePackageNames for: aComponent [

	self projectComponentDefinition addPackages: somePackageNames forComponent: aComponent
]

{ #category : 'private' }
RwProjectLoadComponentVisitor >> _projects: projectDirPath forProject: ignored [

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
RwProjectLoadComponentVisitor >> _visited: aComponent [

	super _visited: aComponent.
	visitedComponents at: aComponent name put: aComponent.
]

{ #category : 'initialization' }
RwProjectLoadComponentVisitor >> initialize [

	super initialize.
	projectNames := Set new.
	projectLoadSpecs := Set new.
	visitedComponents := Dictionary new
]

{ #category : 'accessing' }
RwProjectLoadComponentVisitor >> projectBasePath [

	^ projectBasePath
]

{ #category : 'accessing' }
RwProjectLoadComponentVisitor >> projectBasePath: aString [

	projectBasePath := aString
]

{ #category : 'accessing' }
RwProjectLoadComponentVisitor >> projectComponentDefinition [

	^ projectComponentDefinition
]

{ #category : 'accessing' }
RwProjectLoadComponentVisitor >> projectComponentDefinition: aProjectComponentDefinition [

	projectComponentDefinition := aProjectComponentDefinition
]

{ #category : 'accessing' }
RwProjectLoadComponentVisitor >> projectLoadSpecs [

	^ projectLoadSpecs
]

{ #category : 'accessing' }
RwProjectLoadComponentVisitor >> projectNames [

	^ projectNames
]

{ #category : 'visiting' }
RwProjectLoadComponentVisitor >> visitComponentLoadConfiguration: aComponentLoadConfiguration [

	(visitedConfigurationNames includes: aComponentLoadConfiguration name)
		ifTrue: [ ^ self ].

	self _visited: aComponentLoadConfiguration. 

	definedGroupNames := aComponentLoadConfiguration definedGroupNames.
	self _processGroupNames.

	self _processConditionalPackageNames: aComponentLoadConfiguration.

	self _processConditionalPackageMapSpecs: aComponentLoadConfiguration.

	self configurationNames addAll: aComponentLoadConfiguration configurationNames.
	(self _configurations: self configurationBasePath, '/' forProject: aComponentLoadConfiguration projectName)
		do: [:config | config acceptNestedVisitor: self ].

	self projectNames addAll: aComponentLoadConfiguration projectNames.
	(self _projects: self projectBasePath, '/' forProject: aComponentLoadConfiguration projectName)
		do: [:projectSpec | projectSpec acceptVisitor: self ].
]

{ #category : 'visiting' }
RwProjectLoadComponentVisitor >> visitComponentSpecification: aComponentSpecification [

	self projectLoadSpecs add: aComponentSpecification
]

{ #category : 'accessing' }
RwProjectLoadComponentVisitor >> visitedComponents [

	^ visitedComponents
]
