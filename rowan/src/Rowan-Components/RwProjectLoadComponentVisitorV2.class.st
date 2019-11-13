Class {
	#name : 'RwProjectLoadComponentVisitorV2',
	#superclass : 'RwProjectLoadConfigurationVisitorV2',
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
RwProjectLoadComponentVisitorV2 >> _addPackageNames: somePackageNames for: aComponent [

	self projectComponentDefinition addPackages: somePackageNames forComponent: aComponent
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
RwProjectLoadComponentVisitorV2 >> projectBasePath [

	^ projectBasePath
]

{ #category : 'accessing' }
RwProjectLoadComponentVisitorV2 >> projectBasePath: aString [

	projectBasePath := aString
]

{ #category : 'accessing' }
RwProjectLoadComponentVisitorV2 >> projectComponentDefinition [

	^ projectComponentDefinition
]

{ #category : 'accessing' }
RwProjectLoadComponentVisitorV2 >> projectComponentDefinition: aProjectComponentDefinition [

	projectComponentDefinition := aProjectComponentDefinition
]

{ #category : 'accessing' }
RwProjectLoadComponentVisitorV2 >> projectLoadSpecs [

	^ projectLoadSpecs
]

{ #category : 'accessing' }
RwProjectLoadComponentVisitorV2 >> projectNames [

	^ projectNames
]

{ #category : 'visiting' }
RwProjectLoadComponentVisitorV2 >> visitComponentLoadConfiguration: aComponentLoadConfiguration [

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
RwProjectLoadComponentVisitorV2 >> visitComponentSpecification: aComponentSpecification [

	self projectLoadSpecs add: aComponentSpecification
]

{ #category : 'accessing' }
RwProjectLoadComponentVisitorV2 >> visitedComponents [

	^ visitedComponents
]
