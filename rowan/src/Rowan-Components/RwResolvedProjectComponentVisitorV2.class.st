Class {
	#name : 'RwResolvedProjectComponentVisitorV2',
	#superclass : 'Object',
	#instVars : [
		'projectLoadSpecs',
		'readComponents',
		'readProjects',
		'visitedComponents',
		'visitedComponentNames',
		'customConditionalAttributes',
		'platformConditionalAttributes',
		'definedGroupNames',
		'projectNames',
		'groupNames',
		'componentNames',
		'resolvedProject'
	],
	#category : 'Rowan-Components'
}

{ #category : 'read load specs' }
RwResolvedProjectComponentVisitorV2 class >> _readProjectsForProject: resolvedProject platformConditionalAttributes: platformConditionalAttributes intoProjectSet: intoProjectSet [
	| entitySetDefinition visitor projectVisitorQueue projectVisitedQueue processedProjects |
	entitySetDefinition := intoProjectSet
		ifTrue: [ RwProjectSetDefinition new ]
		ifFalse: [ RwLoadSpecSet new ].
	projectVisitedQueue := {}.
	projectVisitorQueue := {resolvedProject}.
	processedProjects := Dictionary new.
	[ projectVisitorQueue isEmpty ]
		whileFalse: [ 
			| pp |
			pp := projectVisitorQueue removeFirst.

			visitor := self
				readLoadSpecForProject: pp
				platformConditionalAttributes: platformConditionalAttributes.

			processedProjects at: pp projectName put: pp.

			projectVisitedQueue
				addLast:
					{visitor.
					pp}.

			visitor projectLoadSpecs
				do: [ :loadSpec | 
					| theResolvedProject projectsHome |
					"derive resolved project from the load spec ... project load specs are read from the rowan/projects directory"
					projectsHome := loadSpec projectsHome ifNil: [ pp projectsHome ].
					loadSpec relativeRepositoryRoot isEmpty
						ifFalse: [ 
							"using an embedded project"
							resolvedProject loadSpecification updateEmbeddedProjectLoadSpec: loadSpec ].
					theResolvedProject := (loadSpec projectsHome: projectsHome) resolveProject.
					processedProjects
						at: theResolvedProject projectName
						ifAbsent: [ 
							"required project has not been processed, add to the project visitor queue"
							projectVisitorQueue addLast: theResolvedProject ] ] ].
	projectVisitedQueue
		do: [ :visitedArray | 
			| theResolvedProject theLoadSpec |
			theResolvedProject := visitedArray at: 2.
			theLoadSpec := theResolvedProject loadSpecification copy.
			intoProjectSet
				ifTrue: [ entitySetDefinition addProject: theLoadSpec read ]
				ifFalse: [ entitySetDefinition addLoadSpec: theLoadSpec ] ].
	^ entitySetDefinition
]

{ #category : 'instance creation' }
RwResolvedProjectComponentVisitorV2 class >> new [

	^super new initialize

]

{ #category : 'read load specs' }
RwResolvedProjectComponentVisitorV2 class >> readLoadSpecForProject: resolvedProject [
	| visitor |
	visitor := self new
		_readComponentsForProject: resolvedProject.
	resolvedProject
		projectDefinitionSourceProperty:
				RwLoadedProject _projectDiskDefinitionSourceValue;
		_projectDefinitionPlatformConditionalAttributes:
				resolvedProject platformConditionalAttributes copy;
		yourself.
	visitor visitedComponents
		keysAndValuesDo: [ :cName :cmp | resolvedProject _projectComponents _addComponent: cmp ].
	^ visitor
]

{ #category : 'read load specs' }
RwResolvedProjectComponentVisitorV2 class >> readLoadSpecForProject: resolvedProject platformConditionalAttributes: platformConditionalAttributes [
	| visitor |
	visitor := self new
		_readComponentsForProject: resolvedProject
		platformConditionalAttributes: platformConditionalAttributes.
	resolvedProject
		projectDefinitionSourceProperty:
				RwLoadedProject _projectDiskDefinitionSourceValue;
		_projectDefinitionPlatformConditionalAttributes:
				platformConditionalAttributes copy;
		yourself.
	visitor visitedComponents
		keysAndValuesDo: [ :cName :cmp | resolvedProject _projectComponents _addComponent: cmp ].
	^ visitor
]

{ #category : 'read load specs' }
RwResolvedProjectComponentVisitorV2 class >> readLoadSpecSetForProject: resolvedProject [
	^ self
		readLoadSpecSetForProject: resolvedProject
		platformConditionalAttributes: resolvedProject platformConditionalAttributes
]

{ #category : 'read load specs' }
RwResolvedProjectComponentVisitorV2 class >> readLoadSpecSetForProject: resolvedProject platformConditionalAttributes: platformConditionalAttributes [
	^ self
		_readProjectsForProject: resolvedProject
		platformConditionalAttributes: platformConditionalAttributes
		intoProjectSet: false
]

{ #category : 'reading' }
RwResolvedProjectComponentVisitorV2 class >> readProjectForProject: aResolvedProject withComponentNames: componentNamesToRead customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	| visitor |
	visitor := self new
		_readComponentsForProject: aResolvedProject
		withComponentNames: componentNamesToRead
		customConditionalAttributes: customConditionalAttributes
		platformConditionalAttributes: platformConditionalAttributes.
	aResolvedProject
		projectDefinitionSourceProperty:
				RwLoadedProject _projectDiskDefinitionSourceValue;
		_projectDefinitionPlatformConditionalAttributes:
				platformConditionalAttributes copy;
		yourself.
	visitor visitedComponents
		keysAndValuesDo: [ :cName :cmp | aResolvedProject _projectComponents _addComponent: cmp ].
	^ visitor
]

{ #category : 'read load specs' }
RwResolvedProjectComponentVisitorV2 class >> readProjectSetForProject: resolvedProject platformConditionalAttributes: platformConditionalAttributes [
	^ self
		_readProjectsForProject: resolvedProject
		platformConditionalAttributes: platformConditionalAttributes
		intoProjectSet: true
]

{ #category : 'reading' }
RwResolvedProjectComponentVisitorV2 class >> readProjectSetForProject: resolvedProject withComponentNames: componentNamesToRead customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	| projectSetDefinition visitor projectVisitorQueue projectVisitedQueue processedProjects |
	projectSetDefinition := RwProjectSetDefinition new.
	projectVisitedQueue := {}.
	projectVisitorQueue := {{resolvedProject.
	componentNamesToRead.
	customConditionalAttributes}}.
	processedProjects := Dictionary new.
	[ projectVisitorQueue isEmpty ]
		whileFalse: [ 
			| nextDefArray rp cn cca |
			nextDefArray := projectVisitorQueue removeFirst.
			rp := nextDefArray at: 1.
			cn := nextDefArray at: 2.
			cca := nextDefArray at: 3.

			visitor := self
				readProjectForProject: rp
				withComponentNames: cn
				customConditionalAttributes: cca
				platformConditionalAttributes: platformConditionalAttributes.

			processedProjects at: rp projectName put: rp.

			projectVisitedQueue
				addLast:
					{visitor.
					nextDefArray}.

			visitor projectLoadSpecs
				do: [ :loadSpec | 
					| theResolvedProject theLoadSpec projectsHome |
					"derive resolved project from the load spec ... project load specs are read from the rowan/projects directory"
					loadSpec relativeRepositoryRoot isEmpty
						ifFalse: [ 
							"using an embedded project"
							resolvedProject loadSpecification updateEmbeddedProjectLoadSpec: loadSpec ].
					projectsHome := (System gemEnvironmentVariable: 'ROWAN_PROJECTS_HOME')
						ifNil: [ rp projectsHome ].
					theResolvedProject := (loadSpec projectsHome: projectsHome) resolveProject.
					theLoadSpec := loadSpec.
					(processedProjects at: theResolvedProject projectName ifAbsent: [  ])
						ifNil: [ 
							"required project has not been processed, add to the project visitor queue"
							projectVisitorQueue
								addLast:
									{theResolvedProject.
									(theLoadSpec componentNames).
									(theResolvedProject customConditionalAttributes)} ]
						ifNotNil: [ :processedProject | 
							"recursive required project structure"
							(processedProject loadSpecification
								loadConflictsWith: theResolvedProject loadSpecification)
								ifTrue: [ 
									"the required load spec is incompatible with the incoming load spec ... this is an error"
									self
										error:
											'load conflicts between the load specs for the required project '
												, theResolvedProject projectName ] ] ] ].
	projectVisitedQueue
		do: [ :visitedArray | 
			| ndf theVisitor theResolvedProject |
			theVisitor := visitedArray at: 1.
			ndf := visitedArray at: 2.
			theResolvedProject := ndf at: 1.
			theResolvedProject readPackageNames: theResolvedProject packageNames.
			projectSetDefinition addProject: theResolvedProject ].
	^ projectSetDefinition
]

{ #category : 'required projects' }
RwResolvedProjectComponentVisitorV2 class >> requiredProjectNamesForProject: resolvedProject [
	| requiredProjectNames visitor projectVisitorQueue projectVisitedQueue processedProjects |
	requiredProjectNames := Set new.
	projectVisitedQueue := {}.
	projectVisitorQueue := {resolvedProject}.
	processedProjects := Dictionary new.
	[ projectVisitorQueue isEmpty ]
		whileFalse: [ 
			| pp |
			pp := projectVisitorQueue removeFirst.

			visitor := self readLoadSpecForProject: pp.

			processedProjects at: pp projectName put: pp.

			projectVisitedQueue
				addLast:
					{visitor.
					pp}.

			visitor projectLoadSpecs
				do: [ :loadSpec | 
					| theResolvedProject projectsHome |
					"derive resolved project from the load spec"
					projectsHome := (System gemEnvironmentVariable: 'ROWAN_PROJECTS_HOME')
						ifNil: [ pp projectsHome ].
					theResolvedProject := (loadSpec projectsHome: projectsHome) resolveProject.
					processedProjects
						at: theResolvedProject projectName
						ifAbsent: [ 
							"required project has not been processed, add to the project visitor queue"
							projectVisitorQueue addLast: theResolvedProject ] ] ].
	projectVisitedQueue
		do: [ :visitedArray | 
			| pp theVisitor |
			theVisitor := visitedArray at: 1.
			pp := visitedArray at: 2.
			requiredProjectNames add: pp projectName ].
	^ requiredProjectNames
]

{ #category : 'private' }
RwResolvedProjectComponentVisitorV2 >> _addPackageNames: somePackageNames for: aComponent [

	self resolvedProject addPackages: somePackageNames forComponent: aComponent
]

{ #category : 'private' }
RwResolvedProjectComponentVisitorV2 >> _components: componentDirPath forProject: aProjectName [
	| componentDirectory selected |
	self componentNames isEmpty
		ifTrue: [ ^ #() ].
	componentDirectory := componentDirPath asFileReference.
	selected := (self componentNames
		select: [ :componentName | (visitedComponentNames includes: componentName) not ])
		collect: [ :componentName | 
			self readComponents
				at: componentName
				ifAbsentPut: [ 
 					(RwAbstractRowanProjectLoadComponentV2
						fromComponentsDirectory: componentDirectory
						named: componentName)
						projectName: aProjectName;
						yourself ] ].
	^ selected

]

{ #category : 'private' }
RwResolvedProjectComponentVisitorV2 >> _matchPlatformAttributes: platformPatternMatcher [

	self conditionalAttributes do: [:anObject |
		(platformPatternMatcher match: anObject) ifTrue: [ ^true ] ].
	^false
]

{ #category : 'private' }
RwResolvedProjectComponentVisitorV2 >> _platformAttributeMatchIn: platformMatchersList [

	platformMatchersList do: [:platformPatternMatcher |
		(self _matchPlatformAttributes: platformPatternMatcher) 
			ifTrue: [ ^true ] ].
	^false

]

{ #category : 'private' }
RwResolvedProjectComponentVisitorV2 >> _projects: projectDirPath forProject: ignored [
	| urlBase |
	self projectNames isEmpty
		ifTrue: [ ^ #() ].
	urlBase := 'file:' , projectDirPath asFileReference pathString , '/'.
	^ self projectNames
		collect: [ :prjName | 
			self readProjects
				at: prjName
				ifAbsentPut: [ 
					| url |
					url := urlBase , prjName , '.ston'.
					RwSpecification fromUrl: url ] ]

]

{ #category : 'private' }
RwResolvedProjectComponentVisitorV2 >> _readComponentsForProject: aResolvedProject [
	^ self
		_readComponentsForProject: aResolvedProject
		withComponentNames: aResolvedProject componentNames
		customConditionalAttributes: aResolvedProject customConditionalAttributes
		platformConditionalAttributes: aResolvedProject platformConditionalAttributes
]

{ #category : 'private' }
RwResolvedProjectComponentVisitorV2 >> _readComponentsForProject: aResolvedProject platformConditionalAttributes: aPlatformConditionalAttributes [
	^ self
		_readComponentsForProject: aResolvedProject
		withComponentNames: aResolvedProject componentNames
		customConditionalAttributes: aResolvedProject customConditionalAttributes
		platformConditionalAttributes: aPlatformConditionalAttributes
]

{ #category : 'private' }
RwResolvedProjectComponentVisitorV2 >> _readComponentsForProject: aResolvedProject withComponentNames: componentNamesToRead  customConditionalAttributes: aCustomConditionalAttributes platformConditionalAttributes: aPlatformConditionalAttributes [
	| theComponentNames  |
	resolvedProject := aResolvedProject.
	platformConditionalAttributes := aPlatformConditionalAttributes.
	customConditionalAttributes := aCustomConditionalAttributes.

	resolvedProject _projectComponents: RwResolvedProjectComponentsV2 new.	"build new list of components based on (potentially) new list of componentNames"
	resolvedProject _projectDefinition packages: Dictionary new.	"bulid new list of packages as well"
	theComponentNames := componentNamesToRead isEmpty
		ifTrue: [ resolvedProject componentNames ]
		ifFalse: [ componentNamesToRead ].
	^ self _visitComponents: theComponentNames
]

{ #category : 'private' }
RwResolvedProjectComponentVisitorV2 >> _visitComponents: componentNamesToRead [
	| projectName componentDirectory projectsDirectory |

	projectName := resolvedProject projectName.
	componentDirectory := resolvedProject componentsRoot.
	componentDirectory exists
		ifFalse: [ 
			^ self
				error:
					'No component directory (' , componentDirectory pathString printString
						, ') found for project ' , projectName printString ].
	projectsDirectory := resolvedProject projectsRoot.
	projectsDirectory exists
		ifFalse: [ 
			^ self
				error:
					'No projects directory (' , projectsDirectory pathString printString
						, ') found for project ' , projectName printString ].
	componentNamesToRead
		do: [ :componentName | 
			| component |
			component := self readComponents
				at: componentName
				ifAbsentPut: [ 
					RwAbstractRowanProjectLoadComponentV2
						fromComponentsDirectory: componentDirectory
						named: componentName ].
			component projectName: projectName.

			self visit: component	"expect all component names to represent loadable components - throw error if a nested component is encountered" ]
]

{ #category : 'private' }
RwResolvedProjectComponentVisitorV2 >> _visited: aComponent [

	visitedComponentNames add:  aComponent name.
	visitedComponents at: aComponent name put: aComponent.
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> componentNames [

	^ componentNames
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> componentsPath [

	^ self resolvedProject componentsRoot
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> conditionalAttributes [

	^ self platformConditionalAttributes, self customConditionalAttributes
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> customConditionalAttributes [

	^ customConditionalAttributes
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> customConditionalAttributes: aColl [

	customConditionalAttributes := aColl
]

{ #category : 'initialization' }
RwResolvedProjectComponentVisitorV2 >> initialize [
	visitedComponentNames := Set new.
	projectNames := Set new.
	componentNames := Set new.
	readComponents := Dictionary new.
	readProjects := Dictionary new.
	platformConditionalAttributes := #().
	customConditionalAttributes := #().
	projectLoadSpecs := Set new.
	visitedComponents := Dictionary new
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> packageNames [
	^ self resolvedProject packageNames
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> platformConditionalAttributes [

	^ platformConditionalAttributes
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> platformConditionalAttributes: aColl [

	platformConditionalAttributes := aColl
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
RwResolvedProjectComponentVisitorV2 >> projectsPath [

	^ self resolvedProject projectsRoot
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> readComponents [

	^ readComponents
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> readProjects [

	^ readProjects
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
RwResolvedProjectComponentVisitorV2 >> visit: aProjectLoadComponent [

	^aProjectLoadComponent acceptVisitor: self
]

{ #category : 'visiting' }
RwResolvedProjectComponentVisitorV2 >> visitComponent: aComponent [
	(visitedComponentNames includes: aComponent name)
		ifTrue: [ ^ self ].

	self _visited: aComponent.

	aComponent conditionalPropertyMatchers
		keysAndValuesDo: [ :platformMatchers :ignored | 
			(self _platformAttributeMatchIn: platformMatchers)
				ifTrue: [ 
					self _addPackageNames: aComponent packageNames for: aComponent.
					self componentNames addAll: aComponent componentNames.
					self projectNames addAll: aComponent projectNames ] ].

	(self _components: self componentsPath forProject: aComponent projectName)
		do: [ :component | 
			(visitedComponentNames includes: component name)
				ifFalse: [ component acceptNestedVisitor: self ] ].

	(self _projects: self projectsPath forProject: aComponent projectName)
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
RwResolvedProjectComponentVisitorV2 >> visitPackageGroupComponent: aComponent [
	(visitedComponentNames includes: aComponent name)
		ifTrue: [ ^ self ].

	self _visited: aComponent.

	aComponent conditionalPropertyMatchers
		keysAndValuesDo: [ :platformMatchers :ignored | 
			(self _platformAttributeMatchIn: platformMatchers)
				ifTrue: [ 
					"package group component does not impact list of packages or projects - only used in project browser"
					self componentNames addAll: aComponent componentNames ] ].

	(self _components: self componentsPath forProject: aComponent projectName)
		do: [ :component | 
			(visitedComponentNames includes: component name)
				ifFalse: [ component acceptNestedVisitor: self ] ].

	(self _projects: self projectsPath forProject: aComponent projectName)
		do: [ :projectSpec | projectSpec acceptVisitor: self ]
]
