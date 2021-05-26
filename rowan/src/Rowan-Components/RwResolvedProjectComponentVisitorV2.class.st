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

{ #category : 'instance creation' }
RwResolvedProjectComponentVisitorV2 class >> new [

	^super new initialize

]

{ #category : 'read load specs' }
RwResolvedProjectComponentVisitorV2 class >> readLoadSpecForProducedProject: producedProject [
	| visitor |
	visitor := self new
		_readComponentsForProducedProject: producedProject.
	producedProject
		projectDefinitionSourceProperty:
				RwLoadedProject _projectDiskDefinitionSourceValue;
		_projectDefinitionCustomConditionalAttributes:
				producedProject customConditionalAttributes copy;
		_projectDefinitionPlatformConditionalAttributes:
				producedProject platformConditionalAttributes copy;
		yourself.
	visitor visitedComponents
		keysAndValuesDo: [ :cName :cmp | producedProject _projectComponents _addComponent: cmp ].
	^ visitor
]

{ #category : 'to be removed' }
RwResolvedProjectComponentVisitorV2 class >> readLoadSpecForResolvedProject: resolvedProject withComponentNames: componentNamesToRead customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [ 
	| visitor |
	visitor := self new
		_readComponentsForResolvedProject: resolvedProject
		withComponentNames: componentNamesToRead
		customConditionalAttributes: customConditionalAttributes
		platformConditionalAttributes: platformConditionalAttributes.
	resolvedProject
		projectDefinitionSourceProperty:
				RwLoadedProject _projectDiskDefinitionSourceValue;
		_projectDefinitionCustomConditionalAttributes:
				customConditionalAttributes copy;
		_projectDefinitionPlatformConditionalAttributes:
				platformConditionalAttributes copy;
		yourself.
	visitor visitedComponents
		keysAndValuesDo: [ :cName :cmp | resolvedProject _projectComponents _addComponent: cmp ].
	^ visitor
]

{ #category : 'read load specs' }
RwResolvedProjectComponentVisitorV2 class >> readLoadSpecSetForProducedProject: producedProject [
	| loadSpecSet visitor projectVisitorQueue projectVisitedQueue processedProjects |
	loadSpecSet := RwLoadSpecSet new.
	projectVisitedQueue := {}.
	projectVisitorQueue := {producedProject}.
	processedProjects := Dictionary new.
	[ projectVisitorQueue isEmpty ]
		whileFalse: [ 
			| pp cn cca |
			pp := projectVisitorQueue removeFirst.
			cn := pp componentNames.
			cca := pp customConditionalAttributes.

			visitor := self readLoadSpecForProducedProject: pp.

			processedProjects at: pp projectName put: pp.

			projectVisitedQueue
				addLast:
					{visitor.
					pp}.

			visitor projectLoadSpecs
				do: [ :loadSpec | 
					| theProducedProject |
					"derive resolved project from the load spec"
					theProducedProject := loadSpec produceWithParentProject: pp.
					processedProjects
						at: theProducedProject projectName
						ifAbsent: [ 
							"required project has not been processed, add to the project visitor queue"
							projectVisitorQueue addLast: theProducedProject ] ] ].
	projectVisitedQueue
		do: [ :visitedArray | 
			| pp theVisitor theproducedProject theLoadSpec theLoadedProject |
			theVisitor := visitedArray at: 1.
			pp := visitedArray at: 2.
			theproducedProject := pp.
			theLoadSpec := theproducedProject _loadSpecification copy.
			(theLoadedProject := Rowan
				projectNamed: theLoadSpec projectName
				ifAbsent: [  ])
				ifNotNil: [ 
					"project is loaded, so we need to preserve repository root"
					theLoadedProject updateLoadSpecWithRepositoryRoot: theLoadSpec ].
			loadSpecSet addLoadSpec: theLoadSpec ].
	^ loadSpecSet
]

{ #category : 'to be removed' }
RwResolvedProjectComponentVisitorV2 class >> readLoadSpecSetForResolvedProject: resolvedProject withComponentNames: componentNamesToRead customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [ 
	| loadSpecSet visitor projectVisitorQueue projectVisitedQueue processedProjects |
	loadSpecSet := RwLoadSpecSet new.
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
				readLoadSpecForResolvedProject: rp
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
					| theResolvedProject |
					"derive resolved project from the load spec"
					theResolvedProject := loadSpec resolveWithParentProject: rp.
					processedProjects
						at: theResolvedProject projectName
						ifAbsent: [ 
							"required project has not been processed, add to the project visitor queue"
							projectVisitorQueue
								addLast:
									{theResolvedProject.
									(loadSpec componentNames).
									(theResolvedProject customConditionalAttributes)} ] ] ].
	projectVisitedQueue
		do: [ :visitedArray | 
			| ndf theVisitor theResolvedProject theLoadSpec theLoadedProject |
			theVisitor := visitedArray at: 1.
			ndf := visitedArray at: 2.
			theResolvedProject := ndf at: 1.
			theLoadSpec := theResolvedProject _loadSpecification copy.
			(theLoadedProject := Rowan
				projectNamed: theLoadSpec projectName
				ifAbsent: [  ])
				ifNotNil: [ 
					"project is loaded, so we need to preserve repository root"
					theLoadedProject updateLoadSpecWithRepositoryRoot: theLoadSpec ].
			loadSpecSet addLoadSpec: theLoadSpec ].
	^ loadSpecSet
]

{ #category : 'to be removed' }
RwResolvedProjectComponentVisitorV2 class >> readProjectForResolvedProject: resolvedProject withComponentNames: componentNamesToRead customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	| visitor |
	visitor := self new
		_readComponentsForResolvedProject: resolvedProject
		withComponentNames: componentNamesToRead
		customConditionalAttributes: customConditionalAttributes
		platformConditionalAttributes: platformConditionalAttributes.
	resolvedProject
		projectDefinitionSourceProperty:
				RwLoadedProject _projectDiskDefinitionSourceValue;
		_projectDefinitionCustomConditionalAttributes:
				customConditionalAttributes copy;
		_projectDefinitionPlatformConditionalAttributes:
				platformConditionalAttributes copy;
		yourself.
	visitor visitedComponents
		keysAndValuesDo: [ :cName :cmp | resolvedProject _projectComponents _addComponent: cmp ].
	^ visitor
]

{ #category : 'to be removed' }
RwResolvedProjectComponentVisitorV2 class >> readProjectSetForResolvedProject: resolvedProject withComponentNames: componentNamesToRead customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	^ self
		readProjectSetForResolvedProject: resolvedProject
		withComponentNames: componentNamesToRead
		customConditionalAttributes: customConditionalAttributes
		platformConditionalAttributes: platformConditionalAttributes
		useLoadedProjects: false
]

{ #category : 'to be removed' }
RwResolvedProjectComponentVisitorV2 class >> readProjectSetForResolvedProject: resolvedProject withComponentNames: componentNamesToRead customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes useLoadedProjects: useLoadedProjects [
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
				readProjectForResolvedProject: rp
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
					| theResolvedProject theLoadSpec theLoadedProject |
					theLoadedProject := Rowan projectNamed: loadSpec projectName ifAbsent: [  ].
					(theLoadedProject notNil and: [ useLoadedProjects ])
						ifTrue: [ 
							"project is already present in image ... so use it"
							theLoadSpec := theLoadedProject _loadSpecification.
							theResolvedProject := theLoadedProject asDefinition resolve.
							(loadSpec loadConflictsWith: theLoadSpec)
								ifTrue: [ 
									"the load spec for the loaded project is incompatible with the required load sped ... this is an error"
									self
										error:
											'load conflicts between the load spec for the loaded project and the load spec for the required project '
												, theLoadedProject projectName ] ]
						ifFalse: [ 
							"derive resolved project from the load spec"
							theResolvedProject := loadSpec resolveWithParentProject: rp.	"give embedded projects a chance"
							theLoadSpec := loadSpec ].
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
							(processedProject _loadSpecification
								loadConflictsWith: theResolvedProject _loadSpecification)
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

{ #category : 'to be removed' }
RwResolvedProjectComponentVisitorV2 class >> readProjectSetForResolvedProject: resolvedProject withComponentNames: componentNamesToRead platformConditionalAttributes: platformConditionalAttributes [
	^ self
		readProjectSetForResolvedProject: resolvedProject
		withComponentNames: componentNamesToRead
		customConditionalAttributes: resolvedProject customConditionalAttributes
		platformConditionalAttributes: platformConditionalAttributes
		useLoadedProjects: false
]

{ #category : 'to be removed' }
RwResolvedProjectComponentVisitorV2 class >> resolvedProject: resolvedProject customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	^ self new
		platformConditionalAttributes: platformConditionalAttributes;
		customConditionalAttributes: customConditionalAttributes;
		resolvedProject: resolvedProject;
		yourself
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
RwResolvedProjectComponentVisitorV2 >> _readComponentsForProducedProject: aResolvedProject [
	resolvedProject := aResolvedProject.
	platformConditionalAttributes := aResolvedProject platformConditionalAttributes.
	customConditionalAttributes := aResolvedProject customConditionalAttributes.

	resolvedProject _projectComponents: RwResolvedProjectComponentsV2 new.	"build new list of components based on (potentially) new list of componentNames"
	resolvedProject _projectDefinition packages: Dictionary new.	"bulid new list of packages as well"
	^ self _visitComponents: resolvedProject componentNames
]

{ #category : 'to be removed' }
RwResolvedProjectComponentVisitorV2 >> _readComponentsForResolvedProject: aResolvedProject withComponentNames: componentNamesToRead  customConditionalAttributes: aCustomConditionalAttributes platformConditionalAttributes: aPlatformConditionalAttributes [
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
