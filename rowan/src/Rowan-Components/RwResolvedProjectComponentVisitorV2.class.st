Class {
	#name : 'RwResolvedProjectComponentVisitorV2',
	#superclass : 'Object',
	#instVars : [
		'projectLoadSpecs',
		'readComponents',
		'readProjects',
		'visitedComponents',
		'visitedComponentNames',
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

{ #category : 'reading' }
RwResolvedProjectComponentVisitorV2 class >> readProjectForResolvedProject: resolvedProject withComponentNames: componentNamesToRead platformConditionalAttributes: platformConditionalAttributes [
	| visitor |
	visitor := self new
		_readComponentsForResolvedProject: resolvedProject
		withComponentNames: componentNamesToRead
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

{ #category : 'reading' }
RwResolvedProjectComponentVisitorV2 class >> readProjectSetForResolvedProject: resolvedProject withComponentNames: componentNamesToRead platformConditionalAttributes: platformConditionalAttributes [
	| projectSetDefinition visitor projectVisitorQueue projectVisitedQueue |
	projectSetDefinition := RwProjectSetDefinition new.
	projectVisitedQueue := {}.
	projectVisitorQueue := {{resolvedProject.
	componentNamesToRead.
	platformConditionalAttributes}}.
	[ projectVisitorQueue isEmpty ]
		whileFalse: [ 
			| nextDefArray rp cn pca |
			nextDefArray := projectVisitorQueue removeFirst.
			rp := nextDefArray at: 1.
			cn := nextDefArray at: 2.
			pca := nextDefArray at: 3.

			visitor := self
				readProjectForResolvedProject: rp
				withComponentNames: cn
				platformConditionalAttributes: pca.

			projectVisitedQueue
				addLast:
					{visitor.
					nextDefArray}.

			visitor projectLoadSpecs
				do: [ :loadSpec | 
					| theResolvedProject theLoadSpec |
					(Rowan projectNamed: loadSpec projectName ifAbsent: [  ])
						ifNotNil: [ :project | 
							"project is already present in image ... so use it"
							theLoadSpec := project _loadSpecification.
							theResolvedProject := project asDefinition resolve ]
						ifNil: [ 
							"derive resolved project from the load spec"
							theResolvedProject := loadSpec resolveWithParentProject: rp.	"give embedded projects a chance"
							theLoadSpec := loadSpec ].
					projectVisitorQueue
						addLast:
							{theResolvedProject.
							(theLoadSpec componentNames).
							(theResolvedProject platformConditionalAttributes)} ] ].
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

{ #category : 'instance creation' }
RwResolvedProjectComponentVisitorV2 class >> resolvedProject: resolvedProject platformConditionalAttributes: platformConditionalAttributes [
	^ self new
		platformConditionalAttributes: platformConditionalAttributes;
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

	self platformConditionalAttributes do: [:anObject |
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
RwResolvedProjectComponentVisitorV2 >> _readComponentsForResolvedProject: aResolvedProject withComponentNames: componentNamesToRead  platformConditionalAttributes: aPlatformConditionalAttributes [
	| theComponentNames  |
	resolvedProject := aResolvedProject.
	platformConditionalAttributes := aPlatformConditionalAttributes.

	resolvedProject _projectComponents: RwResolvedProjectComponentsV2 new.	"build new list of components based on (potentially) new list of configNames"
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

{ #category : 'initialization' }
RwResolvedProjectComponentVisitorV2 >> initialize [
	visitedComponentNames := Set new.
	projectNames := Set new.
	componentNames := Set new.
	readComponents := Dictionary new.
	readProjects := Dictionary new.
	platformConditionalAttributes := #().
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
