Class {
	#name : 'RwResolvedProjectComponentVisitorV2',
	#superclass : 'RwAbstractProjectComponentVisitorV2',
	#instVars : [
		'resolvedProject'
	],
	#category : 'Rowan-ComponentsV2'
}

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
		projectDefinitionPlatformConditionalAttributes:
				platformConditionalAttributes copy;
		yourself.
	visitor visitedComponents
		keysAndValuesDo: [ :cName :cmp | resolvedProject components _addComponent: cmp ].
	^ visitor
]

{ #category : 'reading' }
RwResolvedProjectComponentVisitorV2 class >> readProjectSetForResolvedProject: resolvedProject withComponentNames: componentNamesToRead platformConditionalAttributes: platformConditionalAttributes [
	| projectSetDefinition visitor projectVisitorQueue projectVisitedQueue |
	projectSetDefinition := RwProjectSetDefinition new.
	projectVisitedQueue := {}.
	projectVisitorQueue := {{resolvedProject.
	componentNamesToRead}}.
	[ projectVisitorQueue isEmpty ]
		whileFalse: [ 
			| nextDefArray rp cn |
			nextDefArray := projectVisitorQueue removeFirst.
			rp := nextDefArray at: 1.
			cn := nextDefArray at: 2.

			visitor := self
				readProjectForResolvedProject: rp
				withComponentNames: cn
				platformConditionalAttributes: platformConditionalAttributes.

			projectVisitedQueue
				addLast:
					{visitor.
					nextDefArray}.

			visitor projectLoadSpecs
				do: [ :loadSpec | 
					| theResolvedProject |
					theResolvedProject := loadSpec resolveWithParentProject: rp.	"give enbedded projects a chance"
					projectVisitorQueue
						addLast:
							{theResolvedProject.
							(loadSpec componentNames)} ] ].
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

	self projectDefinition addPackages: somePackageNames forComponent: aComponent
]

{ #category : 'private' }
RwResolvedProjectComponentVisitorV2 >> _readComponentsForResolvedProject: aResolvedProject withComponentNames: componentNamesToRead  platformConditionalAttributes: aPlatformConditionalAttributes [
	| theComponentNames  |
	resolvedProject := aResolvedProject.
	platformConditionalAttributes := aPlatformConditionalAttributes.

	resolvedProject _projectDefinition components: RwResolvedLoadComponentsV2 new.	"build new list of components based on (potentially) new list of configNames"
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
					RwBasicProjectLoadComponentV2
						fromComponentsDirectory: componentDirectory
						named: componentName ].
			component projectName: projectName.

			self visit: component	"expect all component names to represent loadable components - throw error if a nested component is encountered" ]
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> componentsPath [

	^ self resolvedProject componentsRoot
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> packageNames [
	^ self resolvedProject packageNames
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> projectDefinition [
	^ self resolvedProject _projectDefinition
]

{ #category : 'accessing' }
RwResolvedProjectComponentVisitorV2 >> projectsPath [

	^ self resolvedProject projectsRoot
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
