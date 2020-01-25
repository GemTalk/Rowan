Class {
	#name : 'RwResolvedProjectComponentVisitorV2',
	#superclass : 'RwAbstractProjectComponentVisitorV2',
	#instVars : [
		'resolvedProject'
	],
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'instance creation' }
RwResolvedProjectComponentVisitorV2 class >> readProjectSetForResolvedProject: resolvedProject withComponentNames: componentNames groupNames: groupNames platformConditionalAttributes: platformConditionalAttributes [
	^ self new
		readProjectSetForResolvedProject: resolvedProject
		withComponentNames: componentNames
		groupNames: groupNames
		platformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'instance creation' }
RwResolvedProjectComponentVisitorV2 class >> resolvedProject: resolvedProject platformConditionalAttributes: platformConditionalAttributes groupNames: groupNames [
	^ self new
		platformConditionalAttributes: platformConditionalAttributes;
		groupNames: groupNames;
		resolvedProject: resolvedProject;
		yourself
]

{ #category : 'private' }
RwResolvedProjectComponentVisitorV2 >> _addPackageNames: somePackageNames for: aComponent [

	self projectDefinition addPackages: somePackageNames forComponent: aComponent
]

{ #category : 'private' }
RwResolvedProjectComponentVisitorV2 >> _readComponentsForResolvedProject: aResolvedProject withComponentNames: componentNamesToRead groupNames: aGroupNames platformConditionalAttributes: aPlatformConditionalAttributes [
	| theComponentNames theGroupNames |
	resolvedProject := aResolvedProject.
	platformConditionalAttributes := aPlatformConditionalAttributes.

	resolvedProject _projectDefinition components: RwResolvedLoadComponentsV2 new.	"build new list of components based on (potentially) new list of configNames"
	resolvedProject _projectDefinition packages: Dictionary new.	"bulid new list of packages as well"
	theComponentNames := componentNamesToRead isEmpty
		ifTrue: [ resolvedProject componentNames ]
		ifFalse: [ componentNamesToRead ].
	theGroupNames := aGroupNames isEmpty
		ifTrue: [ resolvedProject groupNames ]
		ifFalse: [ aGroupNames ].
	^ self _visitComponents: theComponentNames groupNames: theGroupNames
]

{ #category : 'private' }
RwResolvedProjectComponentVisitorV2 >> _visitComponents: componentNamesToRead groupNames: aGroupNames [
	| projectName componentDirectory projectsDirectory |
	groupNames := aGroupNames.

	projectName := resolvedProject projectAlias.
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
			component := RwBasicProjectLoadComponentV2
				fromComponentsDirectory: componentDirectory
				named: componentName.
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

{ #category : 'reading' }
RwResolvedProjectComponentVisitorV2 >> readProjectSetForResolvedProject: aResolvedProject withComponentNames: componentNamesToRead groupNames: theGroupNames platformConditionalAttributes: thePlatformConditionalAttributes [
	| projectSetDefinition visitor projectVisitorQueue projectVisitedQueue |
	projectSetDefinition := RwProjectSetDefinition new.
	projectVisitedQueue := {}.
	projectVisitorQueue := {{aResolvedProject.
	componentNamesToRead.
	theGroupNames}}.
	[ projectVisitorQueue isEmpty ]
		whileFalse: [ 
			| nextDefArray rp cn gn |
			nextDefArray := projectVisitorQueue removeFirst.
			rp := nextDefArray at: 1.
			cn := nextDefArray at: 2.
			gn := nextDefArray at: 3.
			visitor := self
				_readComponentsForResolvedProject: rp
				withComponentNames: cn
				groupNames: gn
				platformConditionalAttributes: thePlatformConditionalAttributes.
			rp
				projectDefinitionSourceProperty:
					RwLoadedProject _projectDiskDefinitionSourceValue.
			visitor visitedComponents
				keysAndValuesDo: [ :cName :cmp | rp components _addComponent: cmp ].
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
							(loadSpec componentNames).
							(loadSpec groupNames)} ] ].
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
