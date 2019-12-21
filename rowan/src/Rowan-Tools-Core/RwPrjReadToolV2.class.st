Class {
	#name : 'RwPrjReadToolV2',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'private' }
RwPrjReadToolV2 >> _visitComponents: visitorClass forResolvedProject: resolvedProject withComponentNames: componentNames groupNames: groupNames platformConfigurationAttributes: platformConfigurationAttributes forLoad: forLoad [
	| projectName componentDirectory projectsDirectory visitor |
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
	visitor := visitorClass new
		platformAttributes: platformConfigurationAttributes;
		groupNames: groupNames;
		componentsRoot: componentDirectory;
		projectsRoot: projectsDirectory;
		resolvedProject: resolvedProject;
		yourself.
	componentNames
		do: [ :componentName | 
			| component url |
			url := 'file:' , (componentDirectory / componentName , 'ston') pathString.
			component := RwAbstractProjectConfiguration fromUrl: url.
			component projectName: projectName.
			forLoad
				ifTrue: [ 
					"expect all component names to represent loadable components - throw error if a nested component is encountered"
					visitor visit: component ]
				ifFalse: [ 
					"resolve component names without respect to whether or not the component is loadable"
					visitor visitNested: component ] ].
	^ visitor
]

{ #category : 'read resolved projects' }
RwPrjReadToolV2 >> readComponentsForResolvedProject: resolvedProject withComponentNames: componentNames groupNames: groupNames platformConfigurationAttributes: platformConfigurationAttributes [
	^ self
		readComponentsForResolvedProject: resolvedProject
		withComponentNames: componentNames
		groupNames: groupNames
		platformConfigurationAttributes: platformConfigurationAttributes
		forLoad: true
]

{ #category : 'read resolved projects' }
RwPrjReadToolV2 >> readComponentsForResolvedProject: resolvedProject withComponentNames: componentNames groupNames: groupNames platformConfigurationAttributes: platformConfigurationAttributes forLoad: forLoad [
	| theComponentNames theGroupNames |
	resolvedProject _projectDefinition components: Dictionary new.	"build new list of components based on (potentially) new list of configNames"
	resolvedProject _projectDefinition packages: Dictionary new.	"bulid new list of packages as well"
	theComponentNames := componentNames isEmpty
		ifTrue: [ resolvedProject componentNames ]
		ifFalse: [ componentNames ].
	theGroupNames := groupNames isEmpty
		ifTrue: [ resolvedProject groupNames ]
		ifFalse: [ groupNames ].
	^ self
		_visitComponents: RwProjectLoadComponentVisitorV2
		forResolvedProject: resolvedProject
		withComponentNames: theComponentNames
		groupNames: theGroupNames
		platformConfigurationAttributes: platformConfigurationAttributes
		forLoad: forLoad
]

{ #category : 'read resolved projects' }
RwPrjReadToolV2 >> readProjectSetForResolvedProject: resolvedProject withComponentNames: componentNames groupNames: groupNames [
	"read packages and project metadata into projectDefinition ... return a project definition
		set that contains projectDefinition and any dependent projects"

	^ self
		readProjectSetForResolvedProject: resolvedProject
		withComponentNames: componentNames
		groupNames: groupNames
		platformConfigurationAttributes: Rowan platformConfigurationAttributes
]

{ #category : 'read resolved projects' }
RwPrjReadToolV2 >> readProjectSetForResolvedProject: resolvedProject withComponentNames: componentNames groupNames: groupNames platformConfigurationAttributes: platformConfigurationAttributes [
	| projectSetDefinition visitor projectVisitorQueue projectVisitedQueue |
	projectSetDefinition := RwProjectSetDefinition new.
	projectVisitedQueue := {}.
	projectVisitorQueue := {{resolvedProject.
	componentNames.
	groupNames}}.
	[ projectVisitorQueue isEmpty ]
		whileFalse: [ 
			| nextDefArray rp cn gn |
			nextDefArray := projectVisitorQueue removeFirst.
			rp := nextDefArray at: 1.
			cn := nextDefArray at: 2.
			gn := nextDefArray at: 3.
			visitor := self
				readComponentsForResolvedProject: rp
				withComponentNames: cn
				groupNames: gn
				platformConfigurationAttributes: platformConfigurationAttributes.
			rp
				projectDefinitionSourceProperty:
					RwLoadedProject _projectDiskDefinitionSourceValue.
			visitor visitedComponents
				keysAndValuesDo: [ :cName :cmp | rp components at: cName put: cmp ].
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
