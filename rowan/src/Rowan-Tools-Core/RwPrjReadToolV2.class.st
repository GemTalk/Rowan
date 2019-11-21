Class {
	#name : 'RwPrjReadToolV2',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'private' }
RwPrjReadToolV2 >> _visitComponents: visitorClass forProjectDefinition: projectDefinition withComponentNames: componentNames groupNames: groupNames platformConfigurationAttributes: platformConfigurationAttributes forLoad: forLoad [
	| projectName componentDirectory projectsDirectory visitor |
	projectName := projectDefinition name.
	componentDirectory := projectDefinition componentsRoot.
	componentDirectory exists
		ifFalse: [ 
			^ self
				error:
					'No component directory (' , componentDirectory pathString printString
						, ') found for project ' , projectName printString ].
	projectsDirectory := projectDefinition projectsRoot.
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
		projectDefinition: projectDefinition;
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

{ #category : 'read components' }
RwPrjReadToolV2 >> readComponentsForProjectDefinition: projectDefinition withComponentNames: componentNames groupNames: groupNames platformConfigurationAttributes: platformConfigurationAttributes [
	^ self
		readComponentsForProjectDefinition: projectDefinition
		withComponentNames: componentNames
		groupNames: groupNames
		platformConfigurationAttributes: platformConfigurationAttributes
		forLoad: true
]

{ #category : 'read components' }
RwPrjReadToolV2 >> readComponentsForProjectDefinition: projectDefinition withComponentNames: componentNames groupNames: groupNames platformConfigurationAttributes: platformConfigurationAttributes forLoad: forLoad [
	| theComponentNames theGroupNames |
	projectDefinition components: Dictionary new.	"build new list of components based on (potentially) new list of configNames"
	projectDefinition packages: Dictionary new.	"bulid new list of packages as well"
	theComponentNames := componentNames isEmpty
		ifTrue: [ projectDefinition componentNames ]
		ifFalse: [ componentNames ].
	theGroupNames := groupNames isEmpty
		ifTrue: [ projectDefinition groupNames ]
		ifFalse: [ groupNames ].
	^ self
		_visitComponents: RwProjectLoadComponentVisitorV2
		forProjectDefinition: projectDefinition
		withComponentNames: theComponentNames
		groupNames: theGroupNames
		platformConfigurationAttributes: platformConfigurationAttributes
		forLoad: forLoad
]

{ #category : 'read project definitions' }
RwPrjReadToolV2 >> readProjectSetForProjectDefinition: projectDefinition withComponentNames: componentNames groupNames: groupNames [
	"read packages and project metadata into projectDefinition ... return a project definition
		set that contains projectDefinition and any dependent projects"

	^ self
		readProjectSetForProjectDefinition: projectDefinition
		withComponentNames: componentNames
		groupNames: groupNames
		platformConfigurationAttributes: Rowan platformConfigurationAttributes
]

{ #category : 'read project definitions' }
RwPrjReadToolV2 >> readProjectSetForProjectDefinition: projectDefinition withComponentNames: componentNames groupNames: groupNames platformConfigurationAttributes: platformConfigurationAttributes [
	| projectSetDefinition visitor projectVisitorQueue projectVisitedQueue |
	projectSetDefinition := RwProjectSetDefinition new.
	projectVisitedQueue := {}.
	projectVisitorQueue := {{projectDefinition.
	componentNames.
	groupNames}}.
	[ projectVisitorQueue isEmpty ]
		whileFalse: [ 
			| nextDefArray pcd cn gn |
			nextDefArray := projectVisitorQueue removeFirst.
			pcd := nextDefArray at: 1.
			cn := nextDefArray at: 2.
			gn := nextDefArray at: 3.
			visitor := self
				readComponentsForProjectDefinition: pcd
				withComponentNames: cn
				groupNames: gn
				platformConfigurationAttributes: platformConfigurationAttributes.
			pcd
				projectDefinitionSourceProperty:
					RwLoadedProject _projectDiskDefinitionSourceValue.
			visitor visitedComponents
				keysAndValuesDo: [ :cName :cmp | pcd components at: cName put: cmp ].
			projectVisitedQueue
				addLast:
					{visitor.
					nextDefArray}.
			visitor projectLoadSpecs
				do: [ :loadSpec | 
					| lsd |
					self halt.	"these should be project load specs and they are to be resolved"
					lsd := loadSpec asDefinition.
					lsd projectHome: pcd projectHome.
					lsd cloneRepository.
					projectVisitorQueue
						addLast:
							{lsd.
							(lsd loadedConfigurationNames).
							(lsd loadedGroupNames)} ] ].
	projectVisitedQueue
		do: [ :visitedArray | 
			| ndf theVisitor theProjectDefinition |
			theVisitor := visitedArray at: 1.
			ndf := visitedArray at: 2.
			theProjectDefinition := ndf at: 1.
			theProjectDefinition readPackageNames: theProjectDefinition packageNames.
			projectSetDefinition addProject: theProjectDefinition ].
	^ projectSetDefinition
]
