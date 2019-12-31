Extension { #name : 'RwPrjReadTool' }

{ #category : '*rowan-tools-extensions-components' }
RwPrjReadTool >> readConfigurationsForProjectComponentDefinition: projectDefinition withConfigurations: configNames groupNames: groupNames platformConfigurationAttributes: platformConfigurationAttributes [

	^self 
		readConfigurationsForProjectComponentDefinition: projectDefinition 
			withConfigurations: configNames 
			groupNames: groupNames 
			platformConfigurationAttributes: platformConfigurationAttributes 
			forLoad: true
]

{ #category : '*rowan-tools-extensions-components' }
RwPrjReadTool >> readConfigurationsForProjectComponentDefinition: projectComponentDefinition withConfigurations: configNames groupNames: groupNames platformConfigurationAttributes: platformConfigurationAttributes forLoad: forLoad [

	| theConfigNames theGroupNames |
	projectComponentDefinition components: Dictionary new. "build new list of components based on (potentially) new list of configNames"
	projectComponentDefinition packages: Dictionary new.	"bulid new list of packages as well"
	theConfigNames := configNames isEmpty
		ifTrue: [ projectComponentDefinition defaultConfigurationNames ]
		ifFalse: [ configNames ].
	theGroupNames := groupNames isEmpty
		ifTrue: [  projectComponentDefinition defaultGroupNames ]
		ifFalse: [ groupNames ].
	^ self 
		_visitConfigurations: RwProjectLoadComponentVisitor 
		forProjectComponentDefinition: projectComponentDefinition 
		withConfigurations: theConfigNames 
		groupNames: theGroupNames
		platformConfigurationAttributes: platformConfigurationAttributes
		forLoad: forLoad
]

{ #category : '*rowan-tools-extensions-components' }
RwPrjReadTool >> readProjectSetForComponentProjectDefinition: projectComponentDefinition [

	projectComponentDefinition loadedConfigurationNames
		ifNotNil: [:configNames |
			"project has been loaded previously, use the loaded config and group names"
			^ self 
					readProjectSetForComponentProjectDefinition: projectComponentDefinition
					withConfigurations: configNames
					groupNames: projectComponentDefinition loadedGroupNames ].
	"project has not been loaded previously use the default config and group names from the spec"
	^ self 
			readProjectSetForComponentProjectDefinition: projectComponentDefinition
			withConfigurations: projectComponentDefinition defaultConfigurationNames
			groupNames: projectComponentDefinition defaultGroupNames
]

{ #category : '*rowan-tools-extensions-components' }
RwPrjReadTool >> readProjectSetForComponentProjectDefinition: projectComponentDefinition withConfigurations: configNames groupNames: groupNames [

	"read packages and project metadata into projectComponentDefinition ... return a project definition
		set that contains projectComponentDefinition and any dependent projects"

	^ self 
		readProjectSetForComponentProjectDefinition: projectComponentDefinition 
			withConfigurations: configNames 
			groupNames: groupNames 
			platformConfigurationAttributes: Rowan platformConditionalAttributes
]

{ #category : '*rowan-tools-extensions-components' }
RwPrjReadTool >> readProjectSetForComponentProjectDefinition: projectComponentDefinition withConfigurations: configNames groupNames: groupNames platformConfigurationAttributes: platformConfigurationAttributes [

	| projectSetDefinition visitor projectVisitorQueue projectVisitedQueue |
	projectSetDefinition := RwProjectSetDefinition new.
	projectVisitedQueue := {}.
	projectVisitorQueue := {
		{ projectComponentDefinition . configNames . groupNames }
	}.
	[ projectVisitorQueue isEmpty ] whileFalse: [
		| nextDefArray pcd cn gn |
		nextDefArray := projectVisitorQueue removeFirst.
		pcd := nextDefArray at: 1. 
		cn := nextDefArray at: 2.
		gn := nextDefArray at: 3.
		visitor := self 
			readConfigurationsForProjectComponentDefinition: pcd 
				withConfigurations: cn 
				groupNames: gn 
				platformConfigurationAttributes: platformConfigurationAttributes.
		pcd projectDefinitionSourceProperty: RwLoadedProject _projectDiskDefinitionSourceValue.
		visitor visitedComponents keysAndValuesDo: [:cName :cmp | pcd components at: cName put: cmp ].
		projectVisitedQueue addLast: { visitor . nextDefArray  }.
		visitor projectLoadSpecs do: [:loadSpec |
			| lsd |
			lsd := loadSpec asDefinition.
			lsd projectHome: pcd projectHome.
			lsd cloneRepository.
			projectVisitorQueue addLast: {lsd . lsd loadedConfigurationNames . lsd loadedGroupNames } ] ].
	projectVisitedQueue do: [:visitedArray |
		| projectName ndf theVisitor theProjectComponentDefinition theConfigNames
			theGroupNames thePackageNames thePackageMapSpecs |
		theVisitor := visitedArray at: 1.
		ndf := visitedArray at: 2.
		theProjectComponentDefinition := ndf at: 1.
		projectName := theProjectComponentDefinition name.
		theConfigNames := ndf at: 2.
		theGroupNames := ndf at: 3.
		thePackageNames := theProjectComponentDefinition packageNames.
		theVisitor 
			ifNotNil: [ thePackageMapSpecs := theVisitor packageMapSpecs ]
			ifNil: [ thePackageMapSpecs := Dictionary new ].	
		theProjectComponentDefinition readPackageNames: thePackageNames.
		projectSetDefinition addProject: theProjectComponentDefinition.
		((projectSetDefinition properties at: 'loadedProjectInfo' ifAbsentPut: [Dictionary new])
			at: projectName ifAbsentPut: [ Dictionary new ])
				at: 'loadedConfigurationNames' put: theConfigNames;
				at: 'loadedGroupNames' put: theGroupNames;
				at: 'packageMapSpecs' put: thePackageMapSpecs ].
	^ projectSetDefinition
]
