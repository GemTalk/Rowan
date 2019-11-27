Class {
	#name : 'RwResolvedProjectV2',
	#superclass : 'Object',
	#instVars : [
		'projectDefinition',
		'projectSpecification'
	],
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwResolvedProjectV2 class >> projectSpecification: anRwProjectSpecificationV2 [
	"resolve ensures that the project directory already exists on disk (cloned for git projects) or created on disk for new projects
		answer  the project definition specified by the receiver and any dependent projects"

	"if the project directory already exists on disk, then read the project definition(s) from disk"

	| projectSpecification projectDefinition |
	projectSpecification := anRwProjectSpecificationV2 copy.
	projectDefinition := RwProjectDefinitionV2 basicNew
		properties:
				(Dictionary new
						add: 'name' -> projectSpecification projectName;
						add: 'projectRef' -> RwProjectReferenceDefinitionV2 new;
						yourself);
		packages: Dictionary new;
		projectDefinitionSourceProperty: RwLoadedProject _projectSourceValueNewProject;
		projectSpecification: projectSpecification;
		yourself.
	projectDefinition := projectSpecification _resolve
		ifTrue: [ 
			"read project definition from disk"
			projectDefinition read ]
		ifFalse: [ projectDefinition ].
	^ self new
		projectDefinition: projectDefinition;
		projectSpecification: projectSpecification
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addComponentNamed: aComponentName definedGroupNames: groupNameDict comment: commentString [
	^ self projectDefinition
		addComponentNamed: aComponentName
		definedGroupNames: groupNameDict
		comment: commentString
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName [
	^ self projectDefinition
		addPackageNamed: packageName
		toComponentNamed: componentName
		withConditions: conditionArray
		andGroupName: groupName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc [
	^ self projectDefinition
		addPackageNamed: packageName
		toComponentNamed: componentName
		withConditions: conditionArray
		andGroupName: groupName
		gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addPackagesNamed: packageNames toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName [
	^ self projectDefinition
		addPackagesNamed: packageNames
		toComponentNamed: componentName
		withConditions: conditionArray
		andGroupName: groupName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> comment [
	^ self projectDefinition comment
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> comment: aString [
	self projectDefinition comment: aString
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> commit: message [
	"commit the repository associated with receiver ..."

	^ self projectDefinition commit: message
]

{ #category : 'project specification' }
RwResolvedProjectV2 >> componentNames: anArray [
	^ self projectSpecification componentNames: anArray
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> components [
	^ self projectDefinition components
]

{ #category : 'project specification' }
RwResolvedProjectV2 >> componentsRoot [
	^ self projectSpecification componentsRoot
]

{ #category : 'actions' }
RwResolvedProjectV2 >> export [
	Rowan projectTools writeV2 writeResolvedProject: self
]

{ #category : 'exporting' }
RwResolvedProjectV2 >> exportComponents [
	self components isEmpty
		ifTrue: [ self addComponentNamed: self defaultComponentName comment: '' ].
	self components values
		do: [ :component | component exportToUrl: 'file:' , self componentsRoot pathString , '/' ].
	self components isEmpty
		ifTrue: [ 
			"add README.md as placeholder to ensure that the directory is preserved by git"
			self componentsRoot / 'README' , 'md' writeStreamDo: [ :fileStream |  ] ]
]

{ #category : 'exporting' }
RwResolvedProjectV2 >> exportPackages [
	self projectDefinition exportPackages
]

{ #category : 'exporting' }
RwResolvedProjectV2 >> exportProjects [
	self projectNames do: [ :projectName | self error: 'not yet implemented' ].
	self projectNames isEmpty
		ifTrue: [ 
			"add README.md as placeholder to ensure that the directory is preserved by git"
			self projectsRoot / 'README' , 'md' writeStreamDo: [ :fileStream |  ] ]
]

{ #category : 'actions' }
RwResolvedProjectV2 >> load [
	"load the receiver into the image"

	^ Rowan projectTools loadV2 loadProjectDefinition: self projectDefinition
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> packageNamed: aString [
	^ self projectDefinition packageNamed: aString
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> packageNames [
	^ self projectDefinition packageNames
]

{ #category : 'project specification' }
RwResolvedProjectV2 >> packagesRoot [
	^ self projectSpecification packagesRoot
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> projectDefinition [
	^projectDefinition
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> projectDefinition: object [
	projectDefinition := object
]

{ #category : 'project specification' }
RwResolvedProjectV2 >> projectName [
	^ self projectSpecification projectName
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> projectNames [
	"not yet implemented"

	^ #()
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> projectSpecification [
	^projectSpecification
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> projectSpecification: anRwProjectSpecificationV2 [
	projectSpecification := anRwProjectSpecificationV2
]

{ #category : 'project specification' }
RwResolvedProjectV2 >> projectsRoot [
	^ self projectSpecification projectsRoot
]

{ #category : 'project specification' }
RwResolvedProjectV2 >> repositoryRoot [
	^ self projectSpecification repositoryRoot
]

{ #category : 'project specification' }
RwResolvedProjectV2 >> specsRoot [
	^ self projectSpecification specsRoot
]
