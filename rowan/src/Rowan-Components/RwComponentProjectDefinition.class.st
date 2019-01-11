Class {
	#name : 'RwComponentProjectDefinition',
	#superclass : 'RwProjectDefinition',
	#instVars : [
		'components'
	],
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwComponentProjectDefinition class >> newForProjectReference: aRwProjectReferenceDefintion [

	^ self
		withProperties:
			(Dictionary new
				add: 'name' -> aRwProjectReferenceDefintion projectAlias;
				add: 'projectRef' -> aRwProjectReferenceDefintion;
				yourself)
		packageDefinitions: Dictionary new
		componentDefinitions: Dictionary new
]

{ #category : 'instance creation' }
RwComponentProjectDefinition class >> projectName: projectName configurationNames: configurationNames groupNames: groupNames useGit: useGit projectUrl: projectUrl comment: comment [


	^ self newForProjectReference: (RwProjectReferenceDefinition
		projectName: projectName 
			configurationNames: configurationNames 
			groupNames: groupNames 
			useGit: useGit 
			projectUrl: projectUrl 
			comment: comment)
]

{ #category : 'instance creation' }
RwComponentProjectDefinition class >> projectName: projectName configurationNames: configurationNames groupNames: groupNames  useGit: useGit projectUrl: projectUrl comment: comment projectHome: projectHomeFileReferenceOrString committish: committish committishType: committishType configsPath: configsPath packagesPath: packagesPath projectsPath: projectsPath specsPath: specsPath [


	^ self newForProjectReference: (RwProjectReferenceDefinition
		projectName: projectName 
			configurationNames: configurationNames 
			groupNames: groupNames  
			useGit: useGit 
			projectUrl: projectUrl 
			comment: comment 
			projectHome: projectHomeFileReferenceOrString 
			committish: committish 
			committishType: committishType 
			configsPath: configsPath 
			packagesPath: packagesPath 
			projectsPath: projectsPath 
			specsPath: specsPath)
]

{ #category : 'instance creation' }
RwComponentProjectDefinition class >> projectName: projectName configurationNames: configurationNames groupNames: groupNames useGit: useGit projectUrl: projectUrl committish: committish committishType: committishType comment: comment [

	^ self newForProjectReference: (RwProjectReferenceDefinition
		projectName: projectName 
			configurationNames: configurationNames 
			groupNames: groupNames 
			useGit: useGit 
			projectUrl: projectUrl 
			committish: committish 
			committishType: committishType 
			comment: comment)
]

{ #category : 'instance creation' }
RwComponentProjectDefinition class >> withProperties: properties packageDefinitions: packageDefinitions componentDefinitions: componentDefinitions [

	^ self basicNew
		properties: properties;
		packages: packageDefinitions;
		components: componentDefinitions;
		yourself
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> components [

	^ components
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> components: aComponentDefinitionDictionary [

	components := aComponentDefinitionDictionary
]

{ #category : 'properties' }
RwComponentProjectDefinition >> configsPath [

	^ self projectRef configsPath
]

{ #category : 'properties' }
RwComponentProjectDefinition >> configsPath: aDirectoryPathString [

	self specification configsPath: aDirectoryPathString
]

{ #category : 'properties' }
RwComponentProjectDefinition >> configsRoot [
	^ self projectRef configsRoot
]

{ #category : 'actions' }
RwComponentProjectDefinition >> create [
	"write the project structure to disk, if it doesn't already exist"

	Rowan projectTools create
		createComponentProject: self.
]

{ #category : 'properties' }
RwComponentProjectDefinition >> defaultConfigurationNames [

	^ self propertyAt: 'defaultConfigurationNames' ifAbsent: [ self projectRef configurationNames ]
]

{ #category : 'properties' }
RwComponentProjectDefinition >> defaultGroupNames [

	^ self propertyAt: 'defaultGroupNames' ifAbsent: [ self projectRef groupNames ]
]

{ #category : 'exporting' }
RwComponentProjectDefinition >> exportConfiguration [

	self configurationNames do: [:configName|
		| config |
		config := RwComponentLoadConfiguration new
			name: configName;
			projectName: self projectName;
			yourself.
		self groupNames
			do: [:groupName | config addDefinedGroupName: groupName ].
		self projectNames
			do: [:projectName | config addProjectName: projectName ].
		self groupNames
			do: [:groupName |
				config 
					conditionalPackagesAtConditions: { 'common' } 
					andGroup: groupName
					addPackageNames: { } ].
		config exportToUrl: 'file:',  self configsRoot pathString, '/' ].
]

{ #category : 'exporting' }
RwComponentProjectDefinition >> exportPackages [

	| root |
	root := self packagesRoot.
	(root /  'properties', 'st') 
		writeStreamDo: [ :fileStream | fileStream nextPutAll: '{ #format : ' , self packageFormat printString , '}' ].
]

{ #category : 'exporting' }
RwComponentProjectDefinition >> exportProjects [

	self projectNames do: [:projectName |
		self error: 'not yet implemented' ].
]

{ #category : 'properties' }
RwComponentProjectDefinition >> loadedConfigurationNames [

	^ self propertyAt: 'loadedConfigurationNames' ifAbsent: [ ]
]

{ #category : 'properties' }
RwComponentProjectDefinition >> loadedGroupNames [

	^ self propertyAt: 'loadedGroupNames' ifAbsent: [ ]
]

{ #category : 'properties' }
RwComponentProjectDefinition >> packagesPath [

	^ self projectRef packagesPath
]

{ #category : 'properties' }
RwComponentProjectDefinition >> packagesPath: aString [

	^ self projectRef packagesPath: aString
]

{ #category : 'properties' }
RwComponentProjectDefinition >> packagesRoot [

	^ self projectRef packagesRoot
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> projectHome [

	^ self projectRef projectHome
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> projectHome: aProjectHomeReferenceOrString [

	^ self projectRef projectHome: aProjectHomeReferenceOrString
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> projectRef [

	^ self propertyAt: 'projectRef' ifAbsent: [ ]
]

{ #category : 'properties' }
RwComponentProjectDefinition >> projectsPath [

	^ self projectRef projectsPath
]

{ #category : 'properties' }
RwComponentProjectDefinition >> projectsPath: aString [

	^ self projectRef projectsPath: aString
]

{ #category : 'properties' }
RwComponentProjectDefinition >> projectsRoot [

	^ self projectRef projectsRoot
]

{ #category : 'tool api' }
RwComponentProjectDefinition >> readProjectSet [

	^ Rowan projectTools read  readProjectSetForComponentProjectDefinition: self
]

{ #category : 'properties' }
RwComponentProjectDefinition >> repoPath [

	"use packagesPath"
	^ self shouldNotImplement: #repoPath
]

{ #category : 'properties' }
RwComponentProjectDefinition >> repoPath: astring [

	"use packagesPath:"
	^ self shouldNotImplement: #repoPath:
]

{ #category : 'properties' }
RwComponentProjectDefinition >> repositoryRoot [

	^ self projectRef repositoryRoot
]

{ #category : 'temporary compat' }
RwComponentProjectDefinition >> specification [

self deprecated: 'temporary method ... simulating RwSpecification api'.

	^ self
]

{ #category : 'properties' }
RwComponentProjectDefinition >> specsPath [

	^ self projectRef specsPath
]

{ #category : 'properties' }
RwComponentProjectDefinition >> specsPath: aDirectoryPathString [

	self projectRef specsPath: aDirectoryPathString
]

{ #category : 'properties' }
RwComponentProjectDefinition >> specsRoot [

	^ self projectRef specsRoot
]
