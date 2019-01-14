Class {
	#name : 'RwComponentProjectDefinition',
	#superclass : 'RwProjectDefinition',
	#instVars : [
		'components'
	],
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwComponentProjectDefinition class >> newForDiskBasedProjectNamed: projectName [

	^ self newForProjectReference: (RwProjectReferenceDefinition
		projectName: projectName 
			useGit: false )
]

{ #category : 'instance creation' }
RwComponentProjectDefinition class >> newForGitBasedProjectNamed: projectName [

	^ self newForProjectReference: (RwProjectReferenceDefinition
		projectName: projectName 
			useGit: true )
]

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
RwComponentProjectDefinition >> addPackageNamed: packageName [

	self
		addPackageNamed: packageName 
			toComponentNamed: self defaultComponentName 
			withConditions: #( 'common' ) 
			andGroup: self defaultGroupName
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray andGroup: groupName [

	| component |
	super addPackageNamed: packageName.
	component := self 
		components at: componentName 
		ifAbsentPut: [ RwComponentLoadConfiguration newNamed:componentName for: self name ].
	component
		conditionalPackagesAtConditions: conditionArray
			andGroup: groupName
			addPackageNames: { packageName }
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

{ #category : 'accessing' }
RwComponentProjectDefinition >> defaultComponentName [

	^ self projectRef defaultComponentName
]

{ #category : 'properties' }
RwComponentProjectDefinition >> defaultConfigurationNames [

	^ self propertyAt: 'defaultConfigurationNames' ifAbsent: [ self projectRef configurationNames ]
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> defaultGroupName [

	^ self projectRef defaultGroupName
]

{ #category : 'properties' }
RwComponentProjectDefinition >> defaultGroupNames [

	^ self propertyAt: 'defaultGroupNames' ifAbsent: [ self projectRef groupNames ]
]

{ #category : 'exporting' }
RwComponentProjectDefinition >> exportComponents [

	self components values do: [:component|
		component exportToUrl: 'file:',  self configsRoot pathString, '/' ].
	self components isEmpty
		ifTrue: [
			"add README.md as placeholder to ensure that the directory is preserved by git"
			(self configsRoot /  'README', 'md') writeStreamDo: [ :fileStream | ] ]
]

{ #category : 'exporting' }
RwComponentProjectDefinition >> exportPackages [

	| projectSetDefinition visitor projectSetModification writerVisitorClass |
	(self packagesRoot /  'properties', 'st') 
		writeStreamDo: [ :fileStream | 
			fileStream nextPutAll: '{ '; 
				lf;
				tab;
				nextPutAll: '#format : ' , self packageFormat printString, ',' ;
				lf;
				tab;
				nextPutAll: '#convention : ', self packageConvention printString;
				lf;
				nextPutAll: '}';
				lf ].

"write out packages"
	writerVisitorClass := self packageFormat = 'tonel'
		ifTrue: [ RwModificationTonelWriterVisitor ]
		ifFalse: [ RwModificationFiletreeWriterVisitor ].
	projectSetDefinition:= RwProjectSetDefinition new.
	projectSetDefinition addDefinition: self.
	projectSetModification := projectSetDefinition compareAgainstBase: RwProjectSetDefinition new. "compare against empty project to write out entire project"
	visitor := writerVisitorClass new.

	visitor visit: projectSetModification.
]

{ #category : 'exporting' }
RwComponentProjectDefinition >> exportProjects [

	self projectNames do: [:projectName |
		self error: 'not yet implemented' ].
	self projectNames isEmpty
		ifTrue: [
			"add README.md as placeholder to ensure that the directory is preserved by git"
			(self projectsRoot /  'README', 'md') writeStreamDo: [ :fileStream | ] ]
]

{ #category : 'properties' }
RwComponentProjectDefinition >> loadedConfigurationNames [

	^ self propertyAt: 'loadedConfigurationNames' ifAbsent: [ ]
]

{ #category : 'properties' }
RwComponentProjectDefinition >> loadedGroupNames [

	^ self propertyAt: 'loadedGroupNames' ifAbsent: [ ]
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> packageConvention [

	^ self projectRef packageConvention
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> packageConvention: aString [

	self projectRef packageConvention: aString
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> packageFormat [

	^ self projectRef packageFormat
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> packageFormat: aString [

	self projectRef packageFormat: aString
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
RwComponentProjectDefinition >> projectNames [

	^ self projectRef projectNames
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> projectNames: anArray [

	self projectRef projectNames: anArray
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

{ #category : 'temporary compat' }
RwComponentProjectDefinition >> repoPath [

	"use packagesPath"
	^ self shouldNotImplement: #repoPath
]

{ #category : 'temporary compat' }
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
