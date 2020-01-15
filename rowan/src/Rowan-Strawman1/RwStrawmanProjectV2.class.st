Class {
	#name : 'RwStrawmanProjectV2',
	#superclass : 'RwAbstractResolvedObjectV2',
	#instVars : [
		'loadComponents',
		'comment',
		'packages'
	],
	#category : 'Rowan-Strawman1'
}

{ #category : 'instance creation' }
RwStrawmanProjectV2 class >> basicLoadSpecification: anRwLoadSpecificationV2 [
	"resolve ensures that the project directory already exists on disk (cloned for git projects) or created on disk for new projects
		answer  the project definition specified by the receiver and any dependent projects"

	"if the project directory already exists on disk, then read the project definition(s) from disk"

	| loadSpecification |
	loadSpecification := anRwLoadSpecificationV2 copy.
	^ self new
		_loadSpecification: loadSpecification;
		yourself
]

{ #category : 'instance creation' }
RwStrawmanProjectV2 class >> loadSpecification: anRwLoadSpecificationV2 [
	"resolve ensures that the project directory already exists on disk (cloned for git projects) or created on disk for new projects
		answer  the project definition specified by the receiver and any dependent projects"

	"if the project directory already exists on disk, then read the project definition(s) from disk"

	^ self
		loadSpecification: anRwLoadSpecificationV2
		platformAttributes: self platformConditionalAttributes
]

{ #category : 'instance creation' }
RwStrawmanProjectV2 class >> loadSpecification: anRwLoadSpecificationV2 platformAttributes: platformAttributes [
	"resolve ensures that the project directory already exists on disk (cloned for git projects) or created on disk for new projects
		answer  the project definition specified by the receiver and any dependent projects"

	"if the project directory already exists on disk, then read the project definition(s) from disk"

	^ (self basicLoadSpecification: anRwLoadSpecificationV2)
		resolve: platformAttributes
]

{ #category : 'accessing' }
RwStrawmanProjectV2 class >> platformConditionalAttributes [

	^ {'common'}
]

{ #category : 'private' }
RwStrawmanProjectV2 >> _checkProjectDirectoryStructure [
	"answer true if the basic project directory structure is present"

	self projectRoots
		do: [ :fileRef | 
			fileRef exists
				ifFalse: [ ^ false ] ].
	^ true
]

{ #category : 'private' }
RwStrawmanProjectV2 >> _loadComponents [
	"load component should not be accessed directly -- Rowan private state"

	^ loadComponents
]

{ #category : 'private' }
RwStrawmanProjectV2 >> _loadComponents: aRwResolvedLoadComponentsV2 [
	"load component should not be accessed directly -- Rowan private state"

	loadComponents := aRwResolvedLoadComponentsV2
]

{ #category : 'private' }
RwStrawmanProjectV2 >> _projectRepository [
	^ projectRepository
		ifNil: [ 
			projectRepository := RwStrawmanDiskRepositoryDefinition
				newNamed: self projectAlias
				projectsHome: self projectsHome
				repositoryUrl: self diskUrl ]
]

{ #category : 'accessing' }
RwStrawmanProjectV2 >> addPackagesNamed: packageNames toComponentNamed: aComponentName [
	self _loadComponents
		addPackagesNamed: packageNames
		toComponentNamed: aComponentName
]

{ #category : 'accessing' }
RwStrawmanProjectV2 >> addSimpleComponentNamed: aComponentName condition: condition groupName: groupName comment: commentString [
	self _loadSpecification componentNames add: aComponentName.
	^ self _loadComponents
		addSimpleComponentNamed: aComponentName
		condition: condition
		groupName: groupName
		comment: commentString
]

{ #category : 'accessing' }
RwStrawmanProjectV2 >> comment [
	^ comment
]

{ #category : 'accessing' }
RwStrawmanProjectV2 >> comment: aString [
	comment := aString
]

{ #category : 'accessing' }
RwStrawmanProjectV2 >> diskUrl [
	^ self _loadSpecification diskUrl
]

{ #category : 'actions' }
RwStrawmanProjectV2 >> export [
	self repositoryRoot exists
		ifFalse: [ self error: 'internal error - expected the repository root to be created' ].
	self projectRoots do: [ :path | path ensureCreateDirectory ].
	self
		exportProjectSpecification;
		exportComponents;
		exportPackages
]

{ #category : 'exporting' }
RwStrawmanProjectV2 >> exportComponents [
	self _loadComponents
		do: [ :component | component exportToUrl: 'file:' , self componentsRoot pathString , '/' ].
	self _loadComponents isEmpty
		ifTrue: [ 
			"add README.md as placeholder to ensure that the directory is preserved by git"
			self componentsRoot / 'README' , 'md' writeStreamDo: [ :fileStream |  ] ]
]

{ #category : 'exporting' }
RwStrawmanProjectV2 >> exportLoadSpecification [
	self _loadSpecification exportTo: self specsRoot
]

{ #category : 'exporting' }
RwStrawmanProjectV2 >> exportPackages [
	"not in scope for strawman"
]

{ #category : 'exporting' }
RwStrawmanProjectV2 >> exportProjectSpecification [
	self _projectSpecification exportTo: self repositoryRoot
]

{ #category : 'initiailization' }
RwStrawmanProjectV2 >> initialize [
	super initialize.
	loadComponents := RwResolvedLoadComponentsV2 new.
	packages := Dictionary new
]

{ #category : 'accessing' }
RwStrawmanProjectV2 >> packages [
	^ packages
]

{ #category : 'accessing' }
RwStrawmanProjectV2 >> platformConditionalAttributes [

	^ self class platformConditionalAttributes
]

{ #category : 'actions' }
RwStrawmanProjectV2 >> read [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	^ self readComponentNames: self componentNames groupNames: self groupNames
]

{ #category : 'actions' }
RwStrawmanProjectV2 >> read: platformConditionalAttributes [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	^ self
		readComponentNames: self componentNames
		groupNames: self groupNames
		platformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'private' }
RwStrawmanProjectV2 >> readComponentNames: componentNames groupNames: groupNames platformConditionalAttributes: platformConditionalAttributes [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	| visitor packageDict |
	visitor := RwIndependentComponentVisitorV2 new
		platformConditionalAttributes: platformConditionalAttributes;
		groupNames: groupNames;
		yourself.

	componentNames
		do: [ :componentName | 
			| component url |
			url := 'file:' , (self componentsRoot / componentName , 'ston') pathString.
			component := RwAbstractProjectLoadComponentV2 fromUrl: url.
			component projectName: self projectAlias.

			visitor visit: component ].

	self _loadComponents components: visitor visitedComponents.

	visitor packageNames
		do: [ :packageName | 
			packageDict := packages
				at: packageName
				put:
					(Dictionary new
						at: #'name' put: packageName;
						at: #'properties' put: Dictionary new;
						at: #'definitions' put: Dictionary new;
						yourself).	"to be populated by VAST tonel reader"
			(self vastVaPrequisitesForPackageNamed: packageName)
				ifNotNil: [ :prereqs | (packageDict at: #'properties') at: 'vaPrerequisites' put: prereqs ].
			(self vastVaSubApplicationsForPackageNamed: packageName)
				ifNotNil: [ :subApplications | (packageDict at: #'properties') at: 'vaSubApplications' put: subApplications ] ]
]

{ #category : 'actions' }
RwStrawmanProjectV2 >> resolve [
	"resolve the projectSpecation (clone remote repo or connect to existing repo on disk) and read 
		project from disk, if project is present on disk"

	self _projectRepository resolve
		ifTrue: [ 
			self _projectRepository checkAndUpdateRepositoryRevision: self.
			self _checkProjectDirectoryStructure
				ifTrue: [ 
					"update project definition from disk"
					self read ] ]
]

{ #category : 'actions' }
RwStrawmanProjectV2 >> resolve: platformConfigurationAttributes [
	"resolve the projectSpecation (clone remote repo or connect to existing repo on disk) and read 
		project from disk, if project is present on disk"

	self _projectRepository resolve
		ifTrue: [ 
			self _projectRepository checkAndUpdateRepositoryRevision: self.
			self _checkProjectDirectoryStructure
				ifTrue: [ 
					"update project definition from disk"
					self read: platformConfigurationAttributes ] ]
]

{ #category : 'package properties' }
RwStrawmanProjectV2 >> vastSetVaPrequisitesTo: prereqArray forPackageNamed: packageName [
	self _loadComponents
		vastSetVaPrequisitesTo: prereqArray
		forPackageNamed: packageName
]

{ #category : 'package properties' }
RwStrawmanProjectV2 >> vastSetVaSubApplicationsTo: subApplicationsArray forPackageNamed: packageName [
	self _loadComponents
		vastSetVaSubApplicationsTo: subApplicationsArray
		forPackageNamed: packageName
]

{ #category : 'package properties' }
RwStrawmanProjectV2 >> vastVaPrequisitesForPackageNamed: packageName [
	^ self _loadComponents vastVaPrequisitesForPackageNamed: packageName ifAbsent: []
]

{ #category : 'package properties' }
RwStrawmanProjectV2 >> vastVaSubApplicationsForPackageNamed: packageName [
	^ self _loadComponents vastVaSubApplicationsForPackageNamed: packageName ifAbsent: []
]
