Class {
	#name : 'RwStrawmanProjectV2',
	#superclass : 'RwAbstractResolvedObjectV2',
	#instVars : [
		'loadComponents',
		'comment'
	],
	#category : 'Rowan-Strawman1'
}

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
	loadComponents := RwResolvedLoadComponentsV2 new
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
RwStrawmanProjectV2 >> vastVaPrequisitesForPackageNamed: packageName [
	^ self _loadComponents vastVaPrequisitesForPackageNamed: packageName
]
