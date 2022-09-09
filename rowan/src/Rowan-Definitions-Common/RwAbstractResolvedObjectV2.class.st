Class {
	#name : 'RwAbstractResolvedObjectV2',
	#superclass : 'Object',
	#instVars : [
		'projectRepository',
		'loadSpecification',
		'projectSpecification'
	],
	#category : 'Rowan-Definitions-Common'
}

{ #category : 'instance creation' }
RwAbstractResolvedObjectV2 class >> new [
	"Create a new project that uses Rowan v3 project spec and component classes"

	^ self basicNew initialize
]

{ #category : 'instance creation' }
RwAbstractResolvedObjectV2 class >> newV2 [
	"Create a new project that uses project spec and component classes that are compatible with Rowan v2"

	^ self basicNew initializeV2
]

{ #category : 'private' }
RwAbstractResolvedObjectV2 >> _gemstoneAllUsersName [
	^ self loadSpecification _gemstoneAllUsersName
]

{ #category : 'private' }
RwAbstractResolvedObjectV2 >> _loadSpecification: object [
	"load specification should not be accessed directly -- Rowan private state"

	loadSpecification := object
]

{ #category : 'private' }
RwAbstractResolvedObjectV2 >> _projectRepository [
	self subclassResponsibility: #'_projectRepository'
]

{ #category : 'private' }
RwAbstractResolvedObjectV2 >> _projectRepository: object [
	"project repository should not be accessed directly -- Rowan private state"

	projectRepository := object
]

{ #category : 'private' }
RwAbstractResolvedObjectV2 >> _projectSpecification [
	"project specification should not be accessed directly -- Rowan private state"

	"lazy initialization, since project specification must be read from disk during resolve
		if it is present on disk"

	^ projectSpecification
		ifNil: [ 
			| projectSpecFileRef pName |
			projectSpecFileRef := self repositoryRoot / self projectSpecFile.
			pName := self projectName.	"projectSpecification is involved in default logic for projectName"
			projectSpecification := projectSpecFileRef exists
				ifTrue: [ RwSpecification fromFile: projectSpecFileRef ]
				ifFalse: [ 
					Rowan projectTools trace trace: '--- creating new project spec. The project spec file ', projectSpecFileRef pathString printString, ' does not exist'.
					RwProjectSpecificationV2 new ].
			projectSpecification
				projectName: pName;
				yourself ]
]

{ #category : 'private' }
RwAbstractResolvedObjectV2 >> _projectSpecification: object [
	"project specification should not be accessed directly -- Rowan private state"

	projectSpecification := object
]

{ #category : 'private' }
RwAbstractResolvedObjectV2 >> _validate [
	"ensure that the receiver's specifications contain valid information ... 
		the specs must be able to stand on their own, when they are written to disk, so there is a 
		responsiblity for them to have valid data"

	^ self _validate: self conditionalAttributes
]

{ #category : 'private' }
RwAbstractResolvedObjectV2 >> _validate: conditionalAttributes [
	"ensure that the receiver's specifications contain valid information ... 
		the specs must be able to stand on their, when they are written to disk, so there is a 
		responsiblity for them to have valid data"

	self _projectSpecification _validate.
	self loadSpecification _validate.
	^ true
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> comment [
	^ self subclassResponsibility: #'comment'
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> comment: aString [
	^ self subclassResponsibility: #'comment:'
]

{ #category : 'project specification' }
RwAbstractResolvedObjectV2 >> componentNames [
	"list of component names from the load specification used to load the project "

	^ self loadSpecification componentNames
]

{ #category : 'project specification' }
RwAbstractResolvedObjectV2 >> componentNames: anArray [
	self loadSpecification componentNames: anArray
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> componentsRoot [
	^ self repositoryRoot / self _projectSpecification componentsPath
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> conditionalAttributes [

	^ self platformConditionalAttributes, self customConditionalAttributes
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> customConditionalAttributes [

	^ self loadSpecification customConditionalAttributes
]

{ #category : 'exporting' }
RwAbstractResolvedObjectV2 >> exportLoadSpecification [
	self loadSpecification exportTo: self specsRoot
]

{ #category : 'exporting' }
RwAbstractResolvedObjectV2 >> exportProjectSpecification [
	self _projectSpecification exportTo: self repositoryRoot
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> gitRoot [
	^ self _projectRepository gitRoot
]

{ #category : 'project specification' }
RwAbstractResolvedObjectV2 >> groupNames: anArray [
	self loadSpecification groupNames: anArray
]

{ #category : 'initialization' }
RwAbstractResolvedObjectV2 >> initialize [
	"Create a new project that uses Rowan v3 project spec and component classes"

	projectSpecification := RwProjectSpecificationV3 new.
	loadSpecification := RwLoadSpecificationV2 new.
]

{ #category : 'initialization' }
RwAbstractResolvedObjectV2 >> initializeV2 [
	"Create a new project that uses project spec and component classes that are compatible with Rowan v2"

	projectSpecification := RwProjectSpecificationV2 new.
	loadSpecification := RwLoadSpecificationV2 new.
]

{ #category : 'testing' }
RwAbstractResolvedObjectV2 >> isStrict [
	^ self loadSpecification isStrict
]

{ #category : 'private' }
RwAbstractResolvedObjectV2 >> loadSpecification [
	^ loadSpecification
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> packagesRoot [
	^ self repositoryRoot / self _projectSpecification packagesPath
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> platformConditionalAttributes [

	^ Rowan platformConditionalAttributes
]

{ #category : 'copying' }
RwAbstractResolvedObjectV2 >> postCopy [
	super postCopy.
	projectRepository := projectRepository copy.
	loadSpecification := loadSpecification copy.
	projectSpecification := projectSpecification copy
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectAlias [
	^ self loadSpecification projectAlias ifNil: [ self projectName ]
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectAlias: aString [
	self loadSpecification projectAlias: aString
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectName [
	projectSpecification
		ifNil: [ 
			loadSpecification ifNil: [ ^ self _projectDefinition projectName ].
			^ self loadSpecification projectName ].
	^ self _projectSpecification projectName
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectName: aString [
	self loadSpecification projectName: aString.
	self _projectSpecification projectName: aString
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectRoots [
	^ {(self componentsRoot).
	(self packagesRoot).
	(self specsRoot).
	(self projectsRoot)}
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectsHome [
	^ self loadSpecification projectsHome
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectsHome: aProjectHomeReferenceOrString [
	"keep load spec and project repository in sync with respect to projects home"

	self loadSpecification projectsHome: aProjectHomeReferenceOrString.
	projectRepository
		ifNotNil: [ self _projectRepository projectsHome: aProjectHomeReferenceOrString ]
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectSpecFile [
	"relative path to the project specification file - default: rowan/project.ston"

	^ self loadSpecification projectSpecFile
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectSpecFile: relativePathString [
	"relative path to the project specification file - default: rowan/project.ston"

	| path |
	path := Path from: relativePathString .
	self _projectSpecification projectSpecPath: path parent pathString.
	self _projectSpecification specName: path base.
	self loadSpecification projectSpecFile: relativePathString
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectSpecPath: aStringOrNil [
	"relative path to the project specification file - default rowan/project.ston (relative to repository root)"

	"nil value indicates projectSpec file is in repository root directory"

	| projectSpecFileName |
	self _projectSpecification projectSpecPath: aStringOrNil.
	projectSpecFileName := self _projectSpecification specName , '.ston'.
	aStringOrNil
		ifNotNil: [ 
			projectSpecFileName := ((Path from: aStringOrNil) / projectSpecFileName)
				pathString ].
	self loadSpecification projectSpecFile: projectSpecFileName
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectsRoot [
	^ self repositoryRoot / self _projectSpecification projectsPath
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> repositoryRoot [
	^ self _projectRepository repositoryRoot
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> requiredProjectRoots [

	^ {(self componentsRoot).
	(self packagesRoot).
	(self specsRoot).
	(self projectsRoot)}
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> specsRoot [
	^ self repositoryRoot / self _projectSpecification specsPath
]
