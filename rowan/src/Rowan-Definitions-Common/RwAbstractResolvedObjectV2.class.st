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

	^ self basicNew initialize
]

{ #category : 'private' }
RwAbstractResolvedObjectV2 >> _gemstoneAllUsersName [
	^ self _loadSpecification _gemstoneAllUsersName
]

{ #category : 'private' }
RwAbstractResolvedObjectV2 >> _loadSpecification [
	"load specification should not be accessed directly -- Rowan private state"

	^ loadSpecification
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

	^ self _validate: self platformConditionalAttributes
]

{ #category : 'private' }
RwAbstractResolvedObjectV2 >> _validate: platformConfigurationAttributes [
	"ensure that the receiver's specifications contain valid information ... 
		the specs must be able to stand on their, when they are written to disk, so there is a 
		responsiblity for them to have valid data"

	self _projectSpecification _validate.
	self _loadSpecification _validate.
	^ true
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> addSimpleComponentNamed: aComponentName condition: condition comment: commentString [
	self subclassResponsibility: #'addSimpleComponentNamed:condition:comment:'
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
	^ self _loadSpecification componentNames
]

{ #category : 'project specification' }
RwAbstractResolvedObjectV2 >> componentNames: anArray [
	self _loadSpecification componentNames: anArray
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> componentsRoot [
	^ self repositoryRoot / self _projectSpecification componentsPath
]

{ #category : 'exporting' }
RwAbstractResolvedObjectV2 >> exportLoadSpecification [
	self _loadSpecification exportTo: self specsRoot
]

{ #category : 'exporting' }
RwAbstractResolvedObjectV2 >> exportProjectSpecification [
	self _projectSpecification exportTo: self repositoryRoot
]

{ #category : 'package properties' }
RwAbstractResolvedObjectV2 >> gemstoneSetSymbolDictName: symbolDictName forPackageNamed: packageName [
	self subclassResponsibility: #'gemstoneSetSymbolDictName:forPackageNamed:'
]

{ #category : 'package properties' }
RwAbstractResolvedObjectV2 >> gemstoneSymbolDictNameForPackageNamed: packageName [
	self subclassResponsibility: #'gemstoneSymbolDictNameForPackageNamed:'
]

{ #category : 'project specification' }
RwAbstractResolvedObjectV2 >> groupNames: anArray [
	self _loadSpecification groupNames: anArray
]

{ #category : 'initialization' }
RwAbstractResolvedObjectV2 >> initialize [
	"repository must be explicitly created"

	projectSpecification := RwProjectSpecificationV2 new.
	loadSpecification := RwLoadSpecificationV2 new.
]

{ #category : 'testing' }
RwAbstractResolvedObjectV2 >> isStrict [
	^ self _loadSpecification isStrict
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> packagesRoot [
	^ self repositoryRoot / self _projectSpecification packagesPath
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> platformConditionalAttributes [

	^ self _loadSpecification customConditionalAttributes
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
	^ self _loadSpecification projectAlias ifNil: [ self projectName ]
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectAlias: aString [
	self _loadSpecification projectAlias: aString
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectName [
	projectSpecification
		ifNil: [ 
			loadSpecification ifNil: [ ^ self _projectDefinition projectName ].
			^ self _loadSpecification projectName ].
	^ self _projectSpecification projectName
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectName: aString [
	self _loadSpecification projectName: aString.
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
	^ self _loadSpecification projectsHome
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectsHome: aProjectHomeReferenceOrString [
	self _loadSpecification projectsHome: aProjectHomeReferenceOrString
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectSpecFile [
	"relative path to the project specification file - default: rowan/project.ston"

	^ self _loadSpecification projectSpecFile
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectSpecFile: relativePathString [
	"relative path to the project specification file - default: rowan/project.ston"

	| path |
	path := Path from: relativePathString .
	self _projectSpecification projectSpecPath: path parent pathString.
	self _projectSpecification specName: path base.
	self _loadSpecification projectSpecFile: relativePathString
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectSpecPath: aString [
	"full path to the project specification file - default rowan/project.ston"

	self _projectSpecification projectSpecPath: aString.
	self _loadSpecification
		projectSpecFile:
			((Path from: aString) / self _projectSpecification specName , 'ston') pathString
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
RwAbstractResolvedObjectV2 >> repositoryRoot: aFileReferenceOrString [
	^ self _projectRepository repositoryRoot: aFileReferenceOrString
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
