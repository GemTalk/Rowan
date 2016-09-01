! Package: Cypress-PackageManagementTests


! Remove existing behavior from package Cypress-PackageManagementTests
!!!! This can be cleaned up when some package functionality is moved to the base system.

doit
| packageName |
packageName := 'Cypress-PackageManagementTests'.
System myUserProfile symbolList do: [:symDict |
	symDict do: [:possibleClass |
			| toRemove |
		possibleClass isBehavior ifTrue: [
			{possibleClass. possibleClass class} do: [:aClass |
				aClass category = packageName
					ifTrue: [
							"*anythingbutpackagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										(each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]])
										or: [each first ~= $*]]
					]
					ifFalse: [
							"*packagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]]]
					].
				toRemove do: [:each | aClass removeCategory: each].
			]
		]
	]
].
true.
%


! Class Declarations

doit
(CypressAbstractTest
	subclass: 'CypressFileoutWriterTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagementTests';
		comment: '';
		immediateInvariant.
true.
%

doit
(CypressAbstractTest
	subclass: 'CypressPackageManagerTest'
	instVarNames: #( currentManager repositoriesToCleanUp classesToCleanUp methodsToCleanUp categoriesToCleanUp )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagementTests';
		comment: '';
		immediateInvariant.
true.
%

doit
(CypressAbstractTest
	subclass: 'CypressReferenceTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagementTests';
		comment: '';
		immediateInvariant.
true.
%

! Class Implementation for CypressFileoutWriterTest

! ------------------- Instance methods for CypressFileoutWriterTest

category: 'utility'
method: CypressFileoutWriterTest
chunkFormatMocks

	^'" Package: Cypress-Mocks"!


" Class Declarations "!

(Object
	subclass: ''CypressMockBasic''
	instVarNames: #( name )
	classVars: #( Something )
	classInstVars: #( current )
	poolDictionaries: #())
		category: ''Cypress-Mocks-Definitions'';
		comment: ''This mock contains basic class and instance method selectors''!

" Class Implementation for CypressMockBasic"!

" ------------------- Class methods for CypressMockBasic"!

!CypressMockBasic class methodsFor: ''accessing''!
current
	^current
! !
!CypressMockBasic class methodsFor: ''accessing''!
current: anObject
	current := anObject
! !
!CypressMockBasic class methodsFor: ''initialization''!
initialize
	self current: self new
! !
" ------------------- Instance methods for CypressMockBasic"!

!CypressMockBasic methodsFor: ''accessing''!
extra
	"extra method"
! !
!CypressMockBasic methodsFor: ''initialization''!
initialize
	super initialize.
	self name: ''Unknown''
! !
!CypressMockBasic methodsFor: ''accessing''!
name
	^name
! !
!CypressMockBasic methodsFor: ''accessing''!
name: aString
	name := aString
! !
" Class Extensions "!

" Class Extension for Object"!

" ------------------- Instance methods for Object"!

!Object methodsFor: ''*Cypress-Mocks-Extensions''!
isCypressMockBasic

	^false! !
" Class initializers "!

CypressMockBasic initialize.!



" End of Package: Cypress-Mocks"!


'
%

category: 'tests'
method: CypressFileoutWriterTest
testChunkFormatFileout

	| name pkg struct writer result |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition named: name.
	struct := CypressPackageStructure fromPackage: pkg.
	writer := CypressSmalltalkFileoutWriter new
					packageStructure: struct;
					yourself.
	result := String streamContents: [:stream | writer fileOutPackageOn: stream].
	self assert: result equals: self chunkFormatMocks.
%

category: 'tests'
method: CypressFileoutWriterTest
testTopazFormatFileout

	| name pkg struct writer result |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition named: name.
	struct := CypressPackageStructure fromPackage: pkg.
	writer := CypressTopazFileoutWriter new
					packageStructure: struct;
					yourself.
	result := String streamContents: [:stream | writer fileOutPackageOn: stream].
	self assert: result equals: self topazFormatMocks.
%

category: 'utility'
method: CypressFileoutWriterTest
topazFormatMocks

	^'! Package: Cypress-Mocks


! Remove existing behavior from package Cypress-Mocks
!!!! This can be cleaned up when some package functionality is moved to the base system.

doit
| packageName |
packageName := ''Cypress-Mocks''.
System myUserProfile symbolList do: [:symDict |
	symDict do: [:possibleClass |
			| toRemove |
		possibleClass isBehavior ifTrue: [
			{possibleClass. possibleClass class} do: [:aClass |
				aClass category = packageName
					ifTrue: [
							"*anythingbutpackagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										(each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]])
										or: [each first ~= $*]]
					]
					ifFalse: [
							"*packagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]]]
					].
				toRemove do: [:each | aClass removeCategory: each].
			]
		]
	]
].
%


! Class Declarations

doit
(Object
	subclass: ''CypressMockBasic''
	instVarNames: #( name )
	classVars: #( Something )
	classInstVars: #( current )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: ''Cypress-Mocks-Definitions'';
		comment: ''This mock contains basic class and instance method selectors'';
		immediateInvariant.
%

! Class Implementation for CypressMockBasic

! ------------------- Class methods for CypressMockBasic

category: ''accessing''
set compile_env: 0
classmethod: CypressMockBasic
current
	^current
%

category: ''accessing''
set compile_env: 0
classmethod: CypressMockBasic
current: anObject
	current := anObject
%

category: ''initialization''
set compile_env: 0
classmethod: CypressMockBasic
initialize
	self current: self new
%

! ------------------- Instance methods for CypressMockBasic

category: ''accessing''
set compile_env: 0
method: CypressMockBasic
extra
	"extra method"
%

category: ''initialization''
set compile_env: 0
method: CypressMockBasic
initialize
	super initialize.
	self name: ''Unknown''
%

category: ''accessing''
set compile_env: 0
method: CypressMockBasic
name
	^name
%

category: ''accessing''
set compile_env: 0
method: CypressMockBasic
name: aString
	name := aString
%

! Class Extensions

! Class Extension for Object

! ------------------- Instance methods for Object

category: ''*Cypress-Mocks-Extensions''
set compile_env: 0
method: Object
isCypressMockBasic

	^false
%

! Class initializers 

doit
CypressMockBasic initialize.
%



! End of Package: Cypress-Mocks


'
%

! Class Implementation for CypressPackageManagerTest

! ------------------- Instance methods for CypressPackageManagerTest

category: 'private'
method: CypressPackageManagerTest
addClassNamed: className under: superClass to: aSymbolDictionary inCatgeory: aString

	| newClass |
	newClass := (superClass
				subclass: className
				instVarNames: #()
				classVars: #()
				classInstVars: #()
				poolDictionaries: #()
				inDictionary: aSymbolDictionary
				options: #()) category: aString.
	self classToCleanUp: newClass from: aSymbolDictionary.
	^newClass
%

category: 'private'
method: CypressPackageManagerTest
addMethodNamed: selector body: bodyString toClass: aClass inCategory: aString

	| newMethod |
	(aClass includesCategory: aString)
		ifFalse: [self categoryToCleanUp: aString from: aClass].
	newMethod := aClass
				compileMethod: selector , '   ' , bodyString
				dictionaries: System myUserProfile symbolList
				category: aString
				environmentId: 0.
	self methodToCleanUp: newMethod.
	^newMethod
%

category: 'private'
method: CypressPackageManagerTest
categoryToCleanUp: aSymbol from: aClass

	categoriesToCleanUp add: aSymbol -> aClass
%

category: 'private'
method: CypressPackageManagerTest
classToCleanUp: aClass from: aSymbolDictionary

	classesToCleanUp add: aClass -> aSymbolDictionary
%

category: 'private'
method: CypressPackageManagerTest
classToCleanUpNamed: className from: aSymbolDictionary

	classesToCleanUp add: (aSymbolDictionary at: className asSymbol) -> aSymbolDictionary
%

category: 'set up / teardown'
method: CypressPackageManagerTest
cleanUpCategories

	categoriesToCleanUp reverseDo: [:each | each value removeCategory: each key]
%

category: 'set up / teardown'
method: CypressPackageManagerTest
cleanUpClasses

	classesToCleanUp reverseDo: [:each | each value removeKey: each key name]
%

category: 'set up / teardown'
method: CypressPackageManagerTest
cleanUpMethods

	methodsToCleanUp
		reverseDo: [:each | each methodClass removeSelector: each selector]
%

category: 'set up / teardown'
method: CypressPackageManagerTest
cleanUpRepositories

	repositoriesToCleanUp reverseDo: 
			[:each |
			CypressFileUtilities current deleteAll: each directoryPath.
			GsFile removeServerDirectory: each directoryPath]
%

category: 'private'
method: CypressPackageManagerTest
createEmptyManager

	^CypressPackageManager2 new.
%

category: 'private'
method: CypressPackageManagerTest
createFilesFor: aDictionary
	"aDictionay is a mapping of repository file names to their expected contents."

	aDictionary keysAndValuesDo: 
			[:aPathName :fileContents |
			| filename directoryPath file |
			filename := CypressFileUtilities current localNameFrom: aPathName.
			directoryPath := aPathName copyWithoutSuffix: filename.
			CypressFileUtilities current ensureDirectoryExists: directoryPath.
			file := GsFile openWriteOnServer: aPathName.
			[file nextPutAll: fileContents] ensure: [file close]]
%

category: 'private'
method: CypressPackageManagerTest
createFileTreeCompatibleTestRepoNamed: aString

	| repo |
	repo := currentManager
				createRepositoryNamed: aString
				under: '/tmp'
				alias: aString
				schema: 'cypressft:'.
	self repositoryToCleanUp: repo.
	^repo
%

category: 'private'
method: CypressPackageManagerTest
createFileTreeReadOnlyTestRepoNamed: aString

	| repo |
	repo := currentManager
				createRepositoryNamed: aString
				under: '/tmp'
				alias: aString
				schema: 'cypressfiletree:'.
	self repositoryToCleanUp: repo.
	^repo
%

category: 'private'
method: CypressPackageManagerTest
createManagerFromImage

	^CypressPackageManager2 create.
%

category: 'private'
method: CypressPackageManagerTest
createManagerWithUnknownPackages: someNames

	| manager |
	manager := self createEmptyManager.
	someNames do: [:each | manager addUnknownPackageNamed: each].
	^manager
%

category: 'private'
method: CypressPackageManagerTest
createTestRepoNamed: aString

	| repo |
	repo := currentManager
				createRepositoryNamed: aString
				under: '/tmp'
				alias: aString
				schema: 'cypress:'.
	self repositoryToCleanUp: repo.
	^repo
%

category: 'private'
method: CypressPackageManagerTest
currentPackageInformationGroups

	^currentManager packageInformationList
		inject: Dictionary new
		into: 
			[:dict :each |
			(dict at: each class ifAbsentPut: [OrderedCollection new]) add: each.
			dict]
%

category: 'expected results'
method: CypressPackageManagerTest
cypressFormatXYPackageDirectoryStructure

	^(Dictionary new)
		at: '/tmp/CypressTestRepo/properties.ston'
			put: '{
	"alias" : "CypressTestRepo",
	"_cypress_copyright" : "This%20work%20is%20protected%20by%20copyright.%20All%20rights%20reserved.",
	"_gs_fileout" : "false",
	"_gs_format" : "Cypress",
	"_gs_strict" : "true" }';
		at: '/tmp/CypressTestRepo/X-Y.package/Object.extension/instance/isXY.st'
			put: '"
notice: This work is protected by copyright. All rights reserved.
category: *X-Y-testing
"
isXY   ^false';
		at: '/tmp/CypressTestRepo/X-Y.package/Object.extension/properties.ston'
			put: '{
	"name" : "Object" }';
		at: '/tmp/CypressTestRepo/X-Y.package/properties.ston' put: '{
	 }';
		at: '/tmp/CypressTestRepo/X-Y.package/XYClass.class/instance/stub.st'
			put: '"
notice: This work is protected by copyright. All rights reserved.
category: dummy
"
stub   ^nil';
		at: '/tmp/CypressTestRepo/X-Y.package/XYClass.class/properties.ston'
			put: '{
	"category" : "X-Y",
	"classinstvars" : [
		 ],
	"classvars" : [
		 ],
	"instvars" : [
		 ],
	"name" : "XYClass",
	"pools" : [
		 ],
	"super" : "Object" }';
		at: '/tmp/CypressTestRepo/X-Y.package/XYClass.class/README.md' put: '';
		yourself
%

category: 'expected results'
method: CypressPackageManagerTest
fileTreeFormatXYPackageDirectoryStructure

	^(Dictionary new)
		at: '/tmp/CypressTestRepo/properties.json'
			put: '{
	"alias" : "CypressTestRepo",
	"_cypress_copyright" : "This%20work%20is%20protected%20by%20copyright.%20All%20rights%20reserved.",
	"_gs_fileout" : "false",
	"_gs_format" : "FileTree",
	"_gs_strict" : "false" }';
		at: '/tmp/CypressTestRepo/X-Y.package/Object.extension/instance/isXY.st'
			put: '*X-Y-testing
isXY   ^false';
		at: '/tmp/CypressTestRepo/X-Y.package/Object.extension/properties.ston'
			put: '{
	"name" : "Object" }';
		at: '/tmp/CypressTestRepo/X-Y.package/properties.ston' put: '{
	 }';
		at: '/tmp/CypressTestRepo/X-Y.package/XYClass.class/instance/stub.st'
			put: 'dummy
stub   ^nil';
		at: '/tmp/CypressTestRepo/X-Y.package/XYClass.class/properties.ston'
			put: '{
	"category" : "X-Y",
	"classinstvars" : [
		 ],
	"classvars" : [
		 ],
	"instvars" : [
		 ],
	"name" : "XYClass",
	"pools" : [
		 ],
	"super" : "Object" }';
		at: '/tmp/CypressTestRepo/X-Y.package/XYClass.class/README.md' put: '';
		yourself
%

category: 'private'
method: CypressPackageManagerTest
methodToCleanUp: aCompiledMethod

	methodsToCleanUp add: aCompiledMethod
%

category: 'expected results'
method: CypressPackageManagerTest
readOnlyFileTreeFormatXYPackageDirectoryStructure
	"We cannot commit to a true FileTree repository, so there should be no files created."

	^(Dictionary new)
		at: '/tmp/CypressTestRepo/properties.json'
			put: '{
	"alias" : "CypressTestRepo",
	"_cypress_copyright" : "This%20work%20is%20protected%20by%20copyright.%20All%20rights%20reserved.",
	"_gs_fileout" : "false",
	"_gs_format" : "FileTree",
	"_gs_strict" : "true" }';
		yourself
%

category: 'private'
method: CypressPackageManagerTest
repositoryToCleanUp: aRepository

	repositoriesToCleanUp add: aRepository
%

category: 'set up / teardown'
method: CypressPackageManagerTest
setUp

	repositoriesToCleanUp := OrderedCollection new.
	classesToCleanUp := OrderedCollection new.
	methodsToCleanUp := OrderedCollection new.
	categoriesToCleanUp := OrderedCollection new.
%

category: 'set up / teardown'
method: CypressPackageManagerTest
tearDown

	self
		cleanUpMethods;
		cleanUpClasses;
		cleanUpCategories;
		cleanUpRepositories
%

category: 'tests'
method: CypressPackageManagerTest
testAttachingNewCypressRepositoryToUnknownImagePackages

	| repo groups old new |
	currentManager := self createManagerWithUnknownPackages: #('X' 'X-Y' 'X-Y-A' 'X-Y-B' 'X-Y-C' 'X-Z').
	groups := self currentPackageInformationGroups.
	self
		assert: currentManager knownRepositories isEmpty
			description: 'Newly created Package Manager should not know about any repositories';
		assert: currentManager packageInformationList size equals: 6;
		assert: groups size equals: 1;
		assert: groups keys asArray first equals: CypressUnknownPackageInformation.
	
	repo := self createTestRepoNamed: 'CypressTestRepo'.
	old := currentManager packageInformationNamed: 'X-Y'.
	self deny: old isKnown description: 'Newly defined packages are supposed to be Unknown'.

	currentManager assignRepository: repo to: old.
	new := currentManager packageInformationNamed: 'X-Y'.
	groups := self currentPackageInformationGroups.

	self
		assert: currentManager knownRepositories size = 1
			description: 'After assigning a repository, there should be at least one known';
		assert: (currentManager knownRepositories keys collect: [:each | each printString])
			equals: (Set with: 'cypress:///tmp/CypressTestRepo/');
		assert: currentManager packageInformationList size = 6
			description: 'There should be the same number of Package Information objects';
		assert: groups size equals: 4;
		assert: (groups at: CypressUnknownPackageInformation) size = 1
			description: 'There should have been one Package Information left as Unknown';
		assert: (groups at: CypressUnknownPackageInformation) first name
			equals: 'X-Z';
		assert: (groups at: CypressKnownPackageInformation) size = 1
			description: 'There should have been one Package Information left as Known';
		assert: (groups at: CypressKnownPackageInformation) first name
			equals: 'X-Y';
		assert: (groups at: CypressEclipsedPackageInformation) size = 3
			description: 'There should have been one Package Information left as Eclipsed';
		assert: ((groups at: CypressEclipsedPackageInformation)
					collect: [:each | each name]) asSet
			equals: (Set with: 'X-Y-A' with: 'X-Y-B' with: 'X-Y-C');
		assert: (groups at: CypressConflictingPackageInformation) size = 1
			description: 'There should have been one Package Information left as Conflicting';
		assert: (groups at: CypressConflictingPackageInformation) first name
			equals: 'X';
		assert: new repositories size = 1
			description: 'After assigning a repository to a newly known package, it should have one repository';
		assert: new repositories any 
			equals: repo;
		assert: (new digestFor: repo url)
			equals: (CypressPackageStructure named: new name, '.package') digest
%

category: 'tests'
method: CypressPackageManagerTest
testCreatingAndRetrievingNamedPackageManager

	| original retrieved key savedPackageManagers |
	key := DateAndTime now printString.
	original := CypressPackageManager2 create.
	savedPackageManagers := CypressPackageManager2 savedPackageManagers copy.
	
	[original saveAs: key.
	retrieved := CypressPackageManager2 named: key.
	self assert: original == retrieved
		description: 'a retrieved instance should be identical to the original']
			ensure: [CypressPackageManager2 removePackageManagerSavedAs: key].
	self assert: CypressPackageManager2 savedPackageManagers
		equals: savedPackageManagers
%

category: 'tests'
method: CypressPackageManagerTest
testCreatingNewCypressRepository

	| repo |
	currentManager := self createManagerFromImage.
	repo := self createTestRepoNamed: 'CypressTestRepo'.

	self
		assert: repo directoryPath
			equals: '/tmp/CypressTestRepo/';
		assert: repo packageNames isEmpty
			description: 'a new repository should be empty of packages';
		assert: repo isCodeFormatCypress
			equals: true;
		assert: repo isCodeFormatStrict
			equals: true
%

category: 'tests'
method: CypressPackageManagerTest
testCypressUrls

	| url |
	#(
		( 'cypress:/a/b/c'			'Cypress'	true		'cypress:///a/b/c'			#('a' 'b' 'c') )
		( 'cypresslax:/a/b/c'		'Cypress'	false	'cypresslax:///a/b/c'		#('a' 'b' 'c') )
		( 'cypressft:/a/b/c'		'FileTree'	false	'cypressft:///a/b/c'		#('a' 'b' 'c') )
		( 'cypressfiletree:/a/b/c'	'FileTree'	true		'cypressfiletree:///a/b/c'	#('a' 'b' 'c') )
	) do: [:tuple |
		url := CypressAbstractFileUrl absoluteFromText: tuple first.
		self
			assert: url printString equals: (tuple at: 4);
			assert: url codeFormat equals: (tuple at: 2);
			assert: url isStrict equals: (tuple at: 3);
			assert: url path equals: (tuple at: 5);
			assert: url host equals: ''
	].
	url := CypressAbstractFileUrl absoluteFromText: '/a/b/c/'.
	self
		assert: url printString equals: 'file:///a/b/c/';
		assert: url path equals: #('a' 'b' 'c' '');
		assert: url host equals: ''.
	url := CypressAbstractFileUrl absoluteFromText: 'polution:/a/b/c/'.
	self
		assert: url printString equals: 'polution:/a/b/c/';
		assert: url schemeName equals: 'polution';
		assert: url locator equals: '/a/b/c/'
%

category: 'tests'
method: CypressPackageManagerTest
testDetectingChangedImagePackage

	| old repo repoDigest newClass |
	self testAttachingNewCypressRepositoryToUnknownImagePackages.
	old := currentManager packageInformationNamed: 'X-Y'.
	repo := old repositories any.
	repoDigest := old digestFor: repo url.

	newClass := self addClassNamed: 'XYClass' under: Object to: UserGlobals inCatgeory: 'X-Y'.
	self addMethodNamed: 'stub' body: '' toClass: newClass inCategory: 'dummy'.

	self
		assert: old repositories size = 1
			description: 'There should have been only one repository for the package';
		assert: repoDigest
			equals: nil;
		assert: old imageDigest
			equals: nil;
		assert: (old updateDigestsFromImage; imageDigest) notNil
			description: 'After adding code to the package, the digest should be non-nil'
%

category: 'tests'
method: CypressPackageManagerTest
testInitialPopulationOfPackageInformation
	"A clean ab initio start up will only have Unknown packages
	 (or potential packages) based on the categories used in the image."

	currentManager := self createManagerFromImage.
	self
		assert: currentManager knownRepositories isEmpty
			description: 'Newly created Package Manager should not know about any repositories';
		assert: currentManager packageInformationList notEmpty
			description: 'It is impossible for an image to have no categorized classes';
		assert: (currentManager packageInformationList collect: [:each | each class]) asSet
			equals: (Set with: CypressUnknownPackageInformation)
%

category: 'tests'
method: CypressPackageManagerTest
testKnowingPackagesFromExistingRepository
	"The repository used here is not viable for the long run.
	 It is being used to defer creating the tooling to create a directory with
	 controlled contents, instead of creating the needed functionality."

	| repo groups packageNames old new |
	currentManager := self createManagerFromImage.
	groups := self currentPackageInformationGroups.
	repo := currentManager 
		repositoryOn: 'cypressfiletree:/export/galbadia1/users/rsargent/git/gitRepos/' asUrl
		alias: 'Test Repo - Cypress format'.
	packageNames := repo packageNames.
	self assert: packageNames asSet equals: #('Gofer-Core' 'Gofer-Tests' 'NetworkTests' 'Network-Url') asSet.
	old := (packageNames select: [:each | each beginsWith: 'Network'])
		collect: [:each | currentManager packageInformationNamed: each].
	self
		assert: currentManager knownRepositories size = 1
			description: 'After defining a repository, there should be at least one known';
		assert: (currentManager knownRepositories keys collect: [:each | each printString])
			equals: (Set with: 'cypressfiletree:///export/galbadia1/users/rsargent/git/gitRepos/');
		assert: groups size equals: 1;
		assert: (groups at: CypressUnknownPackageInformation) notEmpty
			description: 'There should only beUnknown Package Information objects';
		assert: old size = 2
			description: 'There should be two "Network" packages already in the image';
		assert: (old allSatisfy: [:each | each repositories isEmpty])
			description: 'The packages in the image should still be unknown without a repository'.

	currentManager lookForPackagesInRepository: repo.
	new := packageNames collect: [:each | currentManager packageInformationNamed: each].
	old := old collect: [:each | currentManager packageInformationNamed: each name].
	groups := self currentPackageInformationGroups.

	self
		assert: new size = 4
			description: 'There should be four Package Information objects that were updated';
		assert: groups size equals: 4;
		assert: (groups at: CypressUnknownPackageInformation) notEmpty
			description: 'There should still be some Package Information objects left as Unknown';
		assert: (groups at: CypressKnownPackageInformation) size = new size
			description: 'There should have been one Known Package Information per package in the repository';
		assert: (groups at: CypressKnownPackageInformation) asSet
			equals: new asSet;
		assert: (old allSatisfy: [:each | each imageDigest notNil])
			description: 'Existing packages should have an image digest after connecting to a repository for them'
%

category: 'tests - loading - To Do'
method: CypressPackageManagerTest
testLoadingPackageBranchFromRepository
%

category: 'tests'
method: CypressPackageManagerTest
testLoadingPackageFromCypressRepository

	| repo new summary |
	currentManager := self createEmptyManager.
	repo := self createTestRepoNamed: 'CypressTestRepo'.
	self createFilesFor: self cypressFormatXYPackageDirectoryStructure.
	self assert: repo packageNames asSet equals: #('X-Y') asSet.

	currentManager lookForPackagesInRepository: repo.
	new := currentManager packageInformationNamed: 'X-Y'.
	[summary := currentManager loadPackageFrom: new inRepository: repo]
		ensure: [
	self classToCleanUpNamed: 'XYClass' from: UserGlobals.
	self methodToCleanUp: (Object compiledMethodAt: #isXY).
	self categoryToCleanUp: '*X-Y-testing' from: Object].

	self
		assert: summary isEmpty
			description: 'This package should have loaded without problems';
		assert: (UserGlobals includesKey: #XYClass)
			 description: 'Should have loaded class named XYClass';
		assert: (Object includesCategory: '*X-Y-testing')
			description: 'Should have loaded a method into *X-Y-testing category on Object';
		assert: (Object includesSelector: #isXY)
			description: 'Should have loaded the method Object>>#isXY';
		assert: (Object selectorsIn: '*X-Y-testing') asSet
			equals: (Set with: #isXY)
%

category: 'tests'
method: CypressPackageManagerTest
testLoadingPackageFromFileTreeRepository

	| repo new summary |
	currentManager := self createEmptyManager.
	repo := self createFileTreeCompatibleTestRepoNamed: 'CypressTestRepo'.
	self createFilesFor: self fileTreeFormatXYPackageDirectoryStructure.
	self assert: repo packageNames asSet equals: #('X-Y') asSet.

	currentManager lookForPackagesInRepository: repo.
	new := currentManager packageInformationNamed: 'X-Y'.
	[summary := currentManager loadPackageFrom: new inRepository: repo]
		ensure: [
	self classToCleanUpNamed: 'XYClass' from: UserGlobals.
	self methodToCleanUp: (Object compiledMethodAt: #isXY).
	self categoryToCleanUp: '*X-Y-testing' from: Object].

	self
		assert: summary isEmpty
			description: 'This package should have loaded without problems';
		assert: (UserGlobals includesKey: #XYClass)
			 description: 'Should have loaded class named XYClass';
		assert: (Object includesCategory: '*X-Y-testing')
			description: 'Should have loaded a method into *X-Y-testing category on Object';
		assert: (Object includesSelector: #isXY)
			description: 'Should have loaded the method Object>>#isXY';
		assert: (Object selectorsIn: '*X-Y-testing') asSet
			equals: (Set with: #isXY)
%

category: 'tests - GemStone fileouts - Future'
method: CypressPackageManagerTest
testLoadingPackageFromGemStoneFileoutRepository
	"Presently, filing out .gs files is an option in the repository properties file,
	 and there is no filing in support at all - as a 'repository'.
	 I am considering having a separate repository format, but if one wants to
	 have .gs files in the same directory as the Cypress files, it cannot use a
	 properties file (it would conflict with the other).
	 Additionally, it needs separate files for separate SymbolDictionaries,
	 as well as some notion of the appropriate user."
%

category: 'tests - loading - To Do'
method: CypressPackageManagerTest
testLoadingPackageWithGlobalExtensionWhenNotSystemUser
%

category: 'tests'
method: CypressPackageManagerTest
testRemovingRepositoryFromPackage

	| old repo |
	self testAttachingNewCypressRepositoryToUnknownImagePackages.
	old := currentManager packageInformationNamed: 'X-Y'.
	repo := old repositories any.
	self
		assert: old repositories size = 1
			description: 'There should have been only one repository for the package'.

	old removeRepository: repo.

	self
		assert: old repositories isEmpty
			description: 'There sole repository should have been removed from the package';
		assert: old repositoryDigests isEmpty
			description: 'After removing the last repository, there should not be any repository digests left';
		assert: old digests size = 1
			description: 'After removing the last repository, there should only be the image digest left';
		assert: old digests keys asSet
			equals: (Set with: #IMAGE);
		assert: old imageDigest isNil
			description: 'There should have been no contents in the image'
%

category: 'tests'
method: CypressPackageManagerTest
testSavingChangedImagePackageToCypressRepository

	| repo old newClass results |
	currentManager := self createManagerWithUnknownPackages: #('X-Y').
	repo := self createTestRepoNamed: 'CypressTestRepo'.
	old := currentManager packageInformationNamed: 'X-Y'.
	currentManager assignRepository: repo to: old.
	old := currentManager packageInformationNamed: 'X-Y'.

	newClass := self addClassNamed: 'XYClass' under: Object to: UserGlobals inCatgeory: 'X-Y'.
	self addMethodNamed: 'stub' body: '^nil' toClass: newClass inCategory: 'dummy'.
	self addMethodNamed: 'isXY' body: '^false' toClass: Object inCategory: '*X-Y-testing'.

	currentManager writeChangesToAllRepositoriesFor: old.

	results := CypressFileUtilities current directoryFileNamesAndContents: repo directoryPath.
	self assert: results equals: self cypressFormatXYPackageDirectoryStructure.
%

category: 'tests'
method: CypressPackageManagerTest
testSavingChangedImagePackageToFileTreeReadOnlyRepository

	| repo old newClass results |
	currentManager := self createManagerWithUnknownPackages: #('X-Y').
	repo := self createFileTreeReadOnlyTestRepoNamed: 'CypressTestRepo'.
	old := currentManager packageInformationNamed: 'X-Y'.
	currentManager assignRepository: repo to: old.
	old := currentManager packageInformationNamed: 'X-Y'.

	newClass := self addClassNamed: 'XYClass' under: Object to: UserGlobals inCatgeory: 'X-Y'.
	self addMethodNamed: 'stub' body: '^nil' toClass: newClass inCategory: 'dummy'.
	self addMethodNamed: 'isXY' body: '^false' toClass: Object inCategory: '*X-Y-testing'.

	currentManager writeChangesToAllRepositoriesFor: old.

	results := CypressFileUtilities current directoryFileNamesAndContents: repo directoryPath.
	self assert: results equals: self readOnlyFileTreeFormatXYPackageDirectoryStructure.
%

category: 'tests'
method: CypressPackageManagerTest
testSavingChangedImagePackageToFileTreeRepository

	| repo old newClass results |
	currentManager := self createManagerWithUnknownPackages: #('X-Y').
	repo := self createFileTreeCompatibleTestRepoNamed: 'CypressTestRepo'.
	old := currentManager packageInformationNamed: 'X-Y'.
	currentManager assignRepository: repo to: old.
	old := currentManager packageInformationNamed: 'X-Y'.

	newClass := self addClassNamed: 'XYClass' under: Object to: UserGlobals inCatgeory: 'X-Y'.
	self addMethodNamed: 'stub' body: '^nil' toClass: newClass inCategory: 'dummy'.
	self addMethodNamed: 'isXY' body: '^false' toClass: Object inCategory: '*X-Y-testing'.

	currentManager writeChangesToAllRepositoriesFor: old.

	results := CypressFileUtilities current directoryFileNamesAndContents: repo directoryPath.
	self assert: results equals: self fileTreeFormatXYPackageDirectoryStructure.
%

category: 'tests - GemStone fileouts - Future'
method: CypressPackageManagerTest
testSavingChangedImagePackageToGemStoneFileoutRepository
	"Presently, filing out .gs files is an option in the repository properties file.
	 I am considering having a separate repository format, but if one wants to
	 have .gs files in the same directory as the Cypress files, it cannot use a
	 properties file (it would conflict with the other).
	 Additionally, it needs separate files for separate SymbolDictionaries,
	 as well as some notion of the appropriate user."
%

! Class Implementation for CypressReferenceTest

! ------------------- Instance methods for CypressReferenceTest

category: 'running'
method: CypressReferenceTest
assert: aString parsesToPackageName: packageName author: authorId branch: branchId version: versionNumber

	| queryReference |
	queryReference := CypressVersionReference name: aString.
	self
		assert: queryReference packageName equals: packageName;
		assert: queryReference author equals: authorId;
		assert: queryReference branch equals: branchId;
		assert: queryReference versionNumber equals: versionNumber.
%

category: 'testing'
method: CypressReferenceTest
testAuthorAlone
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: '-dhk'
			parsesToPackageName: ''
			author: 'dhk'
			branch: ''
			version: 0
%

category: 'testing'
method: CypressReferenceTest
testAuthorAndVersionOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: '-dhk.1'
			parsesToPackageName: ''
			author: 'dhk'
			branch: ''
			version: 1
%

category: 'testing'
method: CypressReferenceTest
testBranchAfterAuthorIsNotABranch

	self
		assert: 'Seaside-Core-jf.configcleanup.3'
			parsesToPackageName: 'Seaside-Core'
			author: 'jf.configcleanup'
			branch: ''
			version: 3;
		assert: 'Seaside-Core-lr.configcleanup.extraspeedup.69'
			parsesToPackageName: 'Seaside-Core'
			author: 'lr.configcleanup.extraspeedup'
			branch: ''
			version: 69;
		assert: 'Seaside-Core-lr.configcleanup42.extraspeedup.69'
			parsesToPackageName: 'Seaside-Core'
			author: 'lr.configcleanup42.extraspeedup'
			branch: ''
			version: 69
%

category: 'testing'
method: CypressReferenceTest
testBranchAlone
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: '.v3'
			parsesToPackageName: ''
			author: ''
			branch: 'v3'
			version: 0
%

category: 'testing'
method: CypressReferenceTest
testBranchAndAuthorOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: '.v3-dhk'
			parsesToPackageName: ''
			author: 'dhk'
			branch: 'v3'
			version: 0
%

category: 'testing'
method: CypressReferenceTest
testBranchAndVersionOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: '.v3.1'
			parsesToPackageName: ''
			author: ''
			branch: 'v3.1'
			version: 0;
		assert: '.v3-.1'
			parsesToPackageName: ''
			author: ''
			branch: 'v3'
			version: 1
%

category: 'testing'
method: CypressReferenceTest
testBranchAuthorAndVersionOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: '.v3-dhk.1'
			parsesToPackageName: ''
			author: 'dhk'
			branch: 'v3'
			version: 1
%

category: 'testing'
method: CypressReferenceTest
testEmptyString
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: ''
			parsesToPackageName: ''
			author: ''
			branch: ''
			version: 0
%

category: 'testing'
method: CypressReferenceTest
testPackageAlone
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: 'Announcements'
			parsesToPackageName: 'Announcements'
			author: ''
			branch: ''
			version: 0;
		assert: 'Announcements.-.'
			parsesToPackageName: 'Announcements'
			author: ''
			branch: ''
			version: 0
%

category: 'testing'
method: CypressReferenceTest
testPackageAndAuthorOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: 'Announcements-dhk'
			parsesToPackageName: 'Announcements'
			author: 'dhk'
			branch: ''
			version: 0
%

category: 'testing'
method: CypressReferenceTest
testPackageAndBranchOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: 'Announcements.v3'
			parsesToPackageName: 'Announcements'
			author: ''
			branch: 'v3'
			version: 0
%

category: 'testing'
method: CypressReferenceTest
testPackageAndVersionOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: 'Announcements..1'
			parsesToPackageName: 'Announcements'
			author: ''
			branch: '.1'
			version: 0;
		assert: 'Announcements.-.1'
			parsesToPackageName: 'Announcements'
			author: ''
			branch: ''
			version: 1
%

category: 'testing'
method: CypressReferenceTest
testPackageAuthorAndVersionOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: 'Announcements-dhk.1'
			parsesToPackageName: 'Announcements'
			author: 'dhk'
			branch: ''
			version: 1
%

category: 'testing'
method: CypressReferenceTest
testPackageBranchAndAuthorOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: 'Announcements.v3-dhk'
			parsesToPackageName: 'Announcements'
			author: 'dhk'
			branch: 'v3'
			version: 0
%

category: 'testing'
method: CypressReferenceTest
testPackageBranchAndVersionOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: 'Announcements.v3.1'
			parsesToPackageName: 'Announcements'
			author: ''
			branch: 'v3.1'
			version: 0;
		assert: 'Announcements.v3-.1'
			parsesToPackageName: 'Announcements'
			author: ''
			branch: 'v3'
			version: 1
%

category: 'testing'
method: CypressReferenceTest
testPackageBranchAuthorAndVersion
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: 'Announcements.v3-dhk.1'
			parsesToPackageName: 'Announcements'
			author: 'dhk'
			branch: 'v3'
			version: 1
%

category: 'testing'
method: CypressReferenceTest
testPunctuationOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: '.-.'
			parsesToPackageName: ''
			author: ''
			branch: ''
			version: 0;
		assert: '.-'
			parsesToPackageName: ''
			author: ''
			branch: ''
			version: 0;
		assert: '.'
			parsesToPackageName: ''
			author: ''
			branch: ''
			version: 0;
		assert: '..'
			parsesToPackageName: ''
			author: ''
			branch: '.'
			version: 0
%

category: 'testing'
method: CypressReferenceTest
testVersionAlone
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: '..1'
			parsesToPackageName: ''
			author: ''
			branch: '.1'
			version: 0;
		assert: '.-.1'
			parsesToPackageName: ''
			author: ''
			branch: ''
			version: 1
%

category: 'testing'
method: CypressReferenceTest
testVersionShouldParseComplexName
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: 'Seaside2.8b5'
			parsesToPackageName: 'Seaside2'
			author: ''
			branch: '8b5'
			version: 0;
		assert: 'Seaside2.8b5-avi.1'
			parsesToPackageName: 'Seaside2'
			author: 'avi'
			branch: '8b5'
			version: 1;
		assert: 'Seaside-Core-pmm.2'
			parsesToPackageName: 'Seaside-Core'
			author: 'pmm'
			branch: ''
			version: 2;
		assert: 'Seaside-Core.configcleanup-jf.3'
			parsesToPackageName: 'Seaside-Core'
			author: 'jf'
			branch: 'configcleanup'
			version: 3;
		assert: 'Seaside-Core.configcleanup.extraspeedup-lr.69'
			parsesToPackageName: 'Seaside-Core'
			author: 'lr'
			branch: 'configcleanup.extraspeedup'
			version: 69;
		assert: 'Seaside-Core.configcleanup42.extraspeedup-lr.69'
			parsesToPackageName: 'Seaside-Core'
			author: 'lr'
			branch: 'configcleanup42.extraspeedup'
			version: 69
%

! Class Extensions

! Class initializers 

doit
true.
%



! End of Package: Cypress-PackageManagementTests


