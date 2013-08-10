! Package: Cypress-Tests


! Remove existing behavior from package Cypress-Tests
!!!! This can be cleaned up when some package functionality is moved to the base system.

doit
| packageName |
packageName := 'Cypress-Tests'.
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
(TestCase
	subclass: 'CypressAbstractTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Tests';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractTest
	subclass: 'CypressStructureTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Tests';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractTest
	subclass: 'CypressLoaderTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Tests';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractTest
	subclass: 'CypressDefinitionTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Tests';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractTest
	subclass: 'CypressPatchTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Tests';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractTest
	subclass: 'CypressSnapshotTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Tests';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractTest
	subclass: 'CypressExtensionsTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Tests';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractTest
	subclass: 'CypressReferenceTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Tests';
		comment: '';
		immediateInvariant.
%

! Class Implementation for CypressAbstractTest

! ------------------- Class methods for CypressAbstractTest

category: 'testing'
set compile_env: 0
classmethod: CypressAbstractTest
isAbstract

	^self sunitName = #CypressAbstractTest
%

! ------------------- Instance methods for CypressAbstractTest

category: 'private'
set compile_env: 0
method: CypressAbstractTest
baseDefinitions

	| className |
	className := 'CypressMockBasic'.
	^{
		CypressClassDefinition
			name: className
			superclassName: 'Object'
			category: 'Cypress-Mocks'
			instVarNames: #('name')
			classInstVarNames: #('current')
			classVarNames: #('Something')
			poolDictionaryNames: #()
			comment: 'This mock contains basic class and instance method selectors'
			subclassType: ''.
		CypressMethodDefinition
			className: className
			classIsMeta: false
			selector: 'extra'
			category: 'accessing'
			source: 'extra
	"extra method"
'.
		CypressMethodDefinition
			className: className
			classIsMeta: false
			selector: 'initialize'
			category: 'initialization'
			source: 'initialize
	super initialize.
	self name: ''Unknown''
'.
		CypressMethodDefinition
			className: className
			classIsMeta: false
			selector: 'name'
			category: 'accessing'
			source: 'name
	^name
'.
		CypressMethodDefinition
			className: className
			classIsMeta: false
			selector: 'name:'
			category: 'accessing'
			source: 'name: aString
	name := aString
'.
		CypressMethodDefinition
			className: className
			classIsMeta: true
			selector: 'current'
			category: 'accessing'
			source: 'current
	^current
'.
		CypressMethodDefinition
			className: className
			classIsMeta: true
			selector: 'current:'
			category: 'accessing'
			source: 'current: anObject
	current := anObject
'.
		CypressMethodDefinition
			className: className
			classIsMeta: true
			selector: 'initialize'
			category: 'initialization'
			source: 'initialize
	self current: self new
'.
		CypressMethodDefinition
			className: 'Object'
			classIsMeta: false
			selector: 'isCypressMockBasic'
			category: '*Cypress-Mocks-Extensions'
		source: 'isCypressMockBasic

	^false'.
	}
%

category: 'private'
set compile_env: 0
method: CypressAbstractTest
basePackageStructureJson
    ^ '{
	"name" : "Cypress-Mocks.package",
	"contents" : [
		{
			"name" : "CypressMockBasic.class",
			"instance" : [
				{
					"name" : "extra.st",
					"contents" : "accessing%0Aextra%0A%09%22extra%20method%22%0A"
				 },
				{
					"name" : "initialize.st",
					"contents" : "initialization%0Ainitialize%0A%09super%20initialize.%0A%09self%20name%3A%20%27Unknown%27%0A"
				 },
				{
					"name" : "name.st",
					"contents" : "accessing%0Aname%0A%09%5Ename%0A"
				 },
				{
					"name" : "name..st",
					"contents" : "accessing%0Aname%3A%20aString%0A%09name%20%3A%3D%20aString%0A"
				 }			],
			"class" : [
				{
					"name" : "current.st",
					"contents" : "accessing%0Acurrent%0A%09%5Ecurrent%0A"
				 },
				{
					"name" : "current..st",
					"contents" : "accessing%0Acurrent%3A%20anObject%0A%09current%20%3A%3D%20anObject%0A"
				 },
				{
					"name" : "initialize.st",
					"contents" : "initialization%0Ainitialize%0A%09self%20current%3A%20self%20new%0A"
				 }			],
			"README.md" : "This%20mock%20contains%20basic%20class%20and%20instance%20method%20selectors",
			"properties.json" : {
				"classinstvars" : [
					"current" ],
				"classvars" : [
					"Something" ],
				"instvars" : [
					"name" ],
				"name" : "CypressMockBasic",
				"pools" : [
					 ],
				"super" : "Object",
				"_gs_subclassType" : "" }
		 },
		{
			"name" : "Object.extension",
			"instance" : [
				{
					"name" : "isCypressMockBasic.st",
					"contents" : "%2ACypress-Mocks-Extensions%0AisCypressMockBasic%0A%0A%09%5Efalse"
				 }			],
			"class" : [
			],
			"properties.json" : {
				"name" : "Object" }
		 }
	],
	"properties.json" : {
		 }
}'
%

category: 'private'
set compile_env: 0
method: CypressAbstractTest
baseTargetPatch

	| className |
	className := 'CypressMockBasic'.
	^{
		CypressModification
			of: (CypressClassDefinition
				name: className
				superclassName: 'Object'
				category: 'Cypress-Mocks'
				instVarNames: #('name')
				classInstVarNames: #('current')
				classVarNames: #('Something')
				poolDictionaryNames: #()
				comment: 'This mock contains basic class and instance method selectors'
				subclassType: '')
			to: (CypressClassDefinition
				name: className
				superclassName: 'Object'
				category: 'Cypress-Mocks'
				instVarNames: #('name')
				classInstVarNames: #('current')
				classVarNames: #()
				poolDictionaryNames: #()
				comment: 'This mock contains basic class and instance method selectors'
				subclassType: '').
		CypressAddition
			of: (CypressMethodDefinition
				className: className
				classIsMeta: false
				selector: 'added'
				category: 'accessing'
				source: 'added
	"added method"
').
		CypressModification
			of: (CypressMethodDefinition
				className: className
				classIsMeta: false
				selector: 'name:'
				category: 'accessing'
				source: 'name: aString
	name := aString
')
			to: (CypressMethodDefinition
				className: className
				classIsMeta: false
				selector: 'name:'
				category: 'accessing'
				source: 'name: aString
	"changed method"
	name := aString
').
		CypressRemoval
			of: (CypressMethodDefinition
				className: className
				classIsMeta: false
				selector: 'extra'
				category: 'accessing'
				source: 'extra
	"extra method"
').
		CypressRemoval
			of: (CypressMethodDefinition
				className: 'Object'
				classIsMeta: false
				selector: 'isCypressMockBasic'
				category: '*Cypress-Mocks-Extensions'
				source: 'isCypressMockBasic

	^false').
		CypressAddition
			of: (CypressClassDefinition
				name: className , 'Sub'
				superclassName: className
				category: 'Cypress-Mocks'
				instVarNames: #('anotherIV')
				classInstVarNames: #('anotherCIV')
				classVarNames: #()
				poolDictionaryNames: #()
				comment: 'Hacked subclass to test class loading and unloading'
				subclassType: '').
		CypressAddition
			of: (CypressMethodDefinition
				className: className , 'Sub'
				classIsMeta: false
				selector: 'added'
				category: 'accessing'
				source: 'added
	"added method"
').
	}
%

category: 'private'
set compile_env: 0
method: CypressAbstractTest
sampleJson
    ^ '{
	"age" : 25,
	"name" : "John%20Smith",
	"phoneNumber" : [
		{
			"number" : "212%20555-1234",
			"type" : "home" },
		{
			"number" : "646%20555-4567",
			"type" : "fax" } ],
	"registered" : true }'
%

category: 'private'
set compile_env: 0
method: CypressAbstractTest
targetDefinitions
	"remove #extra method and modify #name: method"

	| className |
	className := 'CypressMockBasic'.
	^{
		CypressClassDefinition
			name: className
			superclassName: 'Object'
			category: 'Cypress-Mocks'
			instVarNames: #('name')
			classInstVarNames: #('current')
			classVarNames: #()
			poolDictionaryNames: #()
			comment: 'This mock contains basic class and instance method selectors'
			subclassType: ''.
		CypressMethodDefinition
			className: className
			classIsMeta: false
			selector: 'added'
			category: 'accessing'
			source: 'added
	"added method"
'.
		CypressMethodDefinition
			className: className
			classIsMeta: false
			selector: 'initialize'
			category: 'initialization'
			source: 'initialize
	super initialize.
	self name: ''Unknown''
'.
		CypressMethodDefinition
			className: className
			classIsMeta: false
			selector: 'name'
			category: 'accessing'
			source: 'name
	^name
'.
		CypressMethodDefinition
			className: className
			classIsMeta: false
			selector: 'name:'
			category: 'accessing'
			source: 'name: aString
	"changed method"
	name := aString
'.
		CypressMethodDefinition
			className: className
			classIsMeta: true
			selector: 'current'
			category: 'accessing'
			source: 'current
	^current
'.
		CypressMethodDefinition
			className: className
			classIsMeta: true
			selector: 'current:'
			category: 'accessing'
			source: 'current: anObject
	current := anObject
'.
		CypressMethodDefinition
			className: className
			classIsMeta: true
			selector: 'initialize'
			category: 'initialization'
			source: 'initialize
	self current: self new
'.
		CypressClassDefinition
			name: className , 'Sub'
			superclassName: className
			category: 'Cypress-Mocks'
			instVarNames: #('anotherIV')
			classInstVarNames: #('anotherCIV')
			classVarNames: #()
			poolDictionaryNames: #()
			comment: 'Hacked subclass to test class loading and unloading'
			subclassType: ''.
		CypressMethodDefinition
			className: className , 'Sub'
			classIsMeta: false
			selector: 'added'
			category: 'accessing'
			source: 'added
	"added method"
'.
	}
%

! Class Implementation for CypressStructureTest

! ------------------- Instance methods for CypressStructureTest

category: 'private'
set compile_env: 0
method: CypressStructureTest
compileJSON: aJsonString

	^CypressJsonParser parse: aJsonString
%

category: 'tests'
set compile_env: 0
method: CypressStructureTest
testClassStructure

	| jsObject packageStructure classStructure classProperties |
	jsObject := self compileJSON: self basePackageStructureJson.
	packageStructure := CypressPackageStructure fromJs: jsObject.
	classStructure := packageStructure classes first.
	self assert: classStructure name equals: 'CypressMockBasic'.
	self deny: classStructure isClassExtension description: 'Class structure should not have been an extension'.
	self assert: classStructure comment equals: 'This mock contains basic class and instance method selectors'.
	classProperties := classStructure properties.
	self assert: classProperties size equals: 7.
	self assert: (classProperties at: 'instvars') equals: #('name').
	self assert: (classProperties at: 'classinstvars') equals: #('current').
	self assert: (classProperties at: 'name') equals: 'CypressMockBasic'.
	self assert: (classProperties at: '_gs_subclassType') equals: ''.
	self assert: (classProperties at: 'super') equals: 'Object'.
	self assert: classStructure instanceMethods size equals: 4.
	self assert: classStructure classMethods size equals: 3.
	classStructure := packageStructure extensions first.
	self assert: classStructure name equals: 'Object'.
	self assert: classStructure isClassExtension description: 'Class structure should have been an extension'.
	self assert: classStructure comment equals: ''.
	classProperties := classStructure properties.
	self assert: classProperties size equals: 1.
	self assert: (classProperties at: 'name') equals: 'Object'.
	self assert: classStructure instanceMethods size equals: 1.
	self assert: classStructure classMethods size equals: 0.
%

category: 'tests'
set compile_env: 0
method: CypressStructureTest
testJson
	"Let's compile the JSON without errors"

	self compileJSON: self basePackageStructureJson
%

category: 'tests'
set compile_env: 0
method: CypressStructureTest
testPackageStructureFromJson

	| packageStructure classStructure classProperties |
	packageStructure := CypressPackageStructure fromJson: self basePackageStructureJson.
	self assert: packageStructure name equals: 'Cypress-Mocks.package'.
	self assert: packageStructure packageName equals: 'Cypress-Mocks'.
	self assert: packageStructure properties isEmpty description: 'Properties should have been empty'.
	self assert: packageStructure extensions size equals: 1.
	self assert: packageStructure classes size equals: 1.
%

category: 'tests'
set compile_env: 0
method: CypressStructureTest
testPackageStructureFromPackage

	| packageStructure packageDefinitions expectedDefinitions |
	packageStructure := CypressPackageStructure
				fromPackage: (CypressPackageDefinition named: 'Cypress-Mocks').
	packageDefinitions := packageStructure snapshot definitions.
	expectedDefinitions := self baseDefinitions.
	self assert: expectedDefinitions size equals: packageDefinitions size.
	packageDefinitions do: 
			[:def |
			self assert: (expectedDefinitions includes: def)
				description: 'Definition for ', def printString, ' did not match expected ones']
%

category: 'tests'
set compile_env: 0
method: CypressStructureTest
testPackageStructureSnapshot

	| packageStructure packageDefinitions expectedDefinitions |
	packageStructure := CypressPackageStructure
				fromJs: (self compileJSON: self basePackageStructureJson).
	packageDefinitions := packageStructure snapshot definitions.
	expectedDefinitions := self baseDefinitions.
	self assert: expectedDefinitions size equals: packageDefinitions size.
	packageDefinitions do: 
			[:def |
			self assert: (expectedDefinitions includes: def)
				description: 'Definition was not as expected']
%

category: 'tests'
set compile_env: 0
method: CypressStructureTest
testPackageStructureToJson
    | packageStructure stream string expected x y |
    packageStructure := CypressPackageStructure fromPackage: (CypressPackageDefinition named: 'Cypress-Mocks').
    stream := WriteStream on: String new.
    packageStructure writeJsonOn: stream.
    string := stream contents withUnixLineEndings.
    expected := self basePackageStructureJson withUnixLineEndings.
    1 to: string size do: [ :i | 
        (i > expected size or: [ (string at: i) ~= (expected at: i) ])
            ifTrue: [ 
                x := string copyFrom: (i - 25 max: 1) to: (i + 25 min: string size).
                y := expected copyFrom: ((i - 25 max: 1) min: expected size) to: (i + 25 min: expected size).
                Array with: x with: y	"halt" ] ].
    self assert: expected equals: string
%

category: 'tests'
set compile_env: 0
method: CypressStructureTest
testPropertyDictionaryRead

	| propertyDictionary phoneNumbers |
	propertyDictionary := (self compileJSON: self sampleJson) asCypressPropertyObject.
	self assert: (propertyDictionary at: 'name') equals: 'John Smith'.
	self assert: (propertyDictionary at: 'age') equals: 25.
	self assert: (propertyDictionary at: 'registered') description: '"registered" property should have been true'.
	phoneNumbers := propertyDictionary at: 'phoneNumber'.
	self assert: phoneNumbers size equals: 2.
	self assert: ((phoneNumbers at: 1) at: 'number') equals: '212 555-1234'.
	self assert: ((phoneNumbers at: 2) at: 'number') equals: '646 555-4567'.
%

category: 'tests'
set compile_env: 0
method: CypressStructureTest
testPropertyDictionaryWrite
    | propertyDictionary stream x |
    propertyDictionary := (self compileJSON: self sampleJson) asCypressPropertyObject.
    stream := WriteStream on: String new.
    propertyDictionary writeCypressJsonOn: stream indent: 0.
    self assert: (x := stream contents withUnixLineEndings) equals: self sampleJson withUnixLineEndings
%

! Class Implementation for CypressLoaderTest

! ------------------- Instance methods for CypressLoaderTest

category: 'utility'
set compile_env: 0
method: CypressLoaderTest
erroneousClassDefinitions

	^{
		CypressClassDefinition
			name: 'CypressMockBasicSubclassWithDuplicateInstanceVariable'
			superclassName: 'CypressMockBasic'
			category: 'Cypress-Mocks'
			instVarNames: #('name')
			classInstVarNames: #()
			classVarNames: #()
			poolDictionaryNames: #()
			comment: 'This class tries to define an instance variable which already exists in the superclass.'
			subclassType: ''.
	}
%

category: 'running'
set compile_env: 0
method: CypressLoaderTest
tearDown

	| name |
	super tearDown.
	name := 'Cypress-Mocks'.
	(CypressSnapshot definitions: self baseDefinitions)
		 updatePackage: (CypressPackageDefinition named: name)
%

category: 'tests'
set compile_env: 0
method: CypressLoaderTest
testLoad

	| name loader |
	name := 'Cypress-Mocks'.
	loader := (CypressSnapshot definitions: self targetDefinitions)
				updatePackage: (CypressPackageDefinition named: name).
	self
		assert: loader additions size equals: 5;
		assert: loader removals size equals: 2;
		assert: loader unloadable size equals: 0;
		assert: loader provisions notEmpty
			description: 'There should have been a number of classes provided by the system';
		assert: loader errors size equals: 0;
		assert: loader methodAdditions size equals: 0;
		assert: loader requirements isEmpty
			description: 'There should have been no external requirements'
%

category: 'tests'
set compile_env: 0
method: CypressLoaderTest
testLoaderWithClassDefinitionError

	| name loader |
	name := 'Cypress-Mocks'.
	self
		should: 
			[(CypressSnapshot
				definitions: self erroneousClassDefinitions , self targetDefinitions)
					updatePackage: (CypressPackageDefinition named: name)]
		raise: CypressLoaderErrorNotification
		description: 'There should have been a class definition with an error'.
	loader := 
			[(CypressSnapshot
				definitions: self erroneousClassDefinitions , self targetDefinitions)
					updatePackage: (CypressPackageDefinition named: name)]
					on: CypressLoaderError
					do: 
						[:ex |
						self
							assert: ex patchOperation definition name
							equals: 'CypressMockBasicSubclassWithDuplicateInstanceVariable'.
						ex resume].
	self
		assert: loader additions size equals: 4;
		assert: loader removals size equals: 2;
		assert: loader unloadable size equals: 0;
		assert: (loader unloadable gather: [:each | each]) size equals: 0;
		assert: loader provisions notEmpty
			description: 'There should have been a number of classes provided by the system';
		assert: loader errors size equals: 1;
		assert: loader methodAdditions size equals: 0;
		assert: loader requirements size equals: 0
%

category: 'tests'
set compile_env: 0
method: CypressLoaderTest
testLoaderWithUnloadable

	| name loader |
	name := 'Cypress-Mocks'.
	self
		should: 
			[(CypressSnapshot
				definitions: self unloadableDefinitions , self targetDefinitions)
					updatePackage: (CypressPackageDefinition named: name)]
		raise: CypressLoaderMissingClasses
		description: 'There should have been some unloadable definitions'.
	loader := 
			[(CypressSnapshot
				definitions: self unloadableDefinitions , self targetDefinitions)
					updatePackage: (CypressPackageDefinition named: name)]
					on: CypressLoaderMissingClasses
					do: 
						[:ex |
						self
							assert: #('AnotherNonExistentClass' 'NonExistentClass' 'UnloadableClass')
							equals: ex requirementsMap keys asSortedCollection asArray.
						ex resume].
	self
		assert: loader additions size equals: 5;
		assert: loader removals size equals: 2;
		assert: loader unloadable size equals: 3;
		assert: (loader unloadable gather: [:each | each]) size equals: 3;
		assert: loader provisions notEmpty
			description: 'There should have been a number of classes provided by the system';
		assert: loader errors size equals: 0;
		assert: loader methodAdditions size equals: 0;
		assert: loader requirements size equals: 2
%

category: 'utility'
set compile_env: 0
method: CypressLoaderTest
unloadableDefinitions

	^{
		CypressClassDefinition
			name: 'UnloadableClass'
			superclassName: 'NonExistentClass'
			category: 'Cypress-Mocks'
			instVarNames: #()
			classInstVarNames: #()
			classVarNames: #()
			poolDictionaryNames: #()
			comment: 'This class depends on a class that is intended to be missing.'
			subclassType: ''.
		CypressMethodDefinition
			className: 'UnloadableClass'
			classIsMeta: false
			selector: 'unloadable'
			category: 'accessing'
			source: 'unloadable
	"This method cannot be loaded because the class cannot be."'.
		CypressMethodDefinition
			className: 'AnotherNonExistentClass'
			classIsMeta: false
			selector: 'unloadable'
			category: 'accessing'
			source: 'unloadable
	"This method cannot be loaded because the class cannot be."'.
	}
%

! Class Implementation for CypressDefinitionTest

! ------------------- Instance methods for CypressDefinitionTest

category: 'tests'
set compile_env: 0
method: CypressDefinitionTest
testClassDefinition

	self
		assert: (CypressClassDefinition
					name: 'Foo'
					superclassName: 'Object'
					category: 'Foo'
					instVarNames: #()
					classInstVarNames: #()
					classVarNames: #()
					poolDictionaryNames: #()
					comment: ''
					subclassType: '') printString
		equals: 'a CypressClassDefinition(Foo)'
%

category: 'tests'
set compile_env: 0
method: CypressDefinitionTest
testDictionaryOfDefinitions

	| dict |
	"baseDefinitions"
	dict := Dictionary new.
	self baseDefinitions do: [:each | 
		dict at: each put: each ].
	self baseDefinitions do: [:each | 
		self assert: (dict at: each) equals: each ].

	"targetDefinitions"
	dict := Dictionary new.
	self targetDefinitions do: [:each | 
		dict at: each put: each ].
	self targetDefinitions do: [:each | 
		self assert: (dict at: each) equals: each ].
%

category: 'tests'
set compile_env: 0
method: CypressDefinitionTest
testEquality
	| pkg1 pkg2 pkg3 name |
	name := 'Cypress-Mocks'.
	pkg1 := CypressPackageDefinition named: name.
	pkg2 := CypressPackageDefinition named: name.
	pkg3 := CypressPackageDefinition named: 'Nope!'.

	self assert: pkg1 equals: pkg2.
	self deny: pkg1 equals: pkg3
%

category: 'tests'
set compile_env: 0
method: CypressDefinitionTest
testMethodDefinition
	self assert: (CypressMethodDefinition
		className: 'Foo'
		classIsMeta: false
		selector: 'isFoo'
		category: 'testing'
		source: 'isFoo ^true') printString equals: 'a CypressMethodDefinition(Foo>>isFoo)'
%

category: 'tests'
set compile_env: 0
method: CypressDefinitionTest
testNameEquality
	| pkg name |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition named: name.
	self assert: pkg name equals: name.
	self deny: pkg name equals: 'Nope.'.
%

category: 'tests'
set compile_env: 0
method: CypressDefinitionTest
testPrintString
	| name pkg |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition named: name.
	self assert: 'a CypressPackageDefinition(', name, ')' equals: pkg printString.
%

! Class Implementation for CypressPatchTest

! ------------------- Instance methods for CypressPatchTest

category: 'tests'
set compile_env: 0
method: CypressPatchTest
testDictionaryOfPatchOperations
	"loader uses dictionary for managing patch operations ... ensure that Amber Dictionaries stand up"

	| dict |
	dict := Dictionary new.
	self baseTargetPatch do: [:each | 
		dict at: each put: each ].
	self baseTargetPatch do: [:each | 
		self assert: (dict at: each) equals: each ].
%

category: 'tests'
set compile_env: 0
method: CypressPatchTest
testPatch

	| baseSnapshot targetSnapshot patch operations expected |
	baseSnapshot := CypressSnapshot definitions: self baseDefinitions.
	targetSnapshot := CypressSnapshot definitions: self targetDefinitions.
	patch := CypressPatch fromBase: baseSnapshot toTarget: targetSnapshot.
	operations := patch operations.
	expected := self baseTargetPatch asArray.
	self assert: operations size equals: expected size.
	operations do: [:each | self assert: (expected includes: each)]
%

category: 'tests'
set compile_env: 0
method: CypressPatchTest
testPatchOperationEquality

	| className modification removal addition |
	className := 'CypressMockBasic'.
	modification := CypressModification 
			of: (CypressMethodDefinition
          			className: className
        			classIsMeta: false
        			selector: 'name:'
        			category: 'accessing'
        			source:'name: aString
	name := aString') 
			to: (CypressMethodDefinition
          			className: className
        			classIsMeta: false
        			selector: 'name:'
        			category: 'accessing'
        			source:'name: aString
	"changed method"
	name := aString').
	self assert: modification equals: modification.
	removal := CypressRemoval 
			of: (CypressMethodDefinition
          			className: className
        			classIsMeta: false
        			selector: 'extra'
        			category: 'accessing'
        			source:'extra
	"extra method"').
	self assert: removal equals: removal.
	addition := CypressAddition
			of: (CypressMethodDefinition
          			className: className
        			classIsMeta: false
        			selector: 'extra'
        			category: 'accessing'
        			source:'extra
	"extra method"').
	self assert: addition equals: addition.
%

! Class Implementation for CypressSnapshotTest

! ------------------- Instance methods for CypressSnapshotTest

category: 'tests'
set compile_env: 0
method: CypressSnapshotTest
testExtensionsOnlySnapshot

	| name pkg packageDefinitions expectedDefinitions |
	name := 'Cypress-Mocks-Extensions'.
	pkg := CypressPackageDefinition named: name.
	packageDefinitions := pkg snapshot definitions.
	self
		assert: packageDefinitions size equals: 1;
		assert: packageDefinitions first selector equals: 'isCypressMockBasic';
		assert: packageDefinitions first className equals: 'Object'
%

category: 'tests'
set compile_env: 0
method: CypressSnapshotTest
testSnapshot

	| name pkg packageDefinitions expectedDefinitions |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition named: name.
	packageDefinitions := pkg snapshot definitions.
	expectedDefinitions := self baseDefinitions.
	self assert: expectedDefinitions size equals: packageDefinitions size.
	packageDefinitions
		do: [:def | self assert: (expectedDefinitions includes: def)]
%

category: 'tests'
set compile_env: 0
method: CypressSnapshotTest
testSnapshotEquality
	| name pkg packageDefinitions expectedDefinitions |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition named: name.
	packageDefinitions := pkg snapshot definitions.
	expectedDefinitions := self baseDefinitions.
	self assert: packageDefinitions asArray equals: expectedDefinitions asArray
%

! Class Implementation for CypressExtensionsTest

! ------------------- Instance methods for CypressExtensionsTest

category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_beginsWith_

	self
		assert: ('abc' beginsWith: 'a');
		deny: ('abc' beginsWith: 'c');
		assert: ('abc' beginsWith: 'abc');
		deny: ('abc' beginsWith: 'abcx');
		deny: ('abc' beginsWith: '');
		deny: ('' beginsWith: 'abc');
		deny: ('' beginsWith: '')
%

category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_copyWithoutSuffix_

	self
		assert: ('abc' copyWithoutSuffix: 'c') equals: 'ab';
		assert: ('abc' copyWithoutSuffix: 'a') equals: 'abc';
		assert: ('abc' copyWithoutSuffix: 'abc') equals: '';
		assert: ('abc' copyWithoutSuffix: 'xxxabc') equals: 'abc';
		assert: ('abc' copyWithoutSuffix: '') equals: 'abc';
		assert: ('' copyWithoutSuffix: 'abc') equals: '';
		assert: ('' copyWithoutSuffix: '') equals: ''
%

category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_copyWithoutSuffix_or_

	self
		assert: ('abc' copyWithoutSuffix: 'c' or: [nil]) equals: 'ab';
		assert: ('abc' copyWithoutSuffix: 'a' or: [nil]) equals: nil;
		assert: ('abc' copyWithoutSuffix: 'abc' or: [nil]) equals: '';
		assert: ('abc' copyWithoutSuffix: 'xxxabc' or: [nil]) equals: nil;
		assert: ('abc' copyWithoutSuffix: '' or: [nil]) equals: nil;
		assert: ('' copyWithoutSuffix: 'abc' or: [nil]) equals: nil;
		assert: ('' copyWithoutSuffix: '' or: [nil]) equals: nil
%

category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_difference_
	"Set theoretic difference means elements from the first collection not in the second."

	self
		assert: (#(1 2 3) difference: #()) sortAscending equals: #(1 2 3);
		assert: (#() difference: #(1 2 3)) sortAscending equals: #();
		assert: (#(1 2 3) difference: #(1 2 3)) sortAscending equals: #();
		assert: (#(1 2 3) difference: #(2 3 4)) sortAscending equals: #(1);
		assert: (#(1 2 3) difference: #(3 4 5)) sortAscending equals: #(1 2);
		assert: (#(1 2 3) difference: #(4 5 6)) sortAscending equals: #(1 2 3)
%

category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_endsWith_

	self
		assert: ('abc' endsWith: 'c');
		deny: ('abc' endsWith: 'a');
		assert: ('abc' endsWith: 'abc');
		deny: ('abc' endsWith: 'xabc');
		deny: ('abc' endsWith: '');
		deny: ('' endsWith: 'abc');
		deny: ('' endsWith: '')
%

category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_escapePercents

	self
		assert: 'aa aa יי aa aa' encodeAsUTF8 escapePercents
			equals: 'aa%20aa%20%C3%A9%C3%A9%20aa%20aa';
		assert: 'aa aa יי aa aa' escapePercents
			equals: 'aa%20aa%20%E9%E9%20aa%20aa'
%

category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_findString_startingAt_caseSensitive_

	| string |
	string := 'abcABCdef'.
	self
		assert: (string findString: 'abc' startingAt: 1 caseSensitive: true) equals: 1;
		assert: (string findString: 'abc' startingAt: 1 caseSensitive: false) equals: 1;
		assert: (string findString: 'ABC' startingAt: 1 caseSensitive: true) equals: 4;
		assert: (string findString: 'ABC' startingAt: 1 caseSensitive: false) equals: 1;
		assert: (string findString: 'def' startingAt: 1 caseSensitive: true) equals: 7;
		assert: (string findString: 'def' startingAt: 1 caseSensitive: false) equals: 7;
		assert: (string findString: 'DEF' startingAt: 1 caseSensitive: true) equals: 0;
		assert: (string findString: 'DEF' startingAt: 1 caseSensitive: false) equals: 7
%

category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_gather_

	self
		assert: (#(1 2 3) gather: [:each | each * 10 + 1 to: each * 10 + each])
		equals: #(11 21 22 31 32 33)
%

category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_indexOfAnyOf_startingAt_

	self
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 1) equals: 5;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 6) equals: 6;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 7) equals: 14;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 15) equals: 15;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 16) equals: 20;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 21) equals: 0;
		assert: ('word, another, done.' indexOfAnyOf: '+-' startingAt: 1) equals: 0
%

category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_indexOfAnyOf_startingAt_ifAbsent_

	self
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 1 ifAbsent: [nil]) equals: 5;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 6 ifAbsent: [nil]) equals: 6;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 7 ifAbsent: [nil]) equals: 14;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 15 ifAbsent: [nil]) equals: 15;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 16 ifAbsent: [nil]) equals: 20;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 21 ifAbsent: [nil]) equals: nil;
		assert: ('word, another, done.' indexOfAnyOf: '+-' startingAt: 1 ifAbsent: [nil]) equals: nil
%

category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_intersection_

	self
		assert: (#(1 2 3) intersection: #()) sortAscending equals: #();
		assert: (#() intersection: #(1 2 3)) sortAscending equals: #();
		assert: (#(1 2 3) intersection: #(1 2 3)) sortAscending equals: #(1 2 3);
		assert: (#(1 2 3) intersection: #(2 3 4)) sortAscending equals: #(2 3);
		assert: (#(1 2 3) intersection: #(3 4 5)) sortAscending equals: #(3);
		assert: (#(1 2 3) intersection: #(4 5 6)) sortAscending equals: #()
%

category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_parseSelectorFrom_

	self
		assert: (UndefinedObject parseSelectorFrom: 'a') equals: 'a';
		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';
		assert: (UndefinedObject parseSelectorFrom: 'a self halt') equals: 'a';
		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';
		assert: (UndefinedObject parseSelectorFrom: 'a: something') equals: 'a:';
		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';
		assert: (UndefinedObject parseSelectorFrom: 'a: something b: else') equals: 'a:b:';
		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';
		assert: (UndefinedObject parseSelectorFrom: '= another ^false') equals: '=';
		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';
		assert: (UndefinedObject parseSelectorFrom: '=@= another ^false') equals: '=@=';
		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';

		assert: (UndefinedObject parseSelectorFrom: 'a ^undefined') equals: 'a';
		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';
		assert: (UndefinedObject parseSelectorFrom: 'a: something undefined := something') equals: 'a:';
		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';
		assert: (UndefinedObject parseSelectorFrom: 'a: something b: else ^something =@= else') equals: 'a:b:';
		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';
		assert: (UndefinedObject parseSelectorFrom: '= another ^undefined = another') equals: '=';
		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';
		assert: (UndefinedObject parseSelectorFrom: '=@= another ^undefined =@= another') equals: '=@=';
		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind'
%

category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_streamContents_

	self
		assert: (String streamContents: 
					[:stream |
					(1 to: 3) do: [:each | stream nextPutAll: each printString]
						separatedBy: [stream space]])
		equals: '1 2 3'
%

category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_unescapePercents

	self
		assert: 'aa%20aa%20%C3%A9%C3%A9%20aa%20aa'  unescapePercents asByteArray decodeFromUTF8 asString
			equals: 'aa aa יי aa aa';
		assert: 'aa%20aa%20%E9%E9%20aa%20aa' unescapePercents
			equals: 'aa aa יי aa aa' asUnicodeString
%

category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_withUnixLineEndings

	| lf cr crlf |
	lf := String with: Character lf.
	cr := String with: Character cr.
	crlf := cr, lf.

	self
		assert: (self unixLinesFrom: '') equals: #();
		assert: (self unixLinesFrom: 'abc') equals: #('abc');
		assert: (self unixLinesFrom: 'abc', lf) equals: #('abc');
		assert: (self unixLinesFrom: 'abc', cr) equals: #('abc');
		assert: (self unixLinesFrom: 'abc', crlf) equals: #('abc');
		assert: (self unixLinesFrom: 'abc', lf, cr) equals: #('abc' '');
		assert: (self unixLinesFrom: 'abc', cr, cr) equals: #('abc' '');
		assert: (self unixLinesFrom: 'abc', crlf, cr) equals: #('abc' '');
		assert: (self unixLinesFrom: 'abc', lf, lf) equals: #('abc' '');
		assert: (self unixLinesFrom: 'abc', crlf, lf) equals: #('abc' '');
		assert: (self unixLinesFrom: 'abc', lf, crlf) equals: #('abc' '');
		assert: (self unixLinesFrom: 'abc', cr, crlf) equals: #('abc' '');
		assert: (self unixLinesFrom: 'abc', crlf, crlf) equals: #('abc' '')
%

category: 'utility'
set compile_env: 0
method: CypressExtensionsTest
unixLinesFrom: aString
	
	| sourceStream resultStream |
	sourceStream := ReadStream on: aString withUnixLineEndings.
	resultStream := WriteStream on: Array new.
	[sourceStream atEnd]
		whileFalse: [resultStream nextPut: (sourceStream upTo: Character lf)].
	^resultStream contents.
%

! Class Implementation for CypressReferenceTest

! ------------------- Instance methods for CypressReferenceTest

category: 'running'
set compile_env: 0
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
set compile_env: 0
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
set compile_env: 0
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
set compile_env: 0
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
set compile_env: 0
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
set compile_env: 0
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
set compile_env: 0
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
set compile_env: 0
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
set compile_env: 0
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
set compile_env: 0
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
set compile_env: 0
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
set compile_env: 0
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
set compile_env: 0
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
set compile_env: 0
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
set compile_env: 0
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
set compile_env: 0
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
set compile_env: 0
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
set compile_env: 0
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
set compile_env: 0
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
set compile_env: 0
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
%



! End of Package: Cypress-Tests


