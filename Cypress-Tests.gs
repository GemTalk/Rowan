doit
(TestCase subclass: 'CypressAbstractTest'  instVarNames: #()  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Tests'
.
%
doit
(CypressAbstractTest subclass: 'CypressDefinitionTest'  instVarNames: #()  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Tests'
.
%
doit
(CypressAbstractTest subclass: 'CypressLoaderTest'  instVarNames: #()  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Tests'
.
%
doit
(CypressAbstractTest subclass: 'CypressPatchTest'  instVarNames: #()  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Tests'
.
%
doit
(CypressAbstractTest subclass: 'CypressSnapshotTest'  instVarNames: #()  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Tests'
.
%
doit
(CypressAbstractTest subclass: 'CypressStructureTest'  instVarNames: #()  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Tests'
.
%
doit
CypressAbstractTest immediateInvariant.
%
doit
CypressDefinitionTest immediateInvariant.
%
doit
CypressLoaderTest immediateInvariant.
%
doit
CypressPatchTest immediateInvariant.
%
doit
CypressSnapshotTest immediateInvariant.
%
doit
CypressStructureTest immediateInvariant.
%

! Remove existing behavior from CypressAbstractTest
doit
CypressAbstractTest removeAllMethods.
CypressAbstractTest class removeAllMethods.
%
! ------------------- Class methods for CypressAbstractTest
! ------------------- Instance methods for CypressAbstractTest
category: 'private'
set compile_env: 0
method: CypressAbstractTest
baseDefinitions
	| className |
	className := 'CypressMockBasic'.
	^{
		(CypressClassDefinition
        		name: className
       		 	superclassName: 'Object'
       			category: 'Cypress-Mocks'
                       	instVarNames: #('name')
			classInstVarNames: #('current')
        		comment: 'This mock contains basic class and instance method selectors').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: false
        		selector: 'extra'
        		category: 'accessing'
        		source:'extra
	"extra method"').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: false
        		selector: 'initialize'
        		category: 'initialization'
        		source:'initialize
	super initialize.
	self name: ''Unknown''').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: false
        		selector: 'name'
        		category: 'accessing'
        		source:'name
	^name').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: false
        		selector: 'name:'
        		category: 'accessing'
        		source:'name: aString
	name := aString').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: true
        		selector: 'current'
        		category: 'accessing'
        		source:'current
	^current').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: true
        		selector: 'current:'
        		category: 'accessing'
        		source:'current: anObject
	current := anObject').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: true
        		selector: 'initialize'
        		category: 'initialization'
        		source:'initialize
	self current: self new').
   	 	(CypressMethodDefinition
          		className: 'Object'
        		classIsMeta: false
        		selector: 'isCypressMockBasic'
        		category: '*Cypress-Mocks'
        		source:'isCypressMockBasic
	^false')
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
					"contents" : "accessing%0Aextra%0A%09%22extra%20method%22"
				 },
				{
					"name" : "initialize.st",
					"contents" : "initialization%0Ainitialize%0A%09super%20initialize.%0A%09self%20name%3A%20%27Unknown%27"
				 },
				{
					"name" : "name.st",
					"contents" : "accessing%0Aname%0A%09%5Ename"
				 },
				{
					"name" : "name..st",
					"contents" : "accessing%0Aname%3A%20aString%0A%09name%20%3A%3D%20aString"
				 }			],
			"class" : [
				{
					"name" : "current.st",
					"contents" : "accessing%0Acurrent%0A%09%5Ecurrent"
				 },
				{
					"name" : "current..st",
					"contents" : "accessing%0Acurrent%3A%20anObject%0A%09current%20%3A%3D%20anObject"
				 },
				{
					"name" : "initialize.st",
					"contents" : "initialization%0Ainitialize%0A%09self%20current%3A%20self%20new"
				 }			],
			"README.md" : "This%20mock%20contains%20basic%20class%20and%20instance%20method%20selectors",
			"properties.json" : {
				"classinstvars" : [
					"current" ],
				"instvars" : [
					"name" ],
				"name" : "CypressMockBasic",
				"super" : "Object" }
		 },
		{
			"name" : "Object.extension",
			"instance" : [
				{
					"name" : "isCypressMockBasic.st",
					"contents" : "%2ACypress-Mocks%0AisCypressMockBasic%0A%09%5Efalse"
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
		(CypressAddition 
			of: (CypressMethodDefinition
          			className: className
        			classIsMeta: false
        			selector: 'added'
        			category: 'accessing'
        			source:'added
	"added method"')).
		(CypressModification 
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
	name := aString')).
		(CypressRemoval 
			of: (CypressMethodDefinition
          			className: className
        			classIsMeta: false
        			selector: 'extra'
        			category: 'accessing'
        			source:'extra
	"extra method"')).
		(CypressRemoval 
			of: (CypressMethodDefinition
          			className: 'Object'
        			classIsMeta: false
        			selector: 'isCypressMockBasic'
        			category: '*Cypress-Mocks'
        			source:'isCypressMockBasic
	^false'))
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
		(CypressClassDefinition
        		name: className
       		 	superclassName: 'Object'
       			category: 'Cypress-Mocks'
                       	instVarNames: #('name')
			classInstVarNames: #('current')
        		comment: 'This mock contains basic class and instance method selectors').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: false
        		selector: 'added'
        		category: 'accessing'
        		source:'added
	"added method"').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: false
        		selector: 'initialize'
        		category: 'initialization'
        		source:'initialize
	super initialize.
	self name: ''Unknown''').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: false
        		selector: 'name'
        		category: 'accessing'
        		source:'name
	^name').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: false
        		selector: 'name:'
        		category: 'accessing'
        		source:'name: aString
	"changed method"
	name := aString').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: true
        		selector: 'current'
        		category: 'accessing'
        		source:'current
	^current').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: true
        		selector: 'current:'
        		category: 'accessing'
        		source:'current: anObject
	current := anObject').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: true
        		selector: 'initialize'
        		category: 'initialization'
        		source:'initialize
	self current: self new')
	}
%

! Remove existing behavior from CypressDefinitionTest
doit
CypressDefinitionTest removeAllMethods.
CypressDefinitionTest class removeAllMethods.
%
! ------------------- Class methods for CypressDefinitionTest
! ------------------- Instance methods for CypressDefinitionTest
category: 'testing'
set compile_env: 0
method: CypressDefinitionTest
testClassDefinition
	self assert: (CypressClassDefinition
		name: 'Foo'
       		 superclassName: 'Object'
       		category: 'Foo'
                instVarNames: #()
		classInstVarNames: #()
        	comment: '') printString =  'a CypressClassDefinition (Foo)'
%
category: 'testing'
set compile_env: 0
method: CypressDefinitionTest
testDictionaryOfDefinitions

	| dict |
	"baseDefinitions"
	dict := Dictionary new.
	self baseDefinitions do: [:each | 
		dict at: each put: each ].
	self baseDefinitions do: [:each | 
		self assert: (dict at: each) = each ].

	"targetDefinitions"
	dict := Dictionary new.
	self targetDefinitions do: [:each | 
		dict at: each put: each ].
	self targetDefinitions do: [:each | 
		self assert: (dict at: each) = each ].
%
category: 'testing'
set compile_env: 0
method: CypressDefinitionTest
testEquality
	| pkg1 pkg2 pkg3 name |
	name := 'Cypress-Mocks'.
	pkg1 := CypressPackageDefinition new name: name.
	pkg2 := CypressPackageDefinition new name: name.
	pkg3 := CypressPackageDefinition new name: 'Nope!'.

	self assert: pkg1 equals: pkg2.
	self deny: pkg1 = pkg3
%
category: 'testing'
set compile_env: 0
method: CypressDefinitionTest
testMethodDefinition
	self assert: (CypressMethodDefinition
		className: 'Foo'
		classIsMeta: false
		selector: 'isFoo'
		category: 'testing'
		source: 'isFoo ^true') printString = 'a CypressMethodDefinition (Foo>>isFoo)'
%
category: 'testing'
set compile_env: 0
method: CypressDefinitionTest
testNameEquality
	| pkg name |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition new name: name.
	self assert: pkg name equals: name.
	self deny: (pkg name = 'Nope.').
%
category: 'testing'
set compile_env: 0
method: CypressDefinitionTest
testPrintString
	| name pkg |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition new name: name.
	self assert: 'a CypressPackageDefinition(', name, ')' equals: pkg printString.
%

! Remove existing behavior from CypressLoaderTest
doit
CypressLoaderTest removeAllMethods.
CypressLoaderTest class removeAllMethods.
%
! ------------------- Class methods for CypressLoaderTest
! ------------------- Instance methods for CypressLoaderTest
category: 'running'
set compile_env: 0
method: CypressLoaderTest
tearDown

	| name |
	super tearDown.
	name := 'Cypress-Mocks'.
	(CypressSnapshot definitions: self baseDefinitions)
		 updatePackage: (CypressPackageDefinition new name: name)
%
category: 'testing'
set compile_env: 0
method: CypressLoaderTest
testLoad

	| name |
	name := 'Cypress-Mocks'.
	(CypressSnapshot definitions: self targetDefinitions)
		 updatePackage: (CypressPackageDefinition new name: name)
%

! Remove existing behavior from CypressPatchTest
doit
CypressPatchTest removeAllMethods.
CypressPatchTest class removeAllMethods.
%
! ------------------- Class methods for CypressPatchTest
! ------------------- Instance methods for CypressPatchTest
category: 'testing'
set compile_env: 0
method: CypressPatchTest
testDictionaryOfPatchOperations
	"loader uses dictionary for managing patch operations ... ensure that Amber Dictionaries stand up"

	| dict |
	dict := Dictionary new.
	self baseTargetPatch do: [:each | 
		dict at: each put: each ].
	self baseTargetPatch do: [:each | 
		self assert: (dict at: each) = each ].
%
category: 'testing'
set compile_env: 0
method: CypressPatchTest
testPatch
    | baseSnapshot targetSnapshot patch operations expected |
    baseSnapshot := CypressSnapshot definitions: self baseDefinitions.
    targetSnapshot := CypressSnapshot definitions: self targetDefinitions.
    patch := CypressPatch fromBase: baseSnapshot toTarget: targetSnapshot.
    operations := patch operations.
    self assert: operations size = 4.
    expected := self baseTargetPatch asArray.
    1 to: operations size do: [ :index | 
        | op |
        op := operations at: index.
        self assert: (expected includes: op) ]
%
category: 'testing'
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
	self assert: modification = modification.
	removal := CypressRemoval 
			of: (CypressMethodDefinition
          			className: className
        			classIsMeta: false
        			selector: 'extra'
        			category: 'accessing'
        			source:'extra
	"extra method"').
	self assert: removal = removal.
	addition := CypressAddition
			of: (CypressMethodDefinition
          			className: className
        			classIsMeta: false
        			selector: 'extra'
        			category: 'accessing'
        			source:'extra
	"extra method"').
	self assert: addition = addition.
%

! Remove existing behavior from CypressSnapshotTest
doit
CypressSnapshotTest removeAllMethods.
CypressSnapshotTest class removeAllMethods.
%
! ------------------- Class methods for CypressSnapshotTest
! ------------------- Instance methods for CypressSnapshotTest
category: 'testing'
set compile_env: 0
method: CypressSnapshotTest
testSnapshot
	| name pkg packageDefinitions expectedDefinitions |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition new name: name.
	packageDefinitions := pkg snapshot definitions.
	expectedDefinitions := self baseDefinitions.
	self assert: (expectedDefinitions size = packageDefinitions size).
	packageDefinitions do: [:def |
		(expectedDefinitions includes: def)
			ifFalse: [ self assert: false ]].
%
category: 'testing'
set compile_env: 0
method: CypressSnapshotTest
testSnapshotEquality
	| name pkg packageDefinitions expectedDefinitions |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition new name: name.
	packageDefinitions := pkg snapshot definitions.
	expectedDefinitions := self baseDefinitions.
	self assert: packageDefinitions asArray = expectedDefinitions asArray
%

! Remove existing behavior from CypressStructureTest
doit
CypressStructureTest removeAllMethods.
CypressStructureTest class removeAllMethods.
%
! ------------------- Class methods for CypressStructureTest
! ------------------- Instance methods for CypressStructureTest
category: 'private'
set compile_env: 0
method: CypressStructureTest
compileJSON: aJsonString

	^MCFileTreeJsonParser parse: aJsonString
%
category: 'tests'
set compile_env: 0
method: CypressStructureTest
testClassStructure

	| jsObject packageStructure classStructure classProperties |
	jsObject := self compileJSON: self basePackageStructureJson.
	packageStructure := CypressPackageStructure fromJs: jsObject.
	classStructure := packageStructure classes first.
	self assert: classStructure name = 'CypressMockBasic'.
	self deny: classStructure isClassExtension.
	self assert: classStructure comment = 'This mock contains basic class and instance method selectors'.
	classProperties := classStructure properties.
	self assert: classProperties size = 4.
	self assert: (classProperties at: 'instvars') = #('name').
	self assert: (classProperties at: 'classinstvars') = #('current').
	self assert: (classProperties at: 'name') = 'CypressMockBasic'.
	self assert: (classProperties at: 'super') = 'Object'.
	self assert: classStructure instanceMethods size = 4.
	self assert: classStructure classMethods size = 3.
	classStructure := packageStructure extensions first.
	self assert: classStructure name = 'Object'.
	self assert: classStructure isClassExtension.
	self assert: classStructure comment = ''.
	classProperties := classStructure properties.
	self assert: classProperties size = 1.
	self assert: (classProperties at: 'name') = 'Object'.
	self assert: classStructure instanceMethods size = 1.
	self assert: classStructure classMethods size = 0.
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
	self assert: packageStructure name = 'Cypress-Mocks.package'.
	self assert: packageStructure packageName = 'Cypress-Mocks'.
	self assert: packageStructure properties isEmpty.
	self assert: packageStructure extensions size = 1.
	self assert: packageStructure classes size = 1.
%
category: 'tests'
set compile_env: 0
method: CypressStructureTest
testPackageStructureFromPackage

	| packageStructure packageDefinitions expectedDefinitions |
	packageStructure := CypressPackageStructure fromPackage: (CypressPackageDefinition new name: 'Cypress-Mocks').
	packageDefinitions := packageStructure snapshot definitions.
	expectedDefinitions := self baseDefinitions.
	self assert: (expectedDefinitions size = packageDefinitions size).
	packageDefinitions do: [:def |
		(expectedDefinitions includes: def)
			ifFalse: [ 
				def inspect.
				self assert: false ]].
%
category: 'tests'
set compile_env: 0
method: CypressStructureTest
testPackageStructureSnapshot

	| packageStructure packageDefinitions expectedDefinitions |
	packageStructure := CypressPackageStructure fromJs: (self compileJSON: self basePackageStructureJson).
	packageDefinitions := packageStructure snapshot definitions.
	expectedDefinitions := self baseDefinitions.
	self assert: (expectedDefinitions size = packageDefinitions size).
	packageDefinitions do: [:def |
		(expectedDefinitions includes: def)
			ifFalse: [ 
				def inspect.
				self assert: false ]].
%
category: 'tests'
set compile_env: 0
method: CypressStructureTest
testPackageStructureToJson
    | packageStructure stream string expected x y |
    packageStructure := CypressPackageStructure fromPackage: (CypressPackageDefinition new name: 'Cypress-Mocks').
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
    self assert: expected = string
%
category: 'tests'
set compile_env: 0
method: CypressStructureTest
testPropertyDictionaryRead

	| propertyDictionary phoneNumbers |
	propertyDictionary := (self compileJSON: self sampleJson) asCypressPropertyObject.
	self assert: (propertyDictionary at: 'name') = 'John Smith'.
	self assert: (propertyDictionary at: 'age') = 25.
	self assert: (propertyDictionary at: 'registered').
	phoneNumbers := propertyDictionary at: 'phoneNumber'.
	self assert: phoneNumbers size = 2.
	self assert: ((phoneNumbers at: 1) at: 'number') = '212 555-1234'.
	self assert: ((phoneNumbers at: 2) at: 'number') = '646 555-4567'.
%
category: 'tests'
set compile_env: 0
method: CypressStructureTest
testPropertyDictionaryWrite
    | propertyDictionary stream x |
    propertyDictionary := (self compileJSON: self sampleJson) asCypressPropertyObject.
    stream := WriteStream on: String new.
    propertyDictionary writeCypressJsonOn: stream indent: 0.
    self assert: (x := stream contents withUnixLineEndings) = self sampleJson withUnixLineEndings
%
