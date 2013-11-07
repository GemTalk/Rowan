SystemOrganization addCategory: 'Cypress-Tests'!

TestCase subclass: #CypressAbstractTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Tests'!

!CypressAbstractTest class methodsFor: 'testing' stamp: ''!
isAbstract	^self sunitName = #CypressAbstractTest! !

!CypressAbstractTest methodsFor: 'private' stamp: ''!
baseDefinitions	| className |	className := 'CypressMockBasic'.	^{		CypressClassDefinition			name: className			superclassName: 'Object'			category: 'Cypress-Mocks-Definitions'			instVarNames: #('name')			classInstVarNames: #('current')			classVarNames: #('Something')			poolDictionaryNames: #()			comment: 'This mock contains basic class and instance method selectors'			subclassType: ''.		CypressMethodDefinition			className: className			classIsMeta: false			selector: 'extra'			category: 'accessing'			source: 'extra	"extra method"'.		CypressMethodDefinition			className: className			classIsMeta: false			selector: 'initialize'			category: 'initialization'			source: 'initialize	super initialize.	self name: ''Unknown'''.		CypressMethodDefinition			className: className			classIsMeta: false			selector: 'name'			category: 'accessing'			source: 'name	^name'.		CypressMethodDefinition			className: className			classIsMeta: false			selector: 'name:'			category: 'accessing'			source: 'name: aString	name := aString'.		CypressMethodDefinition			className: className			classIsMeta: true			selector: 'current'			category: 'accessing'			source: 'current	^current'.		CypressMethodDefinition			className: className			classIsMeta: true			selector: 'current:'			category: 'accessing'			source: 'current: anObject	current := anObject'.		CypressMethodDefinition			className: className			classIsMeta: true			selector: 'initialize'			category: 'initialization'			source: 'initialize	self current: self new'.		CypressMethodDefinition			className: 'Object'			classIsMeta: false			selector: 'isCypressMockBasic'			category: '*Cypress-Mocks-Extensions'		source: 'isCypressMockBasic	^false'.	}! !

!CypressAbstractTest methodsFor: 'private' stamp: ''!
targetDefinitions	"remove #extra method and modify #name: method"	| className |	className := 'CypressMockBasic'.	^{		CypressClassDefinition			name: className			superclassName: 'Object'			category: 'Cypress-Mocks-Definitions'			instVarNames: #('name')			classInstVarNames: #('current')			classVarNames: #()			poolDictionaryNames: #()			comment: 'This mock contains basic class and instance method selectors'			subclassType: ''.		CypressMethodDefinition			className: className			classIsMeta: false			selector: 'added'			category: 'accessing'			source: 'added	"added method"'.		CypressMethodDefinition			className: className			classIsMeta: false			selector: 'initialize'			category: 'initialization'			source: 'initialize	super initialize.	self name: ''Unknown'''.		CypressMethodDefinition			className: className			classIsMeta: false			selector: 'name'			category: 'accessing'			source: 'name	^name'.		CypressMethodDefinition			className: className			classIsMeta: false			selector: 'name:'			category: 'accessing'			source: 'name: aString	"changed method"	name := aString'.		CypressMethodDefinition			className: className			classIsMeta: true			selector: 'current'			category: 'accessing'			source: 'current	^current'.		CypressMethodDefinition			className: className			classIsMeta: true			selector: 'current:'			category: 'accessing'			source: 'current: anObject	current := anObject'.		CypressMethodDefinition			className: className			classIsMeta: true			selector: 'initialize'			category: 'initialization'			source: 'initialize	self current: self new'.		CypressClassDefinition			name: className , 'Sub'			superclassName: className			category: 'Cypress-Mocks'			instVarNames: #('anotherIV')			classInstVarNames: #('anotherCIV')			classVarNames: #()			poolDictionaryNames: #()			comment: 'Hacked subclass to test class loading and unloading'			subclassType: ''.		CypressMethodDefinition			className: className , 'Sub'			classIsMeta: false			selector: 'added'			category: 'accessing'			source: 'added	"added method"'.	}! !

CypressAbstractTest subclass: #CypressDefinitionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Tests'!

!CypressDefinitionTest methodsFor: 'tests' stamp: ''!
testClassDefinition	self		assert: (CypressClassDefinition					name: 'Foo'					superclassName: 'Object'					category: 'Foo'					instVarNames: #()					classInstVarNames: #()					classVarNames: #()					poolDictionaryNames: #()					comment: ''					subclassType: '') printString		equals: 'a CypressClassDefinition(Foo)'! !

!CypressDefinitionTest methodsFor: 'tests' stamp: ''!
testDictionaryOfDefinitions	| dict |	"baseDefinitions"	dict := Dictionary new.	self baseDefinitions do: [:each | 		dict at: each put: each ].	self baseDefinitions do: [:each | 		self assert: (dict at: each) equals: each ].	"targetDefinitions"	dict := Dictionary new.	self targetDefinitions do: [:each | 		dict at: each put: each ].	self targetDefinitions do: [:each | 		self assert: (dict at: each) equals: each ].! !

!CypressDefinitionTest methodsFor: 'tests' stamp: ''!
testEquality	| pkg1 pkg2 pkg3 name |	name := 'Cypress-Mocks'.	pkg1 := CypressPackageDefinition named: name.	pkg2 := CypressPackageDefinition named: name.	pkg3 := CypressPackageDefinition named: 'Nope!!'.	self assert: pkg1 equals: pkg2.	self deny: pkg1 equals: pkg3! !

!CypressDefinitionTest methodsFor: 'tests' stamp: ''!
testMethodDefinition	self assert: (CypressMethodDefinition		className: 'Foo'		classIsMeta: false		selector: 'isFoo'		category: 'testing'		source: 'isFoo ^true') printString equals: 'a CypressMethodDefinition(Foo>>isFoo)'! !

!CypressDefinitionTest methodsFor: 'tests' stamp: ''!
testNameEquality	| pkg name |	name := 'Cypress-Mocks'.	pkg := CypressPackageDefinition named: name.	self assert: pkg name equals: name.	self deny: pkg name equals: 'Nope.'.! !

!CypressDefinitionTest methodsFor: 'tests' stamp: ''!
testPrintString	| name pkg |	name := 'Cypress-Mocks'.	pkg := CypressPackageDefinition named: name.	self assert: 'a CypressPackageDefinition(', name, ')' equals: pkg printString.! !

CypressAbstractTest subclass: #CypressExtensionsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Tests'!

!CypressExtensionsTest methodsFor: 'tests' stamp: ''!
test_beginsWith_	self		assert: ('abc' beginsWith: 'a');		deny: ('abc' beginsWith: 'c');		assert: ('abc' beginsWith: 'abc');		deny: ('abc' beginsWith: 'abcx');		deny: ('abc' beginsWith: '');		deny: ('' beginsWith: 'abc');		deny: ('' beginsWith: '')! !

!CypressExtensionsTest methodsFor: 'tests' stamp: ''!
test_copyWithoutSuffix_	self		assert: ('abc' copyWithoutSuffix: 'c') equals: 'ab';		assert: ('abc' copyWithoutSuffix: 'a') equals: 'abc';		assert: ('abc' copyWithoutSuffix: 'abc') equals: '';		assert: ('abc' copyWithoutSuffix: 'xxxabc') equals: 'abc';		assert: ('abc' copyWithoutSuffix: '') equals: 'abc';		assert: ('' copyWithoutSuffix: 'abc') equals: '';		assert: ('' copyWithoutSuffix: '') equals: ''! !

!CypressExtensionsTest methodsFor: 'tests' stamp: ''!
test_copyWithoutSuffix_or_	self		assert: ('abc' copyWithoutSuffix: 'c' or: [nil]) equals: 'ab';		assert: ('abc' copyWithoutSuffix: 'a' or: [nil]) equals: nil;		assert: ('abc' copyWithoutSuffix: 'abc' or: [nil]) equals: '';		assert: ('abc' copyWithoutSuffix: 'xxxabc' or: [nil]) equals: nil;		assert: ('abc' copyWithoutSuffix: '' or: [nil]) equals: nil;		assert: ('' copyWithoutSuffix: 'abc' or: [nil]) equals: nil;		assert: ('' copyWithoutSuffix: '' or: [nil]) equals: nil! !

!CypressExtensionsTest methodsFor: 'tests' stamp: ''!
test_difference_	"Set theoretic difference means elements from the first collection not in the second."	self		assert: (#(1 2 3) difference: #()) sortAscending equals: #(1 2 3);		assert: (#() difference: #(1 2 3)) sortAscending equals: #();		assert: (#(1 2 3) difference: #(1 2 3)) sortAscending equals: #();		assert: (#(1 2 3) difference: #(2 3 4)) sortAscending equals: #(1);		assert: (#(1 2 3) difference: #(3 4 5)) sortAscending equals: #(1 2);		assert: (#(1 2 3) difference: #(4 5 6)) sortAscending equals: #(1 2 3)! !

!CypressExtensionsTest methodsFor: 'tests' stamp: ''!
test_endsWith_	self		assert: ('abc' endsWith: 'c');		deny: ('abc' endsWith: 'a');		assert: ('abc' endsWith: 'abc');		deny: ('abc' endsWith: 'xabc');		deny: ('abc' endsWith: '');		deny: ('' endsWith: 'abc');		deny: ('' endsWith: '')! !

!CypressExtensionsTest methodsFor: 'tests' stamp: ''!
test_findString_startingAt_caseSensitive_	| string |	string := 'abcABCdef'.	self		assert: (string findString: 'abc' startingAt: 1 caseSensitive: true) equals: 1;		assert: (string findString: 'abc' startingAt: 1 caseSensitive: false) equals: 1;		assert: (string findString: 'ABC' startingAt: 1 caseSensitive: true) equals: 4;		assert: (string findString: 'ABC' startingAt: 1 caseSensitive: false) equals: 1;		assert: (string findString: 'def' startingAt: 1 caseSensitive: true) equals: 7;		assert: (string findString: 'def' startingAt: 1 caseSensitive: false) equals: 7;		assert: (string findString: 'DEF' startingAt: 1 caseSensitive: true) equals: 0;		assert: (string findString: 'DEF' startingAt: 1 caseSensitive: false) equals: 7! !

!CypressExtensionsTest methodsFor: 'tests' stamp: ''!
test_gather_	self		assert: (#(1 2 3) gather: [:each | each * 10 + 1 to: each * 10 + each])		equals: #(11 21 22 31 32 33)! !

!CypressExtensionsTest methodsFor: 'tests' stamp: ''!
test_indexOfAnyOf_startingAt_	self		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 1) equals: 5;		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 6) equals: 6;		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 7) equals: 14;		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 15) equals: 15;		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 16) equals: 20;		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 21) equals: 0;		assert: ('word, another, done.' indexOfAnyOf: '+-' startingAt: 1) equals: 0! !

!CypressExtensionsTest methodsFor: 'tests' stamp: ''!
test_indexOfAnyOf_startingAt_ifAbsent_	self		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 1 ifAbsent: [nil]) equals: 5;		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 6 ifAbsent: [nil]) equals: 6;		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 7 ifAbsent: [nil]) equals: 14;		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 15 ifAbsent: [nil]) equals: 15;		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 16 ifAbsent: [nil]) equals: 20;		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 21 ifAbsent: [nil]) equals: nil;		assert: ('word, another, done.' indexOfAnyOf: '+-' startingAt: 1 ifAbsent: [nil]) equals: nil! !

!CypressExtensionsTest methodsFor: 'tests' stamp: ''!
test_intersection_	self		assert: (#(1 2 3) intersection: #()) sortAscending equals: #();		assert: (#() intersection: #(1 2 3)) sortAscending equals: #();		assert: (#(1 2 3) intersection: #(1 2 3)) sortAscending equals: #(1 2 3);		assert: (#(1 2 3) intersection: #(2 3 4)) sortAscending equals: #(2 3);		assert: (#(1 2 3) intersection: #(3 4 5)) sortAscending equals: #(3);		assert: (#(1 2 3) intersection: #(4 5 6)) sortAscending equals: #()! !

!CypressExtensionsTest methodsFor: 'tests' stamp: ''!
test_parseSelectorFrom_	self		assert: (UndefinedObject parseSelectorFrom: 'a') equals: 'a';		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';		assert: (UndefinedObject parseSelectorFrom: 'a self halt') equals: 'a';		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';		assert: (UndefinedObject parseSelectorFrom: 'a: something') equals: 'a:';		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';		assert: (UndefinedObject parseSelectorFrom: 'a: something b: else') equals: 'a:b:';		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';		assert: (UndefinedObject parseSelectorFrom: '= another ^false') equals: '=';		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';		assert: (UndefinedObject parseSelectorFrom: '=@= another ^false') equals: '=@=';		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';		assert: (UndefinedObject parseSelectorFrom: 'a ^undefined') equals: 'a';		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';		assert: (UndefinedObject parseSelectorFrom: 'a: something undefined := something') equals: 'a:';		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';		assert: (UndefinedObject parseSelectorFrom: 'a: something b: else ^something =@= else') equals: 'a:b:';		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';		assert: (UndefinedObject parseSelectorFrom: '= another ^undefined = another') equals: '=';		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';		assert: (UndefinedObject parseSelectorFrom: '=@= another ^undefined =@= another') equals: '=@=';		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind'! !

!CypressExtensionsTest methodsFor: 'tests' stamp: ''!
test_streamContents_	self		assert: (String streamContents: 					[:stream |					(1 to: 3) do: [:each | stream nextPutAll: each printString]						separatedBy: [stream space]])		equals: '1 2 3'! !

!CypressExtensionsTest methodsFor: 'tests' stamp: ''!
test_withUnixLineEndings	| lf cr crlf |	lf := String with: Character lf.	cr := String with: Character cr.	crlf := cr, lf.	self		assert: (self unixLinesFrom: '') equals: #();		assert: (self unixLinesFrom: 'abc') equals: #('abc');		assert: (self unixLinesFrom: 'abc', lf) equals: #('abc');		assert: (self unixLinesFrom: 'abc', cr) equals: #('abc');		assert: (self unixLinesFrom: 'abc', crlf) equals: #('abc');		assert: (self unixLinesFrom: 'abc', lf, cr) equals: #('abc' '');		assert: (self unixLinesFrom: 'abc', cr, cr) equals: #('abc' '');		assert: (self unixLinesFrom: 'abc', crlf, cr) equals: #('abc' '');		assert: (self unixLinesFrom: 'abc', lf, lf) equals: #('abc' '');		assert: (self unixLinesFrom: 'abc', crlf, lf) equals: #('abc' '');		assert: (self unixLinesFrom: 'abc', lf, crlf) equals: #('abc' '');		assert: (self unixLinesFrom: 'abc', cr, crlf) equals: #('abc' '');		assert: (self unixLinesFrom: 'abc', crlf, crlf) equals: #('abc' '')! !

!CypressExtensionsTest methodsFor: 'utility' stamp: ''!
unixLinesFrom: aString		| sourceStream resultStream |	sourceStream := ReadStream on: aString withUnixLineEndings.	resultStream := WriteStream on: Array new.	[sourceStream atEnd]		whileFalse: [resultStream nextPut: (sourceStream upTo: Character lf)].	^resultStream contents.! !

CypressAbstractTest subclass: #CypressLoaderTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Tests'!

!CypressLoaderTest methodsFor: 'utility' stamp: ''!
erroneousClassDefinitions	^{		CypressClassDefinition			name: 'CypressMockBasicSubclassWithDuplicateInstanceVariable'			superclassName: 'CypressMockBasic'			category: 'Cypress-Mocks'			instVarNames: #('name')			classInstVarNames: #()			classVarNames: #()			poolDictionaryNames: #()			comment: 'This class tries to define an instance variable which already exists in the superclass.'			subclassType: ''.	}! !

!CypressLoaderTest methodsFor: 'running' stamp: ''!
tearDown	| name |	super tearDown.	name := 'Cypress-Mocks'.	(CypressSnapshot definitions: self baseDefinitions)		 updatePackage: (CypressPackageDefinition named: name)! !

!CypressLoaderTest methodsFor: 'tests' stamp: ''!
testLoad	| name loader |	name := 'Cypress-Mocks'.	loader := (CypressSnapshot definitions: self targetDefinitions)				updatePackage: (CypressPackageDefinition named: name).	self		assert: loader additions size equals: 5;		assert: loader removals size equals: 2;		assert: loader unloadable size equals: 0;		assert: loader provisions notEmpty			description: 'There should have been a number of classes provided by the system';		assert: loader errors size equals: 0;		assert: loader methodAdditions size equals: 0;		assert: loader requirements isEmpty			description: 'There should have been no external requirements'! !

!CypressLoaderTest methodsFor: 'tests' stamp: ''!
testLoaderWithClassDefinitionError	| name loader |	name := 'Cypress-Mocks'.	self		should: 			[(CypressSnapshot				definitions: self erroneousClassDefinitions , self targetDefinitions)					updatePackage: (CypressPackageDefinition named: name)]		raise: CypressLoaderErrorNotification		description: 'There should have been a class definition with an error'.	loader := 			[(CypressSnapshot				definitions: self erroneousClassDefinitions , self targetDefinitions)					updatePackage: (CypressPackageDefinition named: name)]					on: CypressLoaderError					do: 						[:ex |						self							assert: ex patchOperation definition name							equals: 'CypressMockBasicSubclassWithDuplicateInstanceVariable'.						ex resume].	self		assert: loader additions size equals: 4;		assert: loader removals size equals: 2;		assert: loader unloadable size equals: 0;		assert: (loader unloadable gather: [:each | each]) size equals: 0;		assert: loader provisions notEmpty			description: 'There should have been a number of classes provided by the system';		assert: loader errors size equals: 1;		assert: loader methodAdditions size equals: 0;		assert: loader requirements size equals: 0! !

!CypressLoaderTest methodsFor: 'tests' stamp: ''!
testLoaderWithUnloadable	| name loader |	name := 'Cypress-Mocks'.	self		should: 			[(CypressSnapshot				definitions: self unloadableDefinitions , self targetDefinitions)					updatePackage: (CypressPackageDefinition named: name)]		raise: CypressLoaderMissingClasses		description: 'There should have been some unloadable definitions'.	loader := 			[(CypressSnapshot				definitions: self unloadableDefinitions , self targetDefinitions)					updatePackage: (CypressPackageDefinition named: name)]					on: CypressLoaderMissingClasses					do: 						[:ex |						self							assert: #('AnotherNonExistentClass' 'NonExistentClass' 'UnloadableClass')							equals: ex requirementsMap keys asSortedCollection asArray.						ex resume].	self		assert: loader additions size equals: 5;		assert: loader removals size equals: 2;		assert: loader unloadable size equals: 3;		assert: (loader unloadable gather: [:each | each]) size equals: 3;		assert: loader provisions notEmpty			description: 'There should have been a number of classes provided by the system';		assert: loader errors size equals: 0;		assert: loader methodAdditions size equals: 0;		assert: loader requirements size equals: 2! !

!CypressLoaderTest methodsFor: 'utility' stamp: ''!
unloadableDefinitions	^{		CypressClassDefinition			name: 'UnloadableClass'			superclassName: 'NonExistentClass'			category: 'Cypress-Mocks'			instVarNames: #()			classInstVarNames: #()			classVarNames: #()			poolDictionaryNames: #()			comment: 'This class depends on a class that is intended to be missing.'			subclassType: ''.		CypressMethodDefinition			className: 'UnloadableClass'			classIsMeta: false			selector: 'unloadable'			category: 'accessing'			source: 'unloadable	"This method cannot be loaded because the class cannot be."'.		CypressMethodDefinition			className: 'AnotherNonExistentClass'			classIsMeta: false			selector: 'unloadable'			category: 'accessing'			source: 'unloadable	"This method cannot be loaded because the class cannot be."'.	}! !

CypressAbstractTest subclass: #CypressPackageManagerTest
	instanceVariableNames: 'currentManager repositoriesToCleanUp classesToCleanUp methodsToCleanUp categoriesToCleanUp'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Tests'!

!CypressPackageManagerTest methodsFor: 'private' stamp: ''!
addClassNamed: className under: superClass to: aSymbolDictionary inCatgeory: aString	| newClass |	newClass := (superClass				subclass: className				instVarNames: #()				classVars: #()				classInstVars: #()				poolDictionaries: #()				inDictionary: aSymbolDictionary				options: #()) category: aString.	self classToCleanUp: newClass from: aSymbolDictionary.	^newClass! !

!CypressPackageManagerTest methodsFor: 'private' stamp: ''!
addMethodNamed: selector body: bodyString toClass: aClass inCategory: aString	| newMethod |	(aClass includesCategory: aString)		ifFalse: [self categoryToCleanUp: aString from: aClass].	newMethod := aClass				compileMethod: selector , '   ' , bodyString				dictionaries: System myUserProfile symbolList				category: aString				environmentId: 0.	self methodToCleanUp: newMethod.	^newMethod! !

!CypressPackageManagerTest methodsFor: 'private' stamp: ''!
categoryToCleanUp: aSymbol from: aClass	categoriesToCleanUp add: aSymbol -> aClass! !

!CypressPackageManagerTest methodsFor: 'private' stamp: ''!
classToCleanUp: aClass from: aSymbolDictionary	classesToCleanUp add: aClass -> aSymbolDictionary! !

!CypressPackageManagerTest methodsFor: 'private' stamp: ''!
classToCleanUpNamed: className from: aSymbolDictionary	classesToCleanUp add: (aSymbolDictionary at: className asSymbol) -> aSymbolDictionary! !

!CypressPackageManagerTest methodsFor: 'set up / teardown' stamp: ''!
cleanUpCategories	categoriesToCleanUp reverseDo: [:each | each value removeCategory: each key]! !

!CypressPackageManagerTest methodsFor: 'set up / teardown' stamp: ''!
cleanUpClasses	classesToCleanUp reverseDo: [:each | each value removeKey: each key name]! !

!CypressPackageManagerTest methodsFor: 'set up / teardown' stamp: ''!
cleanUpMethods	methodsToCleanUp		reverseDo: [:each | each methodClass removeSelector: each selector]! !

!CypressPackageManagerTest methodsFor: 'set up / teardown' stamp: ''!
cleanUpRepositories	repositoriesToCleanUp reverseDo: 			[:each |			CypressFileUtilities current deleteAll: each directoryPath.			GsFile removeServerDirectory: each directoryPath]! !

!CypressPackageManagerTest methodsFor: 'private' stamp: ''!
createEmptyManager	^CypressPackageManager2 new.! !

!CypressPackageManagerTest methodsFor: 'private' stamp: ''!
createFilesFor: aDictionary	"aDictionay is a mapping of repository file names to their expected contents."	aDictionary keysAndValuesDo: 			[:aPathName :fileContents |			| filename directoryPath file |			filename := CypressFileUtilities current localNameFrom: aPathName.			directoryPath := aPathName copyWithoutSuffix: filename.			CypressFileUtilities current ensureDirectoryExists: directoryPath.			file := GsFile openWriteOnServer: aPathName.			[file nextPutAll: fileContents] ensure: [file close]]! !

!CypressPackageManagerTest methodsFor: 'private' stamp: ''!
createFileTreeCompatibleTestRepoNamed: aString	| repo |	repo := currentManager				createRepositoryNamed: aString				under: '/tmp'				alias: aString				schema: 'cypressft:'.	self repositoryToCleanUp: repo.	^repo! !

!CypressPackageManagerTest methodsFor: 'private' stamp: ''!
createFileTreeReadOnlyTestRepoNamed: aString	| repo |	repo := currentManager				createRepositoryNamed: aString				under: '/tmp'				alias: aString				schema: 'cypressfiletree:'.	self repositoryToCleanUp: repo.	^repo! !

!CypressPackageManagerTest methodsFor: 'private' stamp: ''!
createManagerFromImage	^CypressPackageManager2 create.! !

!CypressPackageManagerTest methodsFor: 'private' stamp: ''!
createManagerWithUnknownPackages: someNames	| manager |	manager := self createEmptyManager.	someNames do: [:each | manager addUnknownPackageNamed: each].	^manager! !

!CypressPackageManagerTest methodsFor: 'private' stamp: ''!
createTestRepoNamed: aString	| repo |	repo := currentManager				createRepositoryNamed: aString				under: '/tmp'				alias: aString				schema: 'cypress:'.	self repositoryToCleanUp: repo.	^repo! !

!CypressPackageManagerTest methodsFor: 'private' stamp: ''!
currentPackageInformationGroups	^currentManager packageInformationList		inject: Dictionary new		into: 			[:dict :each |			(dict at: each class ifAbsentPut: [OrderedCollection new]) add: each.			dict]! !

!CypressPackageManagerTest methodsFor: 'expected results' stamp: ''!
cypressFormatXYPackageDirectoryStructure	^(Dictionary new)		at: '/tmp/CypressTestRepo/properties.ston'			put: '{	"alias" : "CypressTestRepo",	"_cypress_copyright" : "This%20work%20is%20protected%20by%20copyright.%20All%20rights%20reserved.",	"_gs_fileout" : "false",	"_gs_format" : "Cypress",	"_gs_strict" : "true" }';		at: '/tmp/CypressTestRepo/X-Y.package/Object.extension/instance/isXY.st'			put: '"notice: This work is protected by copyright. All rights reserved.category: *X-Y-testing"isXY   ^false';		at: '/tmp/CypressTestRepo/X-Y.package/Object.extension/properties.ston'			put: '{	"name" : "Object" }';		at: '/tmp/CypressTestRepo/X-Y.package/properties.ston' put: '{	 }';		at: '/tmp/CypressTestRepo/X-Y.package/XYClass.class/instance/stub.st'			put: '"notice: This work is protected by copyright. All rights reserved.category: dummy"stub   ^nil';		at: '/tmp/CypressTestRepo/X-Y.package/XYClass.class/properties.ston'			put: '{	"category" : "X-Y",	"classinstvars" : [		 ],	"classvars" : [		 ],	"instvars" : [		 ],	"name" : "XYClass",	"pools" : [		 ],	"super" : "Object" }';		at: '/tmp/CypressTestRepo/X-Y.package/XYClass.class/README.md' put: '';		yourself! !

!CypressPackageManagerTest methodsFor: 'expected results' stamp: ''!
fileTreeFormatXYPackageDirectoryStructure	^(Dictionary new)		at: '/tmp/CypressTestRepo/properties.json'			put: '{	"alias" : "CypressTestRepo",	"_cypress_copyright" : "This%20work%20is%20protected%20by%20copyright.%20All%20rights%20reserved.",	"_gs_fileout" : "false",	"_gs_format" : "FileTree",	"_gs_strict" : "false" }';		at: '/tmp/CypressTestRepo/X-Y.package/Object.extension/instance/isXY.st'			put: '*X-Y-testingisXY   ^false';		at: '/tmp/CypressTestRepo/X-Y.package/Object.extension/properties.ston'			put: '{	"name" : "Object" }';		at: '/tmp/CypressTestRepo/X-Y.package/properties.ston' put: '{	 }';		at: '/tmp/CypressTestRepo/X-Y.package/XYClass.class/instance/stub.st'			put: 'dummystub   ^nil';		at: '/tmp/CypressTestRepo/X-Y.package/XYClass.class/properties.ston'			put: '{	"category" : "X-Y",	"classinstvars" : [		 ],	"classvars" : [		 ],	"instvars" : [		 ],	"name" : "XYClass",	"pools" : [		 ],	"super" : "Object" }';		at: '/tmp/CypressTestRepo/X-Y.package/XYClass.class/README.md' put: '';		yourself! !

!CypressPackageManagerTest methodsFor: 'private' stamp: ''!
methodToCleanUp: aCompiledMethod	methodsToCleanUp add: aCompiledMethod! !

!CypressPackageManagerTest methodsFor: 'expected results' stamp: ''!
readOnlyFileTreeFormatXYPackageDirectoryStructure	"We cannot commit to a true FileTree repository, so there should be no files created."	^(Dictionary new)		at: '/tmp/CypressTestRepo/properties.json'			put: '{	"alias" : "CypressTestRepo",	"_cypress_copyright" : "This%20work%20is%20protected%20by%20copyright.%20All%20rights%20reserved.",	"_gs_fileout" : "false",	"_gs_format" : "FileTree",	"_gs_strict" : "true" }';		yourself! !

!CypressPackageManagerTest methodsFor: 'private' stamp: ''!
repositoryToCleanUp: aRepository	repositoriesToCleanUp add: aRepository! !

!CypressPackageManagerTest methodsFor: 'set up / teardown' stamp: ''!
setUp	repositoriesToCleanUp := OrderedCollection new.	classesToCleanUp := OrderedCollection new.	methodsToCleanUp := OrderedCollection new.	categoriesToCleanUp := OrderedCollection new.! !

!CypressPackageManagerTest methodsFor: 'set up / teardown' stamp: ''!
tearDown	self		cleanUpMethods;		cleanUpClasses;		cleanUpCategories;		cleanUpRepositories! !

!CypressPackageManagerTest methodsFor: 'tests' stamp: ''!
testAttachingNewCypressRepositoryToUnknownImagePackages	| repo groups old new |	currentManager := self createManagerWithUnknownPackages: #('X' 'X-Y' 'X-Y-A' 'X-Y-B' 'X-Y-C' 'X-Z').	groups := self currentPackageInformationGroups.	self		assert: currentManager knownRepositories isEmpty			description: 'Newly created Package Manager should not know about any repositories';		assert: currentManager packageInformationList size equals: 6;		assert: groups size equals: 1;		assert: groups keys asArray first equals: CypressUnknownPackageInformation.		repo := self createTestRepoNamed: 'CypressTestRepo'.	old := currentManager packageInformationNamed: 'X-Y'.	self deny: old isKnown description: 'Newly defined packages are supposed to be Unknown'.	currentManager assignRepository: repo to: old.	new := currentManager packageInformationNamed: 'X-Y'.	groups := self currentPackageInformationGroups.	self		assert: currentManager knownRepositories size = 1			description: 'After assigning a repository, there should be at least one known';		assert: (currentManager knownRepositories keys collect: [:each | each printString])			equals: (Set with: 'cypress:///tmp/CypressTestRepo/');		assert: currentManager packageInformationList size = 6			description: 'There should be the same number of Package Information objects';		assert: groups size equals: 4;		assert: (groups at: CypressUnknownPackageInformation) size = 1			description: 'There should have been one Package Information left as Unknown';		assert: (groups at: CypressUnknownPackageInformation) first name			equals: 'X-Z';		assert: (groups at: CypressKnownPackageInformation) size = 1			description: 'There should have been one Package Information left as Known';		assert: (groups at: CypressKnownPackageInformation) first name			equals: 'X-Y';		assert: (groups at: CypressEclipsedPackageInformation) size = 3			description: 'There should have been one Package Information left as Eclipsed';		assert: ((groups at: CypressEclipsedPackageInformation)					collect: [:each | each name]) asSet			equals: (Set with: 'X-Y-A' with: 'X-Y-B' with: 'X-Y-C');		assert: (groups at: CypressConflictingPackageInformation) size = 1			description: 'There should have been one Package Information left as Conflicting';		assert: (groups at: CypressConflictingPackageInformation) first name			equals: 'X';		assert: new repositories size = 1			description: 'After assigning a repository to a newly known package, it should have one repository';		assert: new repositories any 			equals: repo;		assert: (new digestFor: repo url)			equals: (CypressPackageStructure named: new name, '.package') digest! !

!CypressPackageManagerTest methodsFor: 'tests' stamp: ''!
testCreatingAndRetrievingNamedPackageManager	| original retrieved key savedPackageManagers |	key := DateAndTime now printString.	original := CypressPackageManager2 create.	savedPackageManagers := CypressPackageManager2 savedPackageManagers copy.		[original saveAs: key.	retrieved := CypressPackageManager2 named: key.	self assert: original == retrieved		description: 'a retrieved instance should be identical to the original']			ensure: [CypressPackageManager2 removePackageManagerSavedAs: key].	self assert: CypressPackageManager2 savedPackageManagers		equals: savedPackageManagers! !

!CypressPackageManagerTest methodsFor: 'tests' stamp: ''!
testCreatingNewCypressRepository	| repo |	currentManager := self createManagerFromImage.	repo := self createTestRepoNamed: 'CypressTestRepo'.	self		assert: repo directoryPath			equals: '/tmp/CypressTestRepo/';		assert: repo packageNames isEmpty			description: 'a new repository should be empty of packages';		assert: repo isCodeFormatCypress			equals: true;		assert: repo isCodeFormatStrict			equals: true;		assert: repo areGemStoneFileoutsEnabled			equals: false! !

!CypressPackageManagerTest methodsFor: 'tests' stamp: ''!
testCypressUrls	| url |	#(		( 'cypress:/a/b/c'			'Cypress'	true		'cypress:///a/b/c'			#('a' 'b' 'c') )		( 'cypresslax:/a/b/c'		'Cypress'	false	'cypresslax:///a/b/c'		#('a' 'b' 'c') )		( 'cypressft:/a/b/c'		'FileTree'	false	'cypressft:///a/b/c'		#('a' 'b' 'c') )		( 'cypressfiletree:/a/b/c'	'FileTree'	true		'cypressfiletree:///a/b/c'	#('a' 'b' 'c') )	) do: [:tuple |		url := CypressAbstractFileUrl absoluteFromText: tuple first.		self			assert: url printString equals: (tuple at: 4);			assert: url codeFormat equals: (tuple at: 2);			assert: url isStrict equals: (tuple at: 3);			assert: url path equals: (tuple at: 5);			assert: url host equals: ''	].	url := CypressAbstractFileUrl absoluteFromText: '/a/b/c/'.	self		assert: url printString equals: 'file:///a/b/c/';		assert: url path equals: #('a' 'b' 'c' '');		assert: url host equals: ''.	url := CypressAbstractFileUrl absoluteFromText: 'polution:/a/b/c/'.	self		assert: url printString equals: 'polution:/a/b/c/';		assert: url schemeName equals: 'polution';		assert: url locator equals: '/a/b/c/'! !

!CypressPackageManagerTest methodsFor: 'tests' stamp: ''!
testDetectingChangedImagePackage	| old repo repoDigest newClass |	self testAttachingNewCypressRepositoryToUnknownImagePackages.	old := currentManager packageInformationNamed: 'X-Y'.	repo := old repositories any.	repoDigest := old digestFor: repo url.	newClass := self addClassNamed: 'XYClass' under: Object to: UserGlobals inCatgeory: 'X-Y'.	self addMethodNamed: 'stub' body: '' toClass: newClass inCategory: 'dummy'.	self		assert: old repositories size = 1			description: 'There should have been only one repository for the package';		assert: repoDigest			equals: nil;		assert: old imageDigest			equals: nil;		assert: (old updateDigestsFromImage; imageDigest) notNil			description: 'After adding code to the package, the digest should be non-nil'! !

!CypressPackageManagerTest methodsFor: 'tests' stamp: ''!
testInitialPopulationOfPackageInformation	"A clean ab initio start up will only have Unknown packages	 (or potential packages) based on the categories used in the image."	currentManager := self createManagerFromImage.	self		assert: currentManager knownRepositories isEmpty			description: 'Newly created Package Manager should not know about any repositories';		assert: currentManager packageInformationList notEmpty			description: 'It is impossible for an image to have no categorized classes';		assert: (currentManager packageInformationList collect: [:each | each class]) asSet			equals: (Set with: CypressUnknownPackageInformation)! !

!CypressPackageManagerTest methodsFor: 'tests' stamp: ''!
testKnowingPackagesFromExistingRepository	"The repository used here is not viable for the long run.	 It is being used to defer creating the tooling to create a directory with	 controlled contents, instead of creating the needed functionality."	| repo groups packageNames old new |	currentManager := self createManagerFromImage.	groups := self currentPackageInformationGroups.	repo := currentManager 		repositoryOn: 'cypressfiletree:/export/galbadia1/users/rsargent/git/gitRepos/' asUrl		alias: 'Test Repo - Cypress format'.	packageNames := repo packageNames.	self assert: packageNames asSet equals: #('Gofer-Core' 'Gofer-Tests' 'NetworkTests' 'Network-Url') asSet.	old := (packageNames select: [:each | each beginsWith: 'Network'])		collect: [:each | currentManager packageInformationNamed: each].	self		assert: currentManager knownRepositories size = 1			description: 'After defining a repository, there should be at least one known';		assert: (currentManager knownRepositories keys collect: [:each | each printString])			equals: (Set with: 'cypressfiletree:///export/galbadia1/users/rsargent/git/gitRepos/');		assert: groups size equals: 1;		assert: (groups at: CypressUnknownPackageInformation) notEmpty			description: 'There should only beUnknown Package Information objects';		assert: old size = 2			description: 'There should be two "Network" packages already in the image';		assert: (old allSatisfy: [:each | each repositories isEmpty])			description: 'The packages in the image should still be unknown without a repository'.	currentManager lookForPackagesInRepository: repo.	new := packageNames collect: [:each | currentManager packageInformationNamed: each].	old := old collect: [:each | currentManager packageInformationNamed: each name].	groups := self currentPackageInformationGroups.	self		assert: new size = 4			description: 'There should be four Package Information objects that were updated';		assert: groups size equals: 4;		assert: (groups at: CypressUnknownPackageInformation) notEmpty			description: 'There should still be some Package Information objects left as Unknown';		assert: (groups at: CypressKnownPackageInformation) size = new size			description: 'There should have been one Known Package Information per package in the repository';		assert: (groups at: CypressKnownPackageInformation) asSet			equals: new asSet;		assert: (old allSatisfy: [:each | each imageDigest notNil])			description: 'Existing packages should have an image digest after connecting to a repository for them'! !

!CypressPackageManagerTest methodsFor: 'tests - loading - To Do' stamp: ''!
testLoadingPackageBranchFromRepository! !

!CypressPackageManagerTest methodsFor: 'tests' stamp: ''!
testLoadingPackageFromCypressRepository	| repo new summary |	currentManager := self createEmptyManager.	repo := self createTestRepoNamed: 'CypressTestRepo'.	self createFilesFor: self cypressFormatXYPackageDirectoryStructure.	self assert: repo packageNames asSet equals: #('X-Y') asSet.	currentManager lookForPackagesInRepository: repo.	new := currentManager packageInformationNamed: 'X-Y'.	[summary := currentManager loadPackageFrom: new inRepository: repo]		ensure: [	self classToCleanUpNamed: 'XYClass' from: UserGlobals.	self methodToCleanUp: (Object compiledMethodAt: #isXY).	self categoryToCleanUp: '*X-Y-testing' from: Object].	self		assert: summary isEmpty			description: 'This package should have loaded without problems';		assert: (UserGlobals includesKey: #XYClass)			 description: 'Should have loaded class named XYClass';		assert: (Object includesCategory: '*X-Y-testing')			description: 'Should have loaded a method into *X-Y-testing category on Object';		assert: (Object includesSelector: #isXY)			description: 'Should have loaded the method Object>>#isXY';		assert: (Object selectorsIn: '*X-Y-testing') asSet			equals: (Set with: #isXY)! !

!CypressPackageManagerTest methodsFor: 'tests' stamp: ''!
testLoadingPackageFromFileTreeRepository	| repo new summary |	currentManager := self createEmptyManager.	repo := self createFileTreeCompatibleTestRepoNamed: 'CypressTestRepo'.	self createFilesFor: self fileTreeFormatXYPackageDirectoryStructure.	self assert: repo packageNames asSet equals: #('X-Y') asSet.	currentManager lookForPackagesInRepository: repo.	new := currentManager packageInformationNamed: 'X-Y'.	[summary := currentManager loadPackageFrom: new inRepository: repo]		ensure: [	self classToCleanUpNamed: 'XYClass' from: UserGlobals.	self methodToCleanUp: (Object compiledMethodAt: #isXY).	self categoryToCleanUp: '*X-Y-testing' from: Object].	self		assert: summary isEmpty			description: 'This package should have loaded without problems';		assert: (UserGlobals includesKey: #XYClass)			 description: 'Should have loaded class named XYClass';		assert: (Object includesCategory: '*X-Y-testing')			description: 'Should have loaded a method into *X-Y-testing category on Object';		assert: (Object includesSelector: #isXY)			description: 'Should have loaded the method Object>>#isXY';		assert: (Object selectorsIn: '*X-Y-testing') asSet			equals: (Set with: #isXY)! !

!CypressPackageManagerTest methodsFor: 'tests - GemStone fileouts - Future' stamp: ''!
testLoadingPackageFromGemStoneFileoutRepository	"Presently, filing out .gs files is an option in the repository properties file,	 and there is no filing in support at all - as a 'repository'.	 I am considering having a separate repository format, but if one wants to	 have .gs files in the same directory as the Cypress files, it cannot use a	 properties file (it would conflict with the other).	 Additionally, it needs separate files for separate SymbolDictionaries,	 as well as some notion of the appropriate user."! !

!CypressPackageManagerTest methodsFor: 'tests - loading - To Do' stamp: ''!
testLoadingPackageWithGlobalExtensionWhenNotSystemUser! !

!CypressPackageManagerTest methodsFor: 'tests' stamp: ''!
testRemovingRepositoryFromPackage	| old repo repoDigest newClass |	self testAttachingNewCypressRepositoryToUnknownImagePackages.	old := currentManager packageInformationNamed: 'X-Y'.	repo := old repositories any.	self		assert: old repositories size = 1			description: 'There should have been only one repository for the package'.	old removeRepository: repo.	self		assert: old repositories isEmpty			description: 'There sole repository should have been removed from the package';		assert: old repositoryDigests isEmpty			description: 'After removing the last repository, there should not be any repository digests left';		assert: old digests size = 1			description: 'After removing the last repository, there should only be the image digest left';		assert: old digests keys asSet			equals: (Set with: #IMAGE);		assert: old imageDigest isNil			description: 'There should have been no contents in the image'! !

!CypressPackageManagerTest methodsFor: 'tests' stamp: ''!
testSavingChangedImagePackageToCypressRepository	| repo groups old new repoDigest newClass results |	currentManager := self createManagerWithUnknownPackages: #('X-Y').	repo := self createTestRepoNamed: 'CypressTestRepo'.	old := currentManager packageInformationNamed: 'X-Y'.	currentManager assignRepository: repo to: old.	old := currentManager packageInformationNamed: 'X-Y'.	newClass := self addClassNamed: 'XYClass' under: Object to: UserGlobals inCatgeory: 'X-Y'.	self addMethodNamed: 'stub' body: '^nil' toClass: newClass inCategory: 'dummy'.	self addMethodNamed: 'isXY' body: '^false' toClass: Object inCategory: '*X-Y-testing'.	currentManager writeChangesToAllRepositoriesFor: old.	results := CypressFileUtilities current directoryFileNamesAndContents: repo directoryPath.	self assert: results equals: self cypressFormatXYPackageDirectoryStructure.! !

!CypressPackageManagerTest methodsFor: 'tests' stamp: ''!
testSavingChangedImagePackageToFileTreeReadOnlyRepository	| repo groups old new repoDigest newClass results |	currentManager := self createManagerWithUnknownPackages: #('X-Y').	repo := self createFileTreeReadOnlyTestRepoNamed: 'CypressTestRepo'.	old := currentManager packageInformationNamed: 'X-Y'.	currentManager assignRepository: repo to: old.	old := currentManager packageInformationNamed: 'X-Y'.	newClass := self addClassNamed: 'XYClass' under: Object to: UserGlobals inCatgeory: 'X-Y'.	self addMethodNamed: 'stub' body: '^nil' toClass: newClass inCategory: 'dummy'.	self addMethodNamed: 'isXY' body: '^false' toClass: Object inCategory: '*X-Y-testing'.	currentManager writeChangesToAllRepositoriesFor: old.	results := CypressFileUtilities current directoryFileNamesAndContents: repo directoryPath.	self assert: results equals: self readOnlyFileTreeFormatXYPackageDirectoryStructure.! !

!CypressPackageManagerTest methodsFor: 'tests' stamp: ''!
testSavingChangedImagePackageToFileTreeRepository	| repo groups old new repoDigest newClass results |	currentManager := self createManagerWithUnknownPackages: #('X-Y').	repo := self createFileTreeCompatibleTestRepoNamed: 'CypressTestRepo'.	old := currentManager packageInformationNamed: 'X-Y'.	currentManager assignRepository: repo to: old.	old := currentManager packageInformationNamed: 'X-Y'.	newClass := self addClassNamed: 'XYClass' under: Object to: UserGlobals inCatgeory: 'X-Y'.	self addMethodNamed: 'stub' body: '^nil' toClass: newClass inCategory: 'dummy'.	self addMethodNamed: 'isXY' body: '^false' toClass: Object inCategory: '*X-Y-testing'.	currentManager writeChangesToAllRepositoriesFor: old.	results := CypressFileUtilities current directoryFileNamesAndContents: repo directoryPath.	self assert: results equals: self fileTreeFormatXYPackageDirectoryStructure.! !

!CypressPackageManagerTest methodsFor: 'tests - GemStone fileouts - Future' stamp: ''!
testSavingChangedImagePackageToGemStoneFileoutRepository	"Presently, filing out .gs files is an option in the repository properties file.	 I am considering having a separate repository format, but if one wants to	 have .gs files in the same directory as the Cypress files, it cannot use a	 properties file (it would conflict with the other).	 Additionally, it needs separate files for separate SymbolDictionaries,	 as well as some notion of the appropriate user."! !

CypressAbstractTest subclass: #CypressPatchTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Tests'!

!CypressPatchTest methodsFor: 'private' stamp: ''!
baseTargetPatch	| className |	className := 'CypressMockBasic'.	^{		CypressModification			of: (CypressClassDefinition				name: className				superclassName: 'Object'				category: 'Cypress-Mocks-Definitions'				instVarNames: #('name')				classInstVarNames: #('current')				classVarNames: #('Something')				poolDictionaryNames: #()				comment: 'This mock contains basic class and instance method selectors'				subclassType: '')			to: (CypressClassDefinition				name: className				superclassName: 'Object'				category: 'Cypress-Mocks-Definitions'				instVarNames: #('name')				classInstVarNames: #('current')				classVarNames: #()				poolDictionaryNames: #()				comment: 'This mock contains basic class and instance method selectors'				subclassType: '').		CypressAddition			of: (CypressMethodDefinition				className: className				classIsMeta: false				selector: 'added'				category: 'accessing'				source: 'added	"added method"').		CypressModification			of: (CypressMethodDefinition				className: className				classIsMeta: false				selector: 'name:'				category: 'accessing'				source: 'name: aString	name := aString')			to: (CypressMethodDefinition				className: className				classIsMeta: false				selector: 'name:'				category: 'accessing'				source: 'name: aString	"changed method"	name := aString').		CypressRemoval			of: (CypressMethodDefinition				className: className				classIsMeta: false				selector: 'extra'				category: 'accessing'				source: 'extra	"extra method"').		CypressRemoval			of: (CypressMethodDefinition				className: 'Object'				classIsMeta: false				selector: 'isCypressMockBasic'				category: '*Cypress-Mocks-Extensions'				source: 'isCypressMockBasic	^false').		CypressAddition			of: (CypressClassDefinition				name: className , 'Sub'				superclassName: className				category: 'Cypress-Mocks'				instVarNames: #('anotherIV')				classInstVarNames: #('anotherCIV')				classVarNames: #()				poolDictionaryNames: #()				comment: 'Hacked subclass to test class loading and unloading'				subclassType: '').		CypressAddition			of: (CypressMethodDefinition				className: className , 'Sub'				classIsMeta: false				selector: 'added'				category: 'accessing'				source: 'added	"added method"').	}! !

!CypressPatchTest methodsFor: 'tests' stamp: ''!
testDictionaryOfPatchOperations	"loader uses dictionary for managing patch operations ... ensure that Amber Dictionaries stand up"	| dict |	dict := Dictionary new.	self baseTargetPatch do: [:each | 		dict at: each put: each ].	self baseTargetPatch do: [:each | 		self assert: (dict at: each) equals: each ].! !

!CypressPatchTest methodsFor: 'tests' stamp: ''!
testPatch	| baseSnapshot targetSnapshot patch operations expected |	baseSnapshot := CypressSnapshot definitions: self baseDefinitions.	targetSnapshot := CypressSnapshot definitions: self targetDefinitions.	patch := CypressPatch fromBase: baseSnapshot toTarget: targetSnapshot.	operations := patch operations.	expected := self baseTargetPatch asArray.	self assert: operations size equals: expected size.	operations do: [:each | self assert: (expected includes: each)]! !

!CypressPatchTest methodsFor: 'tests' stamp: ''!
testPatchOperationEquality	| className modification removal addition |	className := 'CypressMockBasic'.	modification := CypressModification 			of: (CypressMethodDefinition          			className: className        			classIsMeta: false        			selector: 'name:'        			category: 'accessing'        			source:'name: aString	name := aString') 			to: (CypressMethodDefinition          			className: className        			classIsMeta: false        			selector: 'name:'        			category: 'accessing'        			source:'name: aString	"changed method"	name := aString').	self assert: modification equals: modification.	removal := CypressRemoval 			of: (CypressMethodDefinition          			className: className        			classIsMeta: false        			selector: 'extra'        			category: 'accessing'        			source:'extra	"extra method"').	self assert: removal equals: removal.	addition := CypressAddition			of: (CypressMethodDefinition          			className: className        			classIsMeta: false        			selector: 'extra'        			category: 'accessing'        			source:'extra	"extra method"').	self assert: addition equals: addition.! !

CypressAbstractTest subclass: #CypressReferenceTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Tests'!

!CypressReferenceTest methodsFor: 'running' stamp: ''!
assert: aString parsesToPackageName: packageName author: authorId branch: branchId version: versionNumber	| queryReference |	queryReference := CypressVersionReference name: aString.	self		assert: queryReference packageName equals: packageName;		assert: queryReference author equals: authorId;		assert: queryReference branch equals: branchId;		assert: queryReference versionNumber equals: versionNumber.! !

!CypressReferenceTest methodsFor: 'testing' stamp: ''!
testAuthorAlone	"Syntax: packageName[.branch][-author][.version]"	self		assert: '-dhk'			parsesToPackageName: ''			author: 'dhk'			branch: ''			version: 0! !

!CypressReferenceTest methodsFor: 'testing' stamp: ''!
testAuthorAndVersionOnly	"Syntax: packageName[.branch][-author][.version]"	self		assert: '-dhk.1'			parsesToPackageName: ''			author: 'dhk'			branch: ''			version: 1! !

!CypressReferenceTest methodsFor: 'testing' stamp: ''!
testBranchAfterAuthorIsNotABranch	self		assert: 'Seaside-Core-jf.configcleanup.3'			parsesToPackageName: 'Seaside-Core'			author: 'jf.configcleanup'			branch: ''			version: 3;		assert: 'Seaside-Core-lr.configcleanup.extraspeedup.69'			parsesToPackageName: 'Seaside-Core'			author: 'lr.configcleanup.extraspeedup'			branch: ''			version: 69;		assert: 'Seaside-Core-lr.configcleanup42.extraspeedup.69'			parsesToPackageName: 'Seaside-Core'			author: 'lr.configcleanup42.extraspeedup'			branch: ''			version: 69! !

!CypressReferenceTest methodsFor: 'testing' stamp: ''!
testBranchAlone	"Syntax: packageName[.branch][-author][.version]"	self		assert: '.v3'			parsesToPackageName: ''			author: ''			branch: 'v3'			version: 0! !

!CypressReferenceTest methodsFor: 'testing' stamp: ''!
testBranchAndAuthorOnly	"Syntax: packageName[.branch][-author][.version]"	self		assert: '.v3-dhk'			parsesToPackageName: ''			author: 'dhk'			branch: 'v3'			version: 0! !

!CypressReferenceTest methodsFor: 'testing' stamp: ''!
testBranchAndVersionOnly	"Syntax: packageName[.branch][-author][.version]"	self		assert: '.v3.1'			parsesToPackageName: ''			author: ''			branch: 'v3.1'			version: 0;		assert: '.v3-.1'			parsesToPackageName: ''			author: ''			branch: 'v3'			version: 1! !

!CypressReferenceTest methodsFor: 'testing' stamp: ''!
testBranchAuthorAndVersionOnly	"Syntax: packageName[.branch][-author][.version]"	self		assert: '.v3-dhk.1'			parsesToPackageName: ''			author: 'dhk'			branch: 'v3'			version: 1! !

!CypressReferenceTest methodsFor: 'testing' stamp: ''!
testEmptyString	"Syntax: packageName[.branch][-author][.version]"	self		assert: ''			parsesToPackageName: ''			author: ''			branch: ''			version: 0! !

!CypressReferenceTest methodsFor: 'testing' stamp: ''!
testPackageAlone	"Syntax: packageName[.branch][-author][.version]"	self		assert: 'Announcements'			parsesToPackageName: 'Announcements'			author: ''			branch: ''			version: 0;		assert: 'Announcements.-.'			parsesToPackageName: 'Announcements'			author: ''			branch: ''			version: 0! !

!CypressReferenceTest methodsFor: 'testing' stamp: ''!
testPackageAndAuthorOnly	"Syntax: packageName[.branch][-author][.version]"	self		assert: 'Announcements-dhk'			parsesToPackageName: 'Announcements'			author: 'dhk'			branch: ''			version: 0! !

!CypressReferenceTest methodsFor: 'testing' stamp: ''!
testPackageAndBranchOnly	"Syntax: packageName[.branch][-author][.version]"	self		assert: 'Announcements.v3'			parsesToPackageName: 'Announcements'			author: ''			branch: 'v3'			version: 0! !

!CypressReferenceTest methodsFor: 'testing' stamp: ''!
testPackageAndVersionOnly	"Syntax: packageName[.branch][-author][.version]"	self		assert: 'Announcements..1'			parsesToPackageName: 'Announcements'			author: ''			branch: '.1'			version: 0;		assert: 'Announcements.-.1'			parsesToPackageName: 'Announcements'			author: ''			branch: ''			version: 1! !

!CypressReferenceTest methodsFor: 'testing' stamp: ''!
testPackageAuthorAndVersionOnly	"Syntax: packageName[.branch][-author][.version]"	self		assert: 'Announcements-dhk.1'			parsesToPackageName: 'Announcements'			author: 'dhk'			branch: ''			version: 1! !

!CypressReferenceTest methodsFor: 'testing' stamp: ''!
testPackageBranchAndAuthorOnly	"Syntax: packageName[.branch][-author][.version]"	self		assert: 'Announcements.v3-dhk'			parsesToPackageName: 'Announcements'			author: 'dhk'			branch: 'v3'			version: 0! !

!CypressReferenceTest methodsFor: 'testing' stamp: ''!
testPackageBranchAndVersionOnly	"Syntax: packageName[.branch][-author][.version]"	self		assert: 'Announcements.v3.1'			parsesToPackageName: 'Announcements'			author: ''			branch: 'v3.1'			version: 0;		assert: 'Announcements.v3-.1'			parsesToPackageName: 'Announcements'			author: ''			branch: 'v3'			version: 1! !

!CypressReferenceTest methodsFor: 'testing' stamp: ''!
testPackageBranchAuthorAndVersion	"Syntax: packageName[.branch][-author][.version]"	self		assert: 'Announcements.v3-dhk.1'			parsesToPackageName: 'Announcements'			author: 'dhk'			branch: 'v3'			version: 1! !

!CypressReferenceTest methodsFor: 'testing' stamp: ''!
testPunctuationOnly	"Syntax: packageName[.branch][-author][.version]"	self		assert: '.-.'			parsesToPackageName: ''			author: ''			branch: ''			version: 0;		assert: '.-'			parsesToPackageName: ''			author: ''			branch: ''			version: 0;		assert: '.'			parsesToPackageName: ''			author: ''			branch: ''			version: 0;		assert: '..'			parsesToPackageName: ''			author: ''			branch: '.'			version: 0! !

!CypressReferenceTest methodsFor: 'testing' stamp: ''!
testVersionAlone	"Syntax: packageName[.branch][-author][.version]"	self		assert: '..1'			parsesToPackageName: ''			author: ''			branch: '.1'			version: 0;		assert: '.-.1'			parsesToPackageName: ''			author: ''			branch: ''			version: 1! !

!CypressReferenceTest methodsFor: 'testing' stamp: ''!
testVersionShouldParseComplexName	"Syntax: packageName[.branch][-author][.version]"	self		assert: 'Seaside2.8b5'			parsesToPackageName: 'Seaside2'			author: ''			branch: '8b5'			version: 0;		assert: 'Seaside2.8b5-avi.1'			parsesToPackageName: 'Seaside2'			author: 'avi'			branch: '8b5'			version: 1;		assert: 'Seaside-Core-pmm.2'			parsesToPackageName: 'Seaside-Core'			author: 'pmm'			branch: ''			version: 2;		assert: 'Seaside-Core.configcleanup-jf.3'			parsesToPackageName: 'Seaside-Core'			author: 'jf'			branch: 'configcleanup'			version: 3;		assert: 'Seaside-Core.configcleanup.extraspeedup-lr.69'			parsesToPackageName: 'Seaside-Core'			author: 'lr'			branch: 'configcleanup.extraspeedup'			version: 69;		assert: 'Seaside-Core.configcleanup42.extraspeedup-lr.69'			parsesToPackageName: 'Seaside-Core'			author: 'lr'			branch: 'configcleanup42.extraspeedup'			version: 69! !

CypressAbstractTest subclass: #CypressSnapshotTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Tests'!

!CypressSnapshotTest methodsFor: 'tests' stamp: ''!
testExtensionsOnlySnapshot	| name pkg packageDefinitions expectedDefinitions |	name := 'Cypress-Mocks-Extensions'.	pkg := CypressPackageDefinition named: name.	packageDefinitions := pkg snapshot definitions.	self		assert: packageDefinitions size equals: 1;		assert: packageDefinitions first selector equals: 'isCypressMockBasic';		assert: packageDefinitions first className equals: 'Object'! !

!CypressSnapshotTest methodsFor: 'tests' stamp: ''!
testSnapshot	| name pkg packageDefinitions expectedDefinitions |	name := 'Cypress-Mocks'.	pkg := CypressPackageDefinition named: name.	packageDefinitions := pkg snapshot definitions.	expectedDefinitions := self baseDefinitions.	self assert: expectedDefinitions size equals: packageDefinitions size.	packageDefinitions		do: [:def | self assert: (expectedDefinitions includes: def)]! !

!CypressSnapshotTest methodsFor: 'tests' stamp: ''!
testSnapshotEquality	| name pkg packageDefinitions expectedDefinitions |	name := 'Cypress-Mocks'.	pkg := CypressPackageDefinition named: name.	packageDefinitions := pkg snapshot definitions.	expectedDefinitions := self baseDefinitions.	self assert: packageDefinitions asArray equals: expectedDefinitions asArray! !

CypressAbstractTest subclass: #CypressStructureTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Tests'!

!CypressStructureTest methodsFor: 'private' stamp: ''!
compileJSON: aJsonString	^CypressJsonParser parse: aJsonString! !

!CypressStructureTest methodsFor: 'private' stamp: ''!
sampleJson    ^ '{	"age" : 25,	"name" : "John%20Smith",	"phoneNumber" : [		{			"number" : "212%20555-1234",			"type" : "home" },		{			"number" : "646%20555-4567",			"type" : "fax" } ],	"registered" : true }'! !

!CypressStructureTest methodsFor: 'tests' stamp: ''!
testPackageStructureFromPackage	| packageStructure packageDefinitions expectedDefinitions |	packageStructure := CypressPackageStructure				fromPackage: (CypressPackageDefinition named: 'Cypress-Mocks').	packageDefinitions := packageStructure snapshot definitions.	expectedDefinitions := self baseDefinitions.	self assert: expectedDefinitions size equals: packageDefinitions size.	packageDefinitions do: 			[:def |			self assert: (expectedDefinitions includes: def)				description: 'Definition for ', def printString, ' did not match expected ones']! !

!CypressStructureTest methodsFor: 'tests' stamp: ''!
testPropertyDictionaryRead	| propertyDictionary phoneNumbers |	propertyDictionary := (self compileJSON: self sampleJson) asCypressPropertyObject.	self assert: (propertyDictionary at: 'name') equals: 'John Smith'.	self assert: (propertyDictionary at: 'age') equals: 25.	self assert: (propertyDictionary at: 'registered') description: '"registered" property should have been true'.	phoneNumbers := propertyDictionary at: 'phoneNumber'.	self assert: phoneNumbers size equals: 2.	self assert: ((phoneNumbers at: 1) at: 'number') equals: '212 555-1234'.	self assert: ((phoneNumbers at: 2) at: 'number') equals: '646 555-4567'.! !

!CypressStructureTest methodsFor: 'tests' stamp: ''!
testPropertyDictionaryWrite    | propertyDictionary stream x |    propertyDictionary := (self compileJSON: self sampleJson) asCypressPropertyObject.    stream := WriteStream on: String new.    propertyDictionary writeCypressJsonOn: stream indent: 0.    self assert: (x := stream contents withUnixLineEndings) equals: self sampleJson withUnixLineEndings! !
