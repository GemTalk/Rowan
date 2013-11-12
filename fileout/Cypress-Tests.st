" Package: Cypress-Tests"!


" Class Declarations "!

(TestCase
	subclass: 'CypressAbstractTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Tests';
		comment: ''!

(CypressAbstractTest
	subclass: 'CypressStructureTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Tests';
		comment: ''!

(CypressAbstractTest
	subclass: 'CypressLoaderTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Tests';
		comment: ''!

(CypressAbstractTest
	subclass: 'CypressExtensionsTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Tests';
		comment: ''!

(CypressAbstractTest
	subclass: 'CypressSnapshotTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Tests';
		comment: ''!

(CypressAbstractTest
	subclass: 'CypressDefinitionTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Tests';
		comment: ''!

(CypressAbstractTest
	subclass: 'CypressPatchTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Tests';
		comment: ''!

" Class Implementation for CypressAbstractTest"!

" ------------------- Class methods for CypressAbstractTest"!

!CypressAbstractTest class methodsFor: 'testing'!
isAbstract

	^self sunitName = #CypressAbstractTest
! !
" ------------------- Instance methods for CypressAbstractTest"!

!CypressAbstractTest methodsFor: 'private'!
baseDefinitions

	| className |
	className := 'CypressMockBasic'.
	^{
		CypressClassDefinition
			name: className
			superclassName: 'Object'
			category: 'Cypress-Mocks-Definitions'
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
	}! !
!CypressAbstractTest methodsFor: 'private'!
targetDefinitions
	"remove #extra method and modify #name: method"

	| className |
	className := 'CypressMockBasic'.
	^{
		CypressClassDefinition
			name: className
			superclassName: 'Object'
			category: 'Cypress-Mocks-Definitions'
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
! !
" Class Implementation for CypressStructureTest"!

" ------------------- Instance methods for CypressStructureTest"!

!CypressStructureTest methodsFor: 'private'!
compileJSON: aJsonString

	^CypressJsonParser parse: aJsonString
! !
!CypressStructureTest methodsFor: 'private'!
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
! !
!CypressStructureTest methodsFor: 'tests'!
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
				description: 'Definition for ', def printString, ' did not match expected ones']! !
!CypressStructureTest methodsFor: 'tests'!
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
! !
!CypressStructureTest methodsFor: 'tests'!
testPropertyDictionaryWrite
    | propertyDictionary stream x |
    propertyDictionary := (self compileJSON: self sampleJson) asCypressPropertyObject.
    stream := WriteStream on: String new.
    propertyDictionary writeCypressJsonOn: stream indent: 0.
    self assert: (x := stream contents withUnixLineEndings) equals: self sampleJson withUnixLineEndings
! !
" Class Implementation for CypressLoaderTest"!

" ------------------- Instance methods for CypressLoaderTest"!

!CypressLoaderTest methodsFor: 'utility'!
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
! !
!CypressLoaderTest methodsFor: 'running'!
tearDown

	| name |
	super tearDown.
	name := 'Cypress-Mocks'.
	(CypressSnapshot definitions: self baseDefinitions)
		 updatePackage: (CypressPackageDefinition named: name)
! !
!CypressLoaderTest methodsFor: 'tests'!
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
			description: 'There should have been no external requirements'! !
!CypressLoaderTest methodsFor: 'tests'!
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
		assert: loader requirements size equals: 0! !
!CypressLoaderTest methodsFor: 'tests'!
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
		assert: loader requirements size equals: 2! !
!CypressLoaderTest methodsFor: 'utility'!
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
! !
" Class Implementation for CypressExtensionsTest"!

" ------------------- Instance methods for CypressExtensionsTest"!

!CypressExtensionsTest methodsFor: 'tests'!
test_beginsWith_

	self
		assert: ('abc' beginsWith: 'a');
		deny: ('abc' beginsWith: 'c');
		assert: ('abc' beginsWith: 'abc');
		deny: ('abc' beginsWith: 'abcx');
		deny: ('abc' beginsWith: '');
		deny: ('' beginsWith: 'abc');
		deny: ('' beginsWith: '')
! !
!CypressExtensionsTest methodsFor: 'tests'!
test_copyWithoutSuffix_

	self
		assert: ('abc' copyWithoutSuffix: 'c') equals: 'ab';
		assert: ('abc' copyWithoutSuffix: 'a') equals: 'abc';
		assert: ('abc' copyWithoutSuffix: 'abc') equals: '';
		assert: ('abc' copyWithoutSuffix: 'xxxabc') equals: 'abc';
		assert: ('abc' copyWithoutSuffix: '') equals: 'abc';
		assert: ('' copyWithoutSuffix: 'abc') equals: '';
		assert: ('' copyWithoutSuffix: '') equals: ''
! !
!CypressExtensionsTest methodsFor: 'tests'!
test_copyWithoutSuffix_or_

	self
		assert: ('abc' copyWithoutSuffix: 'c' or: [nil]) equals: 'ab';
		assert: ('abc' copyWithoutSuffix: 'a' or: [nil]) equals: nil;
		assert: ('abc' copyWithoutSuffix: 'abc' or: [nil]) equals: '';
		assert: ('abc' copyWithoutSuffix: 'xxxabc' or: [nil]) equals: nil;
		assert: ('abc' copyWithoutSuffix: '' or: [nil]) equals: nil;
		assert: ('' copyWithoutSuffix: 'abc' or: [nil]) equals: nil;
		assert: ('' copyWithoutSuffix: '' or: [nil]) equals: nil
! !
!CypressExtensionsTest methodsFor: 'tests'!
test_difference_
	"Set theoretic difference means elements from the first collection not in the second."

	self
		assert: (#(1 2 3) difference: #()) sortAscending equals: #(1 2 3);
		assert: (#() difference: #(1 2 3)) sortAscending equals: #();
		assert: (#(1 2 3) difference: #(1 2 3)) sortAscending equals: #();
		assert: (#(1 2 3) difference: #(2 3 4)) sortAscending equals: #(1);
		assert: (#(1 2 3) difference: #(3 4 5)) sortAscending equals: #(1 2);
		assert: (#(1 2 3) difference: #(4 5 6)) sortAscending equals: #(1 2 3)
! !
!CypressExtensionsTest methodsFor: 'tests'!
test_endsWith_

	self
		assert: ('abc' endsWith: 'c');
		deny: ('abc' endsWith: 'a');
		assert: ('abc' endsWith: 'abc');
		deny: ('abc' endsWith: 'xabc');
		deny: ('abc' endsWith: '');
		deny: ('' endsWith: 'abc');
		deny: ('' endsWith: '')
! !
!CypressExtensionsTest methodsFor: 'tests'!
test_escapePercents

	self
		assert: 'aa aa éé aa aa' encodeAsUTF8 escapePercents
			equals: 'aa%20aa%20%C3%A9%C3%A9%20aa%20aa';
		assert: 'aa aa éé aa aa' escapePercents
			equals: 'aa%20aa%20%E9%E9%20aa%20aa'! !
!CypressExtensionsTest methodsFor: 'tests'!
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
! !
!CypressExtensionsTest methodsFor: 'tests'!
test_gather_

	self
		assert: (#(1 2 3) gather: [:each | each * 10 + 1 to: each * 10 + each])
		equals: #(11 21 22 31 32 33)! !
!CypressExtensionsTest methodsFor: 'tests'!
test_indexOfAnyOf_startingAt_

	self
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 1) equals: 5;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 6) equals: 6;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 7) equals: 14;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 15) equals: 15;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 16) equals: 20;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 21) equals: 0;
		assert: ('word, another, done.' indexOfAnyOf: '+-' startingAt: 1) equals: 0
! !
!CypressExtensionsTest methodsFor: 'tests'!
test_indexOfAnyOf_startingAt_ifAbsent_

	self
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 1 ifAbsent: [nil]) equals: 5;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 6 ifAbsent: [nil]) equals: 6;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 7 ifAbsent: [nil]) equals: 14;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 15 ifAbsent: [nil]) equals: 15;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 16 ifAbsent: [nil]) equals: 20;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 21 ifAbsent: [nil]) equals: nil;
		assert: ('word, another, done.' indexOfAnyOf: '+-' startingAt: 1 ifAbsent: [nil]) equals: nil
! !
!CypressExtensionsTest methodsFor: 'tests'!
test_intersection_

	self
		assert: (#(1 2 3) intersection: #()) sortAscending equals: #();
		assert: (#() intersection: #(1 2 3)) sortAscending equals: #();
		assert: (#(1 2 3) intersection: #(1 2 3)) sortAscending equals: #(1 2 3);
		assert: (#(1 2 3) intersection: #(2 3 4)) sortAscending equals: #(2 3);
		assert: (#(1 2 3) intersection: #(3 4 5)) sortAscending equals: #(3);
		assert: (#(1 2 3) intersection: #(4 5 6)) sortAscending equals: #()
! !
!CypressExtensionsTest methodsFor: 'tests'!
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
! !
!CypressExtensionsTest methodsFor: 'tests'!
test_streamContents_

	self
		assert: (String streamContents: 
					[:stream |
					(1 to: 3) do: [:each | stream nextPutAll: each printString]
						separatedBy: [stream space]])
		equals: '1 2 3'! !
!CypressExtensionsTest methodsFor: 'tests'!
test_unescapePercents

	self
		assert: 'aa%20aa%20%C3%A9%C3%A9%20aa%20aa'  unescapePercents asByteArray decodeFromUTF8 asString
			equals: 'aa aa éé aa aa';
		assert: 'aa%20aa%20%E9%E9%20aa%20aa' unescapePercents
			equals: 'aa aa éé aa aa' asUnicodeString! !
!CypressExtensionsTest methodsFor: 'tests'!
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
		assert: (self unixLinesFrom: 'abc', crlf, crlf) equals: #('abc' '')! !
!CypressExtensionsTest methodsFor: 'utility'!
unixLinesFrom: aString
	
	| sourceStream resultStream |
	sourceStream := ReadStream on: aString withUnixLineEndings.
	resultStream := WriteStream on: Array new.
	[sourceStream atEnd]
		whileFalse: [resultStream nextPut: (sourceStream upTo: Character lf)].
	^resultStream contents.
! !
" Class Implementation for CypressSnapshotTest"!

" ------------------- Instance methods for CypressSnapshotTest"!

!CypressSnapshotTest methodsFor: 'tests'!
testExtensionsOnlySnapshot

	| name pkg packageDefinitions |
	name := 'Cypress-Mocks-Extensions'.
	pkg := CypressPackageDefinition named: name.
	packageDefinitions := pkg snapshot definitions.
	self
		assert: packageDefinitions size equals: 1;
		assert: packageDefinitions first selector equals: 'isCypressMockBasic';
		assert: packageDefinitions first className equals: 'Object'! !
!CypressSnapshotTest methodsFor: 'tests'!
testSnapshot

	| name pkg packageDefinitions expectedDefinitions |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition named: name.
	packageDefinitions := pkg snapshot definitions.
	expectedDefinitions := self baseDefinitions.
	self assert: expectedDefinitions size equals: packageDefinitions size.
	packageDefinitions
		do: [:def | self assert: (expectedDefinitions includes: def)]! !
!CypressSnapshotTest methodsFor: 'tests'!
testSnapshotEquality
	| name pkg packageDefinitions expectedDefinitions |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition named: name.
	packageDefinitions := pkg snapshot definitions.
	expectedDefinitions := self baseDefinitions.
	self assert: packageDefinitions asArray equals: expectedDefinitions asArray
! !
" Class Implementation for CypressDefinitionTest"!

" ------------------- Instance methods for CypressDefinitionTest"!

!CypressDefinitionTest methodsFor: 'tests'!
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
		equals: 'a CypressClassDefinition(Foo)'! !
!CypressDefinitionTest methodsFor: 'tests'!
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
! !
!CypressDefinitionTest methodsFor: 'tests'!
testEquality
	| pkg1 pkg2 pkg3 name |
	name := 'Cypress-Mocks'.
	pkg1 := CypressPackageDefinition named: name.
	pkg2 := CypressPackageDefinition named: name.
	pkg3 := CypressPackageDefinition named: 'Nope!!'.

	self assert: pkg1 equals: pkg2.
	self deny: pkg1 equals: pkg3
! !
!CypressDefinitionTest methodsFor: 'tests'!
testMethodDefinition
	self assert: (CypressMethodDefinition
		className: 'Foo'
		classIsMeta: false
		selector: 'isFoo'
		category: 'testing'
		source: 'isFoo ^true') printString equals: 'a CypressMethodDefinition(Foo>>isFoo)'
! !
!CypressDefinitionTest methodsFor: 'tests'!
testNameEquality
	| pkg name |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition named: name.
	self assert: pkg name equals: name.
	self deny: pkg name equals: 'Nope.'.
! !
!CypressDefinitionTest methodsFor: 'tests'!
testPrintString
	| name pkg |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition named: name.
	self assert: 'a CypressPackageDefinition(', name, ')' equals: pkg printString.
! !
" Class Implementation for CypressPatchTest"!

" ------------------- Instance methods for CypressPatchTest"!

!CypressPatchTest methodsFor: 'private'!
baseTargetPatch

	| className |
	className := 'CypressMockBasic'.
	^{
		CypressModification
			of: (CypressClassDefinition
				name: className
				superclassName: 'Object'
				category: 'Cypress-Mocks-Definitions'
				instVarNames: #('name')
				classInstVarNames: #('current')
				classVarNames: #('Something')
				poolDictionaryNames: #()
				comment: 'This mock contains basic class and instance method selectors'
				subclassType: '')
			to: (CypressClassDefinition
				name: className
				superclassName: 'Object'
				category: 'Cypress-Mocks-Definitions'
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
! !
!CypressPatchTest methodsFor: 'tests'!
testDictionaryOfPatchOperations
	"loader uses dictionary for managing patch operations ... ensure that Amber Dictionaries stand up"

	| dict |
	dict := Dictionary new.
	self baseTargetPatch do: [:each | 
		dict at: each put: each ].
	self baseTargetPatch do: [:each | 
		self assert: (dict at: each) equals: each ].
! !
!CypressPatchTest methodsFor: 'tests'!
testPatch

	| baseSnapshot targetSnapshot patch operations expected |
	baseSnapshot := CypressSnapshot definitions: self baseDefinitions.
	targetSnapshot := CypressSnapshot definitions: self targetDefinitions.
	patch := CypressPatch fromBase: baseSnapshot toTarget: targetSnapshot.
	operations := patch operations.
	expected := self baseTargetPatch asArray.
	self assert: operations size equals: expected size.
	operations do: [:each | self assert: (expected includes: each)]! !
!CypressPatchTest methodsFor: 'tests'!
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
! !
" Class Extensions "!

" Class initializers "!




" End of Package: Cypress-Tests"!


