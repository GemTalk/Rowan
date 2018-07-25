! Package: Tonel-Tests


! Remove existing behavior from package Tonel-Tests
!!!! This can be cleaned up when some package functionality is moved to the base system.

doit
| packageName |
packageName := 'Tonel-Tests'.
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
										each isEmpty not and: [
											(each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]])
											or: [each first ~= $*]]]
					]
					ifFalse: [
							"*packagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										each isEmpty not and: [
											each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]]]]
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
(TestCase
	subclass: 'TonelAbstractTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Tonel-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TonelAbstractTest
	subclass: 'TonelAbstractWriterTest'
	instVarNames: #( directory )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Tonel-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TonelAbstractTest
	subclass: 'TonelReaderTest'
	instVarNames: #( directory )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Tonel-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'TonelParserTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Tonel-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'TonelWriterTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Tonel-Tests';
		comment: '';
		immediateInvariant.
true.
%

! Class Implementation for TonelAbstractTest

! ------------------- Class methods for TonelAbstractTest

category: 'Testing'
classmethod: TonelAbstractTest
isAbstract
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

	^self sunitName = #TonelAbstractTest
%

category: 'private'
classmethod: TonelAbstractTest
mockCypressSnapshotSTON
  ^ 'CypressSnapshot {
	#definitions : [
		CypressClassDefinition {
			#name : ''MCMockASubclass'',
			#superclassName : ''MCMockClassA'',
			#category : ''MonticelloMocks'',
			#comment : '''',
			#instVarNames : [
				''x''
			],
			#classInstVarNames : [ ],
			#classVarNames : [
				''Y''
			],
			#poolDictionaryNames : [ ],
			#subclassType : #normal
		},
		CypressMethodDefinition {
			#classIsMeta : true,
			#source : ''initialize\n\n\tInitializationOrder := InitializationOrder\n\t\tifNil: [ -100 ] \"let the test fail\"\n\t\tifNotNil: [ InitializationOrder + 1.]'',
			#category : ''as yet unclassified'',
			#selector : ''initialize'',
			#className : ''MCMockASubclass''
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''variables\n\t^ x + Y + MCMockClassA'',
			#category : ''as yet unclassified'',
			#selector : ''variables'',
			#className : ''MCMockASubclass''
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''variables2\n\t^ ivar + CVar'',
			#category : ''as yet unclassified'',
			#selector : ''variables2'',
			#className : ''MCMockASubclass''
		},
		CypressClassDefinition {
			#name : ''MCMockClassA'',
			#superclassName : ''MCMock'',
			#category : ''MonticelloMocks'',
			#comment : ''This is a mock class. The Monticello tests manipulated it to simulate a developer modifying code in the image.'',
			#instVarNames : [
				''ivar''
			],
			#classInstVarNames : [ ],
			#classVarNames : [
				''CVar'',
				''InitializationOrder''
			],
			#poolDictionaryNames : [ ],
			#subclassType : #normal
		},
		CypressMethodDefinition {
			#classIsMeta : true,
			#source : ''cVar\n\t^ CVar'',
			#category : ''as yet unclassified'',
			#selector : ''cVar'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : true,
			#source : ''initializationOrder\n\n\t^ InitializationOrder'',
			#category : ''as yet unclassified'',
			#selector : ''initializationOrder'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : true,
			#source : ''initialize\n\tCVar := #initialized.\n\tInitializationOrder := 1.\n'',
			#category : ''as yet unclassified'',
			#selector : ''initialize'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : true,
			#source : ''one\n\n\t^ 1'',
			#category : ''as yet unclassified'',
			#selector : ''one'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : true,
			#source : ''touchCVar\n\tCVar := #touched'',
			#category : ''as yet unclassified'',
			#selector : ''touchCVar'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''a\n\t^ \''a2\'''',
			#category : ''numeric'',
			#selector : ''a'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''b\n\t^ \''b1\'''',
			#category : ''numeric'',
			#selector : ''b'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''c\n\t^ \''c1\'''',
			#category : ''numeric'',
			#selector : ''c'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''d\n\t^ \''d\'''',
			#category : ''numeric'',
			#selector : ''d'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''falsehood\n\t^ false'',
			#category : ''boolean'',
			#selector : ''falsehood'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''moreTruth\n\n\t^ true'',
			#category : ''boolean'',
			#selector : ''moreTruth'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''one\n\t^ 1'',
			#category : ''numeric'',
			#selector : ''one'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''q'',
			#category : ''drag\''n\''drop'',
			#selector : ''q'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''truth\n\t^ true'',
			#category : ''boolean'',
			#selector : ''truth'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''two\n\t^ 2'',
			#category : ''numeric'',
			#selector : ''two'',
			#className : ''MCMockClassA''
		},
		CypressClassDefinition {
			#name : ''MCMockClassB'',
			#superclassName : ''MCMock'',
			#category : ''MonticelloMocks'',
			#comment : ''This comment has a bang! Bang! Bang!'',
			#instVarNames : [
				''ivarb''
			],
			#classInstVarNames : [
				''ciVar''
			],
			#classVarNames : [
				''CVar''
			],
			#poolDictionaryNames : [
				''MCMockAPoolDictionary''
			],
			#subclassType : #normal
		},
		CypressClassDefinition {
			#name : ''MCMockClassD'',
			#superclassName : ''Object'',
			#category : ''MonticelloMocks'',
			#comment : '''',
			#instVarNames : [ ],
			#classInstVarNames : [ ],
			#classVarNames : [ ],
			#poolDictionaryNames : [ ],
			#subclassType : #normal
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''one\n\t^ 1'',
			#category : ''as yet unclassified'',
			#selector : ''one'',
			#className : ''MCMockClassD''
		},
		CypressClassDefinition {
			#name : ''MCMockClassE'',
			#superclassName : ''Object'',
			#category : ''MonticelloMocks'',
			#comment : '''',
			#instVarNames : [ ],
			#classInstVarNames : [ ],
			#classVarNames : [ ],
			#poolDictionaryNames : [ ],
			#subclassType : #variable
		},
		CypressMethodDefinition {
			#classIsMeta : true,
			#source : ''two\n\t^ 2'',
			#category : ''as yet unclassified'',
			#selector : ''two'',
			#className : ''MCMockClassE''
		},
		CypressClassDefinition {
			#name : ''MCMockClassF'',
			#superclassName : ''Object'',
			#category : ''MonticelloMocks'',
			#comment : '''',
			#instVarNames : [ ],
			#classInstVarNames : [ ],
			#classVarNames : [
				''Foo''
			],
			#poolDictionaryNames : [ ],
			#subclassType : #normal
		},
		CypressClassDefinition {
			#name : ''MCMockClassG'',
			#superclassName : ''Object'',
			#category : ''MonticelloMocks'',
			#comment : '''',
			#instVarNames : [ ],
			#classInstVarNames : [ ],
			#classVarNames : [ ],
			#poolDictionaryNames : [ ],
			#subclassType : #words
		},
		CypressClassDefinition {
			#name : ''MCMockClassH'',
			#superclassName : ''Object'',
			#category : ''MonticelloMocks'',
			#comment : '''',
			#instVarNames : [ ],
			#classInstVarNames : [ ],
			#classVarNames : [ ],
			#poolDictionaryNames : [ ],
			#subclassType : #bytes
		},
		CypressClassDefinition {
			#name : ''MCMockClassI'',
			#superclassName : ''Object'',
			#category : ''MonticelloMocks'',
			#comment : '''',
			#instVarNames : [ ],
			#classInstVarNames : [ ],
			#classVarNames : [ ],
			#poolDictionaryNames : [ ],
			#subclassType : #weak
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''mockClassExtension\n\n\t\"I change the protocol of this method to resolve the failing test: MCChangeNotificationTest >> testExtMethodModified. This test basically test that when we modified an extension method, the extended package is marked as \''modified\''. The problem is that Monticello treat differently a classic method from an extension method, and this only by checking if the protocol name start with a star. Therefore, if the protocol does not match the extending package name, the extending package name will never be notified, and the test will fail. \"'',
			#category : ''*MonticelloMocks'',
			#selector : ''mockClassExtension'',
			#className : ''MCSnapshotTest''
		}
	]
}'
%

category: 'private'
classmethod: TonelAbstractTest
mockMCSnapshotSTON
	^ 'MCSnapshot {
	#definitions : [
		MCOrganizationDefinition {
			#categories : [
				#MonticelloMocks
			]
		},
		MCClassDefinition {
			#name : #MCMockASubclass,
			#superclassName : #MCMockClassA,
			#variables : OrderedCollection [
				MCInstanceVariableDefinition {
					#name : ''x''
				},
				MCClassVariableDefinition {
					#name : ''Y''
				}
			],
			#category : #MonticelloMocks,
			#type : #normal,
			#comment : '''',
			#commentStamp : '''',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCMethodDefinition {
			#classIsMeta : true,
			#source : ''initialize\r\r\tInitializationOrder := InitializationOrder\r\t\tifNil: [ -100 ] "let the test fail"\r\t\tifNotNil: [ InitializationOrder + 1.]'',
			#category : #''as yet unclassified'',
			#selector : #initialize,
			#className : #MCMockASubclass
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''variables\r\t^ x + Y + MCMockClassA'',
			#category : #''as yet unclassified'',
			#selector : #variables,
			#className : #MCMockASubclass
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''variables2\r\t^ ivar + CVar'',
			#category : #''as yet unclassified'',
			#selector : #variables2,
			#className : #MCMockASubclass
		},
		MCClassDefinition {
			#name : #MCMockClassA,
			#superclassName : #MCMock,
			#variables : OrderedCollection [
				MCInstanceVariableDefinition {
					#name : ''ivar''
				},
				MCClassVariableDefinition {
					#name : ''CVar''
				},
				MCClassVariableDefinition {
					#name : ''InitializationOrder''
				}
			],
			#category : #MonticelloMocks,
			#type : #normal,
			#comment : ''This is a mock class. The Monticello tests manipulated it to simulate a developer modifying code in the image.'',
			#commentStamp : ''cwp 8/10/2003 16:43'',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCMethodDefinition {
			#classIsMeta : true,
			#source : ''cVar\r\t^ CVar'',
			#category : #''as yet unclassified'',
			#selector : #cVar,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : true,
			#source : ''initializationOrder\r\r\t^ InitializationOrder'',
			#category : #''as yet unclassified'',
			#selector : #initializationOrder,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : true,
			#source : ''initialize\r\tCVar := #initialized.\r\tInitializationOrder := 1.\r'',
			#category : #''as yet unclassified'',
			#selector : #initialize,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : true,
			#source : ''one\r\r\t^ 1'',
			#category : #''as yet unclassified'',
			#selector : #one,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : true,
			#source : ''touchCVar\r\tCVar := #touched'',
			#category : #''as yet unclassified'',
			#selector : #touchCVar,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''a\r\t^ \''a2\'''',
			#category : #numeric,
			#selector : #a,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''b\r\t^ \''b1\'''',
			#category : #numeric,
			#selector : #b,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''c\r\t^ \''c1\'''',
			#category : #numeric,
			#selector : #c,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''d\r\t^ \''d\'''',
			#category : #numeric,
			#selector : #d,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''falsehood\r\t^ false'',
			#category : #boolean,
			#selector : #falsehood,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''moreTruth\r\r\t^ true'',
			#category : #boolean,
			#selector : #moreTruth,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''one\r\t^ 1'',
			#category : #numeric,
			#selector : #one,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''q'',
			#category : #''drag\''n\''drop'',
			#selector : #q,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''truth\r\t^ true'',
			#category : #boolean,
			#selector : #truth,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''two\r\t^ 2'',
			#category : #numeric,
			#selector : #two,
			#className : #MCMockClassA
		},
		MCClassDefinition {
			#name : #MCMockClassB,
			#superclassName : #MCMock,
			#variables : OrderedCollection [
				MCInstanceVariableDefinition {
					#name : ''ivarb''
				},
				MCClassVariableDefinition {
					#name : ''CVar''
				},
				MCPoolImportDefinition {
					#name : ''MCMockAPoolDictionary''
				},
				MCClassInstanceVariableDefinition {
					#name : ''ciVar''
				}
			],
			#category : #MonticelloMocks,
			#type : #normal,
			#comment : ''This comment has a bang! Bang! Bang!'',
			#commentStamp : '''',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCClassDefinition {
			#name : #MCMockClassD,
			#superclassName : #Object,
			#variables : OrderedCollection [ ],
			#category : #MonticelloMocks,
			#type : #normal,
			#comment : '''',
			#commentStamp : '''',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''one\r\t^ 1'',
			#category : #''as yet unclassified'',
			#selector : #one,
			#className : #MCMockClassD
		},
		MCClassDefinition {
			#name : #MCMockClassE,
			#superclassName : #Object,
			#variables : OrderedCollection [ ],
			#category : #MonticelloMocks,
			#type : #variable,
			#comment : '''',
			#commentStamp : '''',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCMethodDefinition {
			#classIsMeta : true,
			#source : ''two\r\t^ 2'',
			#category : #''as yet unclassified'',
			#selector : #two,
			#className : #MCMockClassE
		},
		MCClassDefinition {
			#name : #MCMockClassF,
			#superclassName : #Object,
			#variables : OrderedCollection [
				MCClassVariableDefinition {
					#name : ''Foo''
				}
			],
			#category : #MonticelloMocks,
			#type : #normal,
			#comment : '''',
			#commentStamp : '''',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCClassDefinition {
			#name : #MCMockClassG,
			#superclassName : #Object,
			#variables : OrderedCollection [ ],
			#category : #MonticelloMocks,
			#type : #words,
			#comment : '''',
			#commentStamp : '''',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCClassDefinition {
			#name : #MCMockClassH,
			#superclassName : #Object,
			#variables : OrderedCollection [ ],
			#category : #MonticelloMocks,
			#type : #bytes,
			#comment : '''',
			#commentStamp : '''',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCClassDefinition {
			#name : #MCMockClassI,
			#superclassName : #Object,
			#variables : OrderedCollection [ ],
			#category : #MonticelloMocks,
			#type : #weak,
			#comment : '''',
			#commentStamp : '''',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''mockClassExtension\r\r\t"I change the protocol of this method to resolve the failing test: MCChangeNotificationTest >> testExtMethodModified. This test basically test that when we modified an extension method, the extended package is marked as \''modified\''. The problem is that Monticello treat differently a classic method from an extension method, and this only by checking if the protocol name start with a star. Therefore, if the protocol does not match the extending package name, the extending package name will never be notified, and the test will fail. " '',
			#category : #''*MonticelloMocks'',
			#selector : #mockClassExtension,
			#className : #MCSnapshotTest
		}
	]
}'
%

! ------------------- Instance methods for TonelAbstractTest

category: 'private'
method: TonelAbstractTest
fileUtils
  self subclassResponsibility
%

category: 'mocks'
method: TonelAbstractTest
mockCypressSnapshot
  ^ STON fromString: self class mockCypressSnapshotSTON
%

category: 'mocks'
method: TonelAbstractTest
mockMCSnapshot
	^ STON fromString: self class mockMCSnapshotSTON
%

! Class Implementation for TonelAbstractWriterTest

! ------------------- Class methods for TonelAbstractWriterTest

category: 'Testing'
classmethod: TonelAbstractWriterTest
isAbstract
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

	^self sunitName = #TonelAbstractWriterTest
%

! ------------------- Instance methods for TonelAbstractWriterTest

category: 'tests'
method: TonelAbstractWriterTest
expectedMCMockASubclassClassSt
	^ 'Class {
	#name : #MCMockASubclass,
	#superclass : #MCMockClassA,
	#instVars : [
		''x''
	],
	#classVars : [
		''Y''
	],
	#category : #MonticelloMocks
}

{ #category : #''as yet unclassified'' }
MCMockASubclass class >> initialize [

	InitializationOrder := InitializationOrder
		ifNil: [ -100 ] "let the test fail"
		ifNotNil: [ InitializationOrder + 1.]
]

{ #category : #''as yet unclassified'' }
MCMockASubclass >> variables [
	^ x + Y + MCMockClassA
]

{ #category : #''as yet unclassified'' }
MCMockASubclass >> variables2 [
	^ ivar + CVar
]
'
%

category: 'tests'
method: TonelAbstractWriterTest
expectedMCMockClassAClassSt
	^ '"
This is a mock class. The Monticello tests manipulated it to simulate a developer modifying code in the image.
"
Class {
	#name : #MCMockClassA,
	#superclass : #MCMock,
	#instVars : [
		''ivar''
	],
	#classVars : [
		''CVar'',
		''InitializationOrder''
	],
	#category : #MonticelloMocks
}

{ #category : #''as yet unclassified'' }
MCMockClassA class >> cVar [
	^ CVar
]

{ #category : #''as yet unclassified'' }
MCMockClassA class >> initializationOrder [

	^ InitializationOrder
]

{ #category : #''as yet unclassified'' }
MCMockClassA class >> initialize [
	CVar := #initialized.
	InitializationOrder := 1.

]

{ #category : #''as yet unclassified'' }
MCMockClassA class >> one [

	^ 1
]

{ #category : #''as yet unclassified'' }
MCMockClassA class >> touchCVar [
	CVar := #touched
]

{ #category : #numeric }
MCMockClassA >> a [
	^ ''a2''
]

{ #category : #numeric }
MCMockClassA >> b [
	^ ''b1''
]

{ #category : #numeric }
MCMockClassA >> c [
	^ ''c1''
]

{ #category : #numeric }
MCMockClassA >> d [
	^ ''d''
]

{ #category : #boolean }
MCMockClassA >> falsehood [
	^ false
]

{ #category : #boolean }
MCMockClassA >> moreTruth [

	^ true
]

{ #category : #numeric }
MCMockClassA >> one [
	^ 1
]

{ #category : #''drag\''n\''drop'' }
MCMockClassA >> q [
]

{ #category : #boolean }
MCMockClassA >> truth [
	^ true
]

{ #category : #numeric }
MCMockClassA >> two [
	^ 2
]
'
%

category: 'tests'
method: TonelAbstractWriterTest
expectedMCSnapshotTestExtensionSt
	^ 'Extension { #name : #MCSnapshotTest }

{ #category : #''*MonticelloMocks'' }
MCSnapshotTest >> mockClassExtension [

	"I change the protocol of this method to resolve the failing test: MCChangeNotificationTest >> testExtMethodModified. This test basically test that when we modified an extension method, the extended package is marked as ''modified''. The problem is that Monticello treat differently a classic method from an extension method, and this only by checking if the protocol name start with a star. Therefore, if the protocol does not match the extending package name, the extending package name will never be notified, and the test will fail. "
]
'
%

category: 'mocks'
method: TonelAbstractWriterTest
mockSnapshot

	self subclassResponsibility
%

category: 'running'
method: TonelAbstractWriterTest
setUp
  directory := nil.
  super setUp
%

category: 'tests'
method: TonelAbstractWriterTest
testWriteSnapshot
  | writer dir nl packageDir snapshot |
  dir := self directory.
  writer := self writerClass on: dir.
  snapshot := self mockSnapshot.
  snapshot dynamicInstVarAt: #'packageName' put: 'MonticelloMocks'.
  writer writeSnapshot: snapshot.
  self assert: (self directoryNamed: 'MonticelloMocks' existsIn: dir).
  packageDir := self directoryNamed: 'MonticelloMocks' in: dir.
  self
    assert: (self fileNamesIn: packageDir)
    equals:
      #('MCMockASubclass.class.st' 'MCMockClassA.class.st' 'MCMockClassB.class.st' 'MCMockClassD.class.st' 'MCMockClassE.class.st' 'MCMockClassF.class.st' 'MCMockClassG.class.st' 'MCMockClassH.class.st' 'MCMockClassI.class.st' 'MCSnapshotTest.extension.st' 'package.st').
  nl := TonelWriter lineEnding.
  self
    assert:
      (self contentsOfFileNamed: 'MCMockClassA.class.st' inDirectory: packageDir)
    equals: (self expectedMCMockClassAClassSt withLineEndings: nl).
  self
    assert:
      (self contentsOfFileNamed: 'MCMockASubclass.class.st' inDirectory: packageDir)
    equals: (self expectedMCMockASubclassClassSt withLineEndings: nl).
  self
    assert:
      (self contentsOfFileNamed: 'MCSnapshotTest.extension.st' inDirectory: packageDir)
    equals: (self expectedMCSnapshotTestExtensionSt withLineEndings: nl)
%

category: 'private'
method: TonelAbstractWriterTest
writerClass

  self subclassResponsibility
%

! Class Implementation for TonelReaderTest

! ------------------- Class methods for TonelReaderTest

category: 'Testing'
classmethod: TonelReaderTest
isAbstract
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

	^self sunitName = #TonelReaderTest
%

! ------------------- Instance methods for TonelReaderTest

category: 'tests'
method: TonelReaderTest
assertClassDefinition: a and: b
	self assert: a className equals: b className.
	self assert: a superclassName equals: b superclassName.
	self assert: a traitCompositionString equals: b traitCompositionString.
	self assert: a classTraitCompositionString equals: b classTraitCompositionString.
	self assert: a category equals: b category.	
	self assert: a instVarNames equals: b instVarNames.
	self assert: a classInstVarNames equals: b classInstVarNames.
	self assert: a classVarNames equals: b classVarNames.
	self assert: a poolDictionaries equals: b poolDictionaries.
	self assert: a type equals: b type.
	self assert: a comment equals: b comment.
%

category: 'tests'
method: TonelReaderTest
assertDefinition: a and: b
	a isOrganizationDefinition ifTrue: [ ^ self assertOrganisationDefinition: a and: b ].
	a isClassDefinition ifTrue: [ ^ self assertClassDefinition: a and: b ].
	a isMethodDefinition ifTrue: [ ^ self assertMethodDefinition: a and: b ].
%

category: 'tests'
method: TonelReaderTest
assertMethodDefinition: a and: b
	self assert: a className equals: b className.
	self assert: a selector equals: b selector.
	self assert: a protocol equals: b protocol.
	self assert: a source asByteArray equals: b source asByteArray.
	self assert: a classIsMeta equals: b classIsMeta
%

category: 'tests'
method: TonelReaderTest
assertOrganisationDefinition: a and: b
	self assert: a categories equals: b categories
%

category: 'mocks'
method: TonelReaderTest
mockSnapshot

	self subclassResponsibility
%

category: 'tests'
method: TonelReaderTest
testLoadDefinitions
  | snapshot reader |
  snapshot := self mockSnapshot.
  reader := self createReaderFor: snapshot fileName: 'MonticelloMocks'.
  reader loadDefinitions.
  self assert: reader definitions size equals: snapshot definitions size.
  reader definitions sorted
    with: snapshot definitions sorted
    do: [ :a :b | self assertDefinition: a and: b ]
%

! Class Implementation for TonelParserTest

! ------------------- Class methods for TonelParserTest

category: 'Testing'
classmethod: TonelParserTest
isAbstract
  "Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

  ^ self sunitName = #'TonelParserTest'
%

! ------------------- Instance methods for TonelParserTest

category: 'asserting'
method: TonelParserTest
assertParse: aString rule: rule equals: result 
	self 
		assert: (self parse: aString rule: rule)
		equals: result
%

category: 'private'
method: TonelParserTest
newClassDefinitionForClassNamed: nameString superclassName: superclassString category: categoryString instVarNames: ivarArray classVarNames: cvarArray poolDictionaryNames: poolArray classInstVarNames: civarArray type: typeSymbol comment: commentString
  self subclassResponsibility
%

category: 'private'
method: TonelParserTest
newClassDefinitionFrom: anArray
  self subclassResponsibility
%

category: 'private'
method: TonelParserTest
newMethodDefinitionForClassNamed: className classIsMeta: meta selector: selector category: category source: source
  self subclassResponsibility
%

category: 'private'
method: TonelParserTest
newTraitDefinitionFrom: anArray
  self subclassResponsibility
%

category: 'private'
method: TonelParserTest
newTypeDefinitionFrom: anArray
  self subclassResponsibility
%

category: 'private'
method: TonelParserTest
parse: aString rule: rule
  | parser |
  parser := TonelParser onString: aString forReader: self.
  ^ parser perform: rule
%

category: 'asserting'
method: TonelParserTest
shouldParse: aString rule: rule raise: error
  | parser |
  parser := TonelParser onString: aString  forReader: self.
  self should: [ parser perform: rule ] raise: error
%

category: 'tests'
method: TonelParserTest
testComment
	self 
		shouldParse: 'this "should" be an error'
		rule: #comment
		raise: TonelParseError.
	
	self  
		assertParse: '"this is a comment"'
		rule: #comment
		equals: 'this is a comment'.
	
	self  
		assertParse: '"""this"" is a comment with ""nested"" colons ""also at the end"""'
		rule: #comment
		equals: '"this" is a comment with "nested" colons "also at the end"'	
		
		
%

category: 'tests'
method: TonelParserTest
testExtractSelector
	| parser |
	
	parser := TonelParser new.
	
	self assert: (parser extractSelector: 'unary') equals: #unary.
	self assert: (parser extractSelector: '+ something') equals: #+.
	self assert: (parser extractSelector: '==> other') equals: #==>.
	self 
		assert: (parser extractSelector: 'some: arg1 keyword: arg2 selector: arg3') 
		equals: #some:keyword:selector:.
	self 
		assert: (parser extractSelector: 'some: 	arg1 keyword:arg2 selector: arg3') 
		equals: #some:keyword:selector:.
	self 
		assert: (parser extractSelector: 'some: arg1 
keyword: arg2 
selector: arg3') 
		equals: #some:keyword:selector:.
%

category: 'tests'
method: TonelParserTest
testMetadata
	self 
		assertParse: '{ #vars: [ #a, #b ] }' 
		rule: #metadata 
		equals: { #vars -> #(a b) } asDictionary.
	
	self 
		assertParse: '{ 
	#vars: [ #a, #b ],
	#uses: { #someNested: 42 } 
	}' 
		rule: #metadata 
		equals: { 
			#vars -> #(a b). 
			#uses -> { #someNested -> 42 } asDictionary
		} asDictionary
%

category: 'tests'
method: TonelParserTest
testMethod
	self 
		assertParse: 'Object>>name' 
		rule: #method
		equals: #(('Object' nil) 'name').
		
	self 
		assertParse: 'Object >> name: aString' 
		rule: #method
		equals: #(('Object' nil) 'name: aString').
		
	self 
		assertParse: 'Object >> name: aString [ I do not care ]' 
		rule: #method
		equals: #(('Object' nil) 'name: aString').
		
	self 
		assertParse: 'Object class >> name: aString' 
		rule: #method
		equals: #(('Object' 'class') 'name: aString').
%

category: 'tests'
method: TonelParserTest
testMethodBody
	self 
		assertParse: '[ method body... I will ignore what is inside ]'
		rule: #methodBody
		equals: ' method body... I will ignore what is inside'.

	self 
		assertParse: '[
method 
	[body... [I 
		will ignore] 
what] is inside
]'
		rule: #methodBody
		equals: '
method 
	[body... [I 
		will ignore] 
what] is inside'.
		
	self 
		assertParse: '[ method body with "''", ''"'', "[", '']'' ]'
		rule: #methodBody
		equals: ' method body with "''", ''"'', "[", '']'''.
	
%

category: 'tests'
method: TonelParserTest
testMethodDef
	self 
		assertParse: '
{ #category: ''accessing'' }
Object>>name [
	^ self printString
]'
		rule: #methodDef
		equals: (self newMethodDefinitionForClassNamed: #Object
			classIsMeta: false
			selector: #name
			category: 'accessing' 
			source: 'name
	^ self printString').
			
	self 
		assertParse: '
Object class>>name [ 
	^ self printString
]'
		rule: #methodDef
		equals: (self newMethodDefinitionForClassNamed: #Object
			classIsMeta: true
			selector: #name
			category: ''
			source: 'name 
	^ self printString').

	self 
		assertParse: '
TClass classSide >> template: aSystemCategoryName [ 
	"I really do not care"
]'
		rule: #methodDef
		equals: (self newMethodDefinitionForClassNamed: #TClass
			classIsMeta: true
			selector: #template:
			category: ''
			source: 'template: aSystemCategoryName 
	"I really do not care"').
%

category: 'tests'
method: TonelParserTest
testMethodDefList
	| parsed |
	
	parsed := self 
		parse: '
Object class>>new [
	^ self basicNew initialize
]

{ #category: ''accessing'' }
Object>>name [
	^ self printString
]

{ #category: ''printing'' }
Object>>printOn: aStream [
	"Append to the argument, aStream, a sequence of characters that  
	identifies the receiver."

	| title |
	title := self class name.
	aStream
		nextPutAll: (title first isVowel ifTrue: [''an ''] ifFalse: [''a '']);
		nextPutAll: title

]'
		rule: #methodDefList.
		
	self assert: parsed size equals: 3
%

category: 'tests'
method: TonelParserTest
testRemoveFromEnclosingStartEnd
  | parser nl |
  nl := TonelWriter lineEnding.
  parser := TonelParser new.
  self
    assert: (parser removeFrom: '[ ^ self ]' enclosingStart: $[ end: $])
    equals: '^ self'.
  self
    assert:
      (parser
        removeFrom:
          ('[ 
	^ self ]' withLineEndings: nl)
        enclosingStart: $[
        end: $])
    equals: Character tab asString , '^ self'.
  self
    assert:
      (parser
        removeFrom:
          ('[ 
	^ self
	]' withLineEndings: nl)
        enclosingStart: $[
        end: $])
    equals: Character tab asString , '^ self'.
  self
    assert:
      (parser
        removeFrom:
          ('[ 

	^ self

]' withLineEndings: nl)
        enclosingStart: $[
        end: $])
    equals:
      ('
	^ self
' withLineEndings: nl)
%

category: 'tests'
method: TonelParserTest
testType
	self assertParse: 'Class' rule: #type equals: 'Class'.
	self assertParse: 'Trait' rule: #type equals: 'Trait'.
	self assertParse: 'Extension' rule: #type equals: 'Extension'
%

category: 'tests'
method: TonelParserTest
testTypeDef
  self
    assertParse:
      '
"
this is a test
"
Class { 
	#name: ''X'',
	#superclass: ''Y'',
	#category: ''Z'' 
}'
    rule: #'typeDef'
    equals:
      (self
        newClassDefinitionForClassNamed: 'X'
        superclassName: 'Y'
        category: 'Z'
        instVarNames: #()
        classVarNames: #()
        poolDictionaryNames: #()
        classInstVarNames: #()
        type: #'normal'
        comment: 'this is a test')
%

category: 'tests'
method: TonelParserTest
testTypeDefWithClassVars
  self
    assertParse:
      '
"
this is a test
"
Class {
	#name : #MCMockASubclass,
	#superclass : #MCMockClassA,
	#instVars : [
		''x''
	],
	#classVars : [
		''Y''
	],
	#category : #MonticelloMocks
}
'
    rule: #'typeDef'
    equals:
      (self
        newClassDefinitionForClassNamed: 'MCMockASubclass'
        superclassName: 'MCMockClassA'
        category: 'MonticelloMocks'
        instVarNames: #(#'x')
        classVarNames: #(#'Y')
        poolDictionaryNames: #()
        classInstVarNames: #()
        type: #'normal'
        comment: 'this is a test')
%

! Class Implementation for TonelWriterTest

! ------------------- Class methods for TonelWriterTest

category: 'Testing'
classmethod: TonelWriterTest
isAbstract
  "Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

  ^ self sunitName = #'TonelWriterTest'
%

! ------------------- Instance methods for TonelWriterTest

category: 'private'
method: TonelWriterTest
creatClassDefinition: nameString superclassName: superclassString category: categoryString instVarNames: ivarArray classVarNames: cvarArray poolDictionaryNames: poolArray classInstVarNames: civarArray type: typeSymbol comment: commentString
  self subclassResponsibility
%

category: 'private'
method: TonelWriterTest
creatClassDefinition: nameString superclassName: superclassString traitComposition: traitCompositionString classTraitComposition: classTraitCompositionString category: categoryString instVarNames: ivarArray classVarNames: cvarArray poolDictionaryNames: poolArray classInstVarNames: civarArray type: typeSymbol comment: commentString
  self subclassResponsibility
%

category: 'private'
method: TonelWriterTest
creatMethodDefinition: classString classIsMeta: metaBoolean selector: selectorString category: catString source: sourceString
  self subclassResponsibility
%

category: 'private'
method: TonelWriterTest
creatMethodDefinition: classString selector: selectorString category: catString source: sourceString
  ^ self
    creatMethodDefinition: classString
    classIsMeta: false
    selector: selectorString
    category: catString
    source: sourceString
%

category: 'private'
method: TonelWriterTest
defaultPackageWriter
  self subclassResponsibility
%

category: 'tests'
method: TonelWriterTest
testSplitMethodSourceInto
  | writer declaration source definition newLine tab space |
  newLine := TonelWriter lineEnding.
  tab := Character tab asString.
  space := Character space asString.
  writer := TonelWriter new.	"simplest split"
  definition := self creatMethodDefinition: #'SomeClass'
    selector: #'name'
    category: 'accessing'
    source:
      'name
	^ self'.
  writer
    splitMethodSource: definition
    into: [ :d :s | 
      declaration := d.
      source := s ].
  self assert: declaration equals: 'name'.
  self assert: source equals: newLine , tab , '^ self'.	"test space at the end of method declaration (it needs to be kept)"
  definition := self creatMethodDefinition: #'SomeClass'
    selector: #'name:'
    category: 'accessing'
    source:
      'name: aString 
	name := aString'.
  writer
    splitMethodSource: definition
    into: [ :d :s | 
      declaration := d.
      source := s ].
  self assert: declaration equals: 'name: aString'.
  self assert: source equals: space , newLine , tab , 'name := aString'.	"test multiline declaration"
  definition := self creatMethodDefinition: #'SomeClass'
    selector: #'method:with:several:lines:'
    category: 'accessing'
    source:
      'method: var1
	with: var2
	several: var3
	lines: var4
	
	^ var1 + var2 + var3 + var4'.
  writer
    splitMethodSource: definition
    into: [ :d :s | 
      declaration := d.
      source := s ].
  self
    assert: declaration
    equals:
      ('method: var1
	with: var2
	several: var3
	lines: var4'
        withLineEndings: TonelWriter lineEnding).
  self
    assert: source
    equals:
      (newLine , tab , newLine , tab , '^ var1 + var2 + var3 + var4'
        withLineEndings: newLine).	"test comment before declaration (it may happen, if someone copied from diffmorph)"
  definition := self creatMethodDefinition: #'SomeClass'
    selector: #'name:'
    category: 'accessing'
    source:
      '
"protocol: accessing"
name: aString 
	name := aString'.
  writer
    splitMethodSource: definition
    into: [ :d :s | 
      declaration := d.
      source := s ].
  self assert: declaration equals: 'name: aString'.
  self assert: source equals: space , newLine , tab , 'name := aString'.	"test source right after declaration (no enter between selector and source)"
  definition := self creatMethodDefinition: #'SomeClass'
    selector: #'name:'
    category: 'accessing'
    source: 'name: aString name := aString'.
  writer
    splitMethodSource: definition
    into: [ :d :s | 
      declaration := d.
      source := s ].
  self assert: declaration equals: 'name: aString'.
  self assert: source equals: ' name := aString'.	"test method name containin name of keywords in variables"
  definition := self creatMethodDefinition: #'SomeClass'
    selector: #'a:b:c:'
    category: 'accessing'
    source: 'a: b b: c c: d ^ 42'.
  writer
    splitMethodSource: definition
    into: [ :d :s | 
      declaration := d.
      source := s ].
  self assert: declaration equals: 'a: b b: c c: d'.
  self assert: source equals: ' ^ 42'
%

category: 'tests'
method: TonelWriterTest
testWriteClassDefinitionOn
  | writer def stream |
  writer := TonelWriter new
    packageWriter: self defaultPackageWriter;
    yourself.
  stream := TonelParser writeStreamClass on: String new.
  def := self
    creatClassDefinition: #'SomeObject'
    superclassName: #'Object'
    category: #'Kernel'
    instVarNames: #()
    classVarNames: #()
    poolDictionaryNames: #()
    classInstVarNames: #()
    type: #'normal'
    comment: 'comment test'.
  writer writeClassDefinition: def on: stream.
  self
    assert: stream contents
    equals:
      ('"
comment test
"
Class {
	#name : #SomeObject,
	#superclass : #Object,
	#category : #Kernel
}
'
        withLineEndings: TonelWriter lineEnding).
  stream := String new writeStreamPortable.
  def := self
    creatClassDefinition: #'SomeObject'
    superclassName: #'Object'
    category: #'Kernel'
    instVarNames: #(#'a' #'b' #'c')
    classVarNames: #(#'D' #'E')
    poolDictionaryNames: #(#'POOL')
    classInstVarNames: #(#'instVarA')
    type: #'normal'
    comment: 'comment test'.
  writer writeClassDefinition: def on: stream.
  self
    assert: stream contents
    equals:
      ('"
comment test
"
Class {
	#name : #SomeObject,
	#superclass : #Object,
	#instVars : [
		''a'',
		''b'',
		''c''
	],
	#classVars : [
		''D'',
		''E''
	],
	#pools : [
		''POOL''
	],
	#classInstVars : [
		''instVarA''
	],
	#category : #Kernel
}
'
        withLineEndings: TonelWriter lineEnding)
%

category: 'tests'
method: TonelWriterTest
testWriteMethodDefinitionOn
  | writer def stream |
  writer := TonelWriter new.
  stream := String new writeStreamPortable.
  def := self creatMethodDefinition: #'Object'
    classIsMeta: false
    selector: #'selector'
    category: 'accessing'
    source:
      'selector
	^ 42'.
  writer writeMethodDefinition: def on: stream.
  self
    assert: stream contents
    equals:
      ('
{ #category : #accessing }
Object >> selector [
	^ 42
]
'
        withLineEndings: TonelWriter lineEnding).
  stream := String new writeStreamPortable.
  def := self creatMethodDefinition: #'Object'
    classIsMeta: true
    selector: #'selector'
    category: 'accessing'
    source:
      'selector
	^ 42'.
  writer writeMethodDefinition: def on: stream.
  self
    assert: stream contents
    equals:
      ('
{ #category : #accessing }
Object class >> selector [
	^ 42
]
'
        withLineEndings: TonelWriter lineEnding).
  stream := String new writeStreamPortable.
  def := self creatMethodDefinition: #'Object'
    classIsMeta: false
    selector: #'='
    category: 'comparing'
    source:
      '= anObject
	^ self == anObject'.
  writer writeMethodDefinition: def on: stream.
  self
    assert: stream contents
    equals:
      ('
{ #category : #comparing }
Object >> = anObject [
	^ self == anObject
]
'
        withLineEndings: TonelWriter lineEnding)
%

category: 'tests'
method: TonelWriterTest
testWritePackageOn
  | writer def stream |
  writer := TonelWriter new.
  stream := TonelParser writeStreamClass on: String new.
  writer writePackage: 'ThePackage' on: stream.
  self
    assert: stream contents
    equals:
      ('Package { #name : #ThePackage }
'
).

  stream := TonelParser writeStreamClass on: String new.
  writer writePackage: 'The-Package' on: stream.
  self
    assert: stream contents
    equals:
      ('Package { #name : #The-Package }
'
).

  stream := TonelParser writeStreamClass on: String new.
  writer writePackage: 'The Package' on: stream.
  self
    assert: stream contents
    equals:
      ('Package { #name : #''The Package'' }
'
).
%

! Class Extensions

! Class initializers 

doit
true.
%



! End of Package: Tonel-Tests


