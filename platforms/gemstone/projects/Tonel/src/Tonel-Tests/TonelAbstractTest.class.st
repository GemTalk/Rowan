Class {
	#name : 'TonelAbstractTest',
	#superclass : 'TestCase',
	#category : 'Tonel-Tests'
}

{ #category : 'Testing' }
TonelAbstractTest class >> isAbstract [
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

	^self sunitName = #TonelAbstractTest

]

{ #category : 'private' }
TonelAbstractTest class >> mockCypressSnapshotSTON [
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

]

{ #category : 'private' }
TonelAbstractTest class >> mockMCSnapshotSTON [
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

]

{ #category : 'private' }
TonelAbstractTest >> fileUtils [
  self subclassResponsibility

]

{ #category : 'mocks' }
TonelAbstractTest >> mockCypressSnapshot [
  ^ STON fromString: self class mockCypressSnapshotSTON

]

{ #category : 'mocks' }
TonelAbstractTest >> mockMCSnapshot [
	^ STON fromString: self class mockMCSnapshotSTON

]
