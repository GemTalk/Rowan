! Package: CypressTonel-Tests


! Remove existing behavior from package CypressTonel-Tests
!!!! This can be cleaned up when some package functionality is moved to the base system.

doit
| packageName |
packageName := 'CypressTonel-Tests'.
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
(TonelAbstractWriterTest
	subclass: 'TonelCypressWriterTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'CypressTonel-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TonelReaderTest
	subclass: 'TonelCypressReaderTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'CypressTonel-Tests';
		comment: '';
		immediateInvariant.
true.
%

! Class Implementation for TonelCypressWriterTest

! ------------------- Instance methods for TonelCypressWriterTest

category: 'private'
method: TonelCypressWriterTest
fileUtils
  ^ CypressFileUtilities current
%

category: 'mocks'
method: TonelCypressWriterTest
mockSnapshot

	^ self mockCypressSnapshot
%

category: 'private'
method: TonelCypressWriterTest
writerClass

  ^ TonelCypressWriter
%

! Class Implementation for TonelCypressReaderTest

! ------------------- Instance methods for TonelCypressReaderTest

category: 'tests'
method: TonelCypressReaderTest
assertClassDefinition: a and: b
	self assert: a className equals: b className.
	self assert: a superclassName equals: b superclassName.
	self assert: a category equals: b category.	
	self assert: a instVarNames equals: b instVarNames.
	self assert: a classInstVarNames equals: b classInstVarNames.
	self assert: a classVarNames equals: b classVarNames.
	self assert: a poolDictionaries equals: b poolDictionaries.
	self assert: a type equals: b type.
	self assert: a comment equals: b comment.
%

category: 'tests'
method: TonelCypressReaderTest
assertDefinition: a and: b
	a isClassDefinition ifTrue: [ ^ self assertClassDefinition: a and: b ].
	a isMethodDefinition ifTrue: [ ^ self assertMethodDefinition: a and: b ].
%

category: 'tests'
method: TonelCypressReaderTest
assertMethodDefinition: a and: b
	self assert: a className equals: b className.
	self assert: a selector equals: b selector.
	self assert: a category equals: b category.
	self assert: a source asByteArray equals: b source asByteArray.
	self assert: a classIsMeta equals: b classIsMeta
%

category: 'private'
method: TonelCypressReaderTest
fileUtils
  ^ CypressFileUtilities current
%

category: 'mocks'
method: TonelCypressReaderTest
mockSnapshot
  ^ self mockCypressSnapshot
%

! Class Extensions

! Class initializers 

doit
true.
%



! End of Package: CypressTonel-Tests


