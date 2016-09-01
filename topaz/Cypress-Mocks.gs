! Package: Cypress-Mocks


! Remove existing behavior from package Cypress-Mocks
!!!! This can be cleaned up when some package functionality is moved to the base system.

doit
| packageName |
packageName := 'Cypress-Mocks'.
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
(Object
	subclass: 'CypressMockBasic'
	instVarNames: #( name )
	classVars: #( Something )
	classInstVars: #( current )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Mocks-Definitions';
		comment: 'This mock contains basic class and instance method selectors';
		immediateInvariant.
true.
%

! Class Implementation for CypressMockBasic

! ------------------- Class methods for CypressMockBasic

category: 'accessing'
classmethod: CypressMockBasic
current
	^current
%

category: 'accessing'
classmethod: CypressMockBasic
current: anObject
	current := anObject
%

category: 'initialization'
classmethod: CypressMockBasic
initialize
	self current: self new
%

! ------------------- Instance methods for CypressMockBasic

category: 'accessing'
method: CypressMockBasic
extra
	"extra method"
%

category: 'initialization'
method: CypressMockBasic
initialize
	super initialize.
	self name: 'Unknown'
%

category: 'accessing'
method: CypressMockBasic
name
	^name
%

category: 'accessing'
method: CypressMockBasic
name: aString
	name := aString
%

! Class Extensions

! Class Extension for Object

! ------------------- Instance methods for Object

category: '*Cypress-Mocks-Extensions'
method: Object
isCypressMockBasic

	^false
%

! Class initializers 

doit
CypressMockBasic initialize.
true.
%



! End of Package: Cypress-Mocks


