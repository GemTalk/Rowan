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
%


! Class Declarations

doit
(Object
	subclass: 'CypressMockBasic'
	instVarNames: #( name )
	classVars: #( Something )
	classInstVars: #( current )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Mocks';
		comment: 'This mock contains basic class and instance method selectors';
		immediateInvariant.
%

! Class Implementation for CypressMockBasic

! ------------------- Class methods for CypressMockBasic

category: 'accessing'
set compile_env: 0
classmethod: CypressMockBasic
current
	^current
%

category: 'accessing'
set compile_env: 0
classmethod: CypressMockBasic
current: anObject
	current := anObject
%

category: 'initialization'
set compile_env: 0
classmethod: CypressMockBasic
initialize
	self current: self new
%

! ------------------- Instance methods for CypressMockBasic

category: 'accessing'
set compile_env: 0
method: CypressMockBasic
extra
	"extra method"
%

category: 'initialization'
set compile_env: 0
method: CypressMockBasic
initialize
	super initialize.
	self name: 'Unknown'
%

category: 'accessing'
set compile_env: 0
method: CypressMockBasic
name
	^name
%

category: 'accessing'
set compile_env: 0
method: CypressMockBasic
name: aString
	name := aString
%

! Class Extensions

! Class Extension for Object

! ------------------- Instance methods for Object

category: '*Cypress-Mocks-Extensions'
set compile_env: 0
method: Object
isCypressMockBasic

	^false
%

! ------------------- Class initializers 

doit
CypressMockBasic initialize.
%



! End of Package: Cypress-Mocks


