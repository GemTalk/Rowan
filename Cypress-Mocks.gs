! Package: Cypress-Mocks
! Written: 2013-07-19T14:38:05.78616499900818-07:00

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

