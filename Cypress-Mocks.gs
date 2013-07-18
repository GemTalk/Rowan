doit
(CypressObject subclass: 'CypressMockBasic'  instVarNames: #( name)  classVars: #()  classInstVars: #( current)  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Mocks'
.
%
doit
CypressMockBasic immediateInvariant.
%

! Remove existing behavior from CypressMockBasic
doit
CypressMockBasic removeAllMethods.
CypressMockBasic class removeAllMethods.
%
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
category: 'initialization'
set compile_env: 0
method: CypressMockBasic
initialize
	super initialize.
	self name: 'Unknown'
%
