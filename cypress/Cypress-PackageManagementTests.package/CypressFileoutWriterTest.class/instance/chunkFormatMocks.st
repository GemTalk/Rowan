"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: utility
"
chunkFormatMocks

	^'" Package: Cypress-Mocks"!


" Class Declarations "!

(Object
	subclass: ''CypressMockBasic''
	instVarNames: #( name )
	classVars: #( Something )
	classInstVars: #( current )
	poolDictionaries: #())
		category: ''Cypress-Mocks-Definitions'';
		comment: ''This mock contains basic class and instance method selectors''!

" Class Implementation for CypressMockBasic"!

" ------------------- Class methods for CypressMockBasic"!

!CypressMockBasic class methodsFor: ''accessing''!
current
	^current
! !
!CypressMockBasic class methodsFor: ''accessing''!
current: anObject
	current := anObject
! !
!CypressMockBasic class methodsFor: ''initialization''!
initialize
	self current: self new
! !
" ------------------- Instance methods for CypressMockBasic"!

!CypressMockBasic methodsFor: ''accessing''!
extra
	"extra method"
! !
!CypressMockBasic methodsFor: ''initialization''!
initialize
	super initialize.
	self name: ''Unknown''
! !
!CypressMockBasic methodsFor: ''accessing''!
name
	^name
! !
!CypressMockBasic methodsFor: ''accessing''!
name: aString
	name := aString
! !
" Class Extensions "!

" Class Extension for Object"!

" ------------------- Instance methods for Object"!

!Object methodsFor: ''*Cypress-Mocks-Extensions''!
isCypressMockBasic

	^false! !
" Class initializers "!

CypressMockBasic initialize.!



" End of Package: Cypress-Mocks"!


'
