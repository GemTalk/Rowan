SystemOrganization addCategory: 'Cypress-Mocks-Definitions'!

Object subclass: #CypressMockBasic
	instanceVariableNames: 'name'
	classVariableNames: 'Something'
	poolDictionaries: ''
	category: 'Cypress-Mocks-Definitions'!

CypressMockBasic class
	instanceVariableNames: 'current'!

!CypressMockBasic commentStamp: '' prior: 0!
This mock contains basic class and instance method selectors!

!CypressMockBasic class methodsFor: 'accessing' stamp: ''!
current	^current! !

!CypressMockBasic class methodsFor: 'accessing' stamp: ''!
current: anObject	current := anObject! !

!CypressMockBasic class methodsFor: 'initialization' stamp: ''!
initialize	self current: self new! !

!CypressMockBasic methodsFor: 'accessing' stamp: ''!
extra	"extra method"! !

!CypressMockBasic methodsFor: 'initialization' stamp: ''!
initialize	super initialize.	self name: 'Unknown'! !

!CypressMockBasic methodsFor: 'accessing' stamp: ''!
name	^name! !

!CypressMockBasic methodsFor: 'accessing' stamp: ''!
name: aString	name := aString! !

!Object methodsFor: '*Cypress-Mocks-Extensions' stamp: ''!
isCypressMockBasic	^false! !
CypressMockBasic initialize!
