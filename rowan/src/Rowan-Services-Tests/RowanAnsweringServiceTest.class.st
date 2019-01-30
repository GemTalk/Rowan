Class {
	#name : 'RowanAnsweringServiceTest',
	#superclass : 'RowanServicesTest',
	#category : 'Rowan-Services-Tests'
}

{ #category : 'tests' }
RowanAnsweringServiceTest >> test_flipAutoCommit [
	| answeringService autoCommit | 
	self jadeiteIssueTested: #issue396 withTitle: 'Ability to turn on autocommit would be nice'.
	answeringService := RowanAnsweringService new organizer: ClassOrganizer new.  
	autoCommit := RowanService autoCommit. 
	[answeringService flipAutoCommit. 
	self assert: RowanService autoCommit equals: autoCommit not. 
	self assert: (SessionTemps current at: #'Jadeite_AutoCommit') equals: autoCommit not.
	answeringService flipAutoCommit. 
	self assert: RowanService autoCommit equals: autoCommit. 
	self assert: (SessionTemps current at: #'Jadeite_AutoCommit') equals: autoCommit] ensure: [
		RowanService setAutoCommit: autoCommit.
		self assert: RowanService autoCommit equals: autoCommit.]
]

{ #category : 'tests' }
RowanAnsweringServiceTest >> test_initializeAutoCommit [
	| answeringService autoCommit | 
	self jadeiteIssueTested: #issue396 withTitle: 'Ability to turn on autocommit would be nice'.
	answeringService := RowanAnsweringService new organizer: ClassOrganizer new.  
	autoCommit := RowanService autoCommit. 
	self assert: RowanService autoCommit equals: autoCommit.
	self assert: (SessionTemps current at: #'Jadeite_AutoCommit') equals: autoCommit.
	[answeringService initializeAutoCommit. 
	self deny: RowanService autoCommit. 
	self deny: (SessionTemps current at: #'Jadeite_AutoCommit')] ensure: [
		RowanService setAutoCommit: autoCommit.
		self assert: RowanService autoCommit equals: autoCommit.]
]

{ #category : 'tests' }
RowanAnsweringServiceTest >> test_loadedPackageExists [
	| answeringService |
	self jadeiteIssueTested: #issue205 withTitle: 'misspelled extension category name causes trouble'.
	answeringService := RowanAnsweringService new organizer: ClassOrganizer new.
	Rowan packageNames do:[:packageName |
		self assert: (answeringService loadedPackageExists: packageName) answer].
	self deny: (answeringService loadedPackageExists: 'AAAA') answer.
	self deny: (answeringService loadedPackageExists: 'AJsfdjsdf') answer.
	self assert: (answeringService loadedPackageExists: 'Rowan-Kernel') answer. 
	self assert: (answeringService loadedPackageExists: 'rowan-kernel') answer. "lower case is accepted"
]

{ #category : 'tests' }
RowanAnsweringServiceTest >> test_matchingPattern [
	| answeringService randomSymbol | 
	self jadeiteIssueTested: #issue235 withTitle: 'Need Find Class/Method from console'.
	answeringService := RowanAnsweringService new organizer: ClassOrganizer new.  
	answeringService selectorsMatchingPattern: #('size'). 
	self assert: answeringService answer size equals: 1. 
	self assert: answeringService answer first = #size.

	answeringService selectorsMatchingPattern: #('SIZE').  "no case match"
	self assert: answeringService answer size equals: 1. 
	self assert: answeringService answer first = #size.

	answeringService selectorsMatchingPattern: #('test_matching' $*). 
	self assert: answeringService answer size equals: 1.
	self assert: answeringService answer first = #test_matchingPattern.

	answeringService selectorsMatchingPattern: #($* 'test_matching' ). 
	self assert: answeringService answer size equals: 0.

	"sorted result"
	answeringService selectorsMatchingPattern: #('size' $*). 
	self assert: answeringService answer size > 1. 
	1 to: answeringService answer size - 1 do:[:idx | 
		self assert: (answeringService answer at: idx) < (answeringService answer at: idx + 1)].
]

{ #category : 'tests' }
RowanAnsweringServiceTest >> test_setAutoCommit [
	| answeringService autoCommit | 
	self jadeiteIssueTested: #issue396 withTitle: 'Ability to turn on autocommit would be nice'.
	answeringService := RowanAnsweringService new organizer: ClassOrganizer new.  
	autoCommit := RowanService autoCommit. 
	[answeringService setAutoCommit: true. 
	self assert: RowanService autoCommit. 
	self assert: (SessionTemps current at: #'Jadeite_AutoCommit').
	answeringService setAutoCommit: false. 
	self deny: RowanService autoCommit. 
	self deny: (SessionTemps current at: #'Jadeite_AutoCommit')] ensure: [
		RowanService setAutoCommit: autoCommit.
		self assert: RowanService autoCommit equals: autoCommit.]
]
