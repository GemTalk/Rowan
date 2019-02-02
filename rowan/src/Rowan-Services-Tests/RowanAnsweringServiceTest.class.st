Class {
	#name : 'RowanAnsweringServiceTest',
	#superclass : 'RowanServicesTest',
	#instVars : [
		'service'
	],
	#category : 'Rowan-Services-Tests'
}

{ #category : 'support' }
RowanAnsweringServiceTest >> setUp [

	super setUp. 
	service := RowanAnsweringService new organizer: ClassOrganizer new.
]

{ #category : 'tests' }
RowanAnsweringServiceTest >> test_initializeAutoCommit [
	| autoCommit | 
	self jadeiteIssueTested: #issue396 withTitle: 'Ability to turn on autocommit would be nice'.
	autoCommit := RowanService autoCommit. 
	self assert: RowanService autoCommit equals: autoCommit.
	self assert: (SessionTemps current at: #'Jadeite_AutoCommit') equals: autoCommit.
	[service initializeAutoCommit. 
	self deny: RowanService autoCommit. 
	self deny: (SessionTemps current at: #'Jadeite_AutoCommit')] ensure: [
		RowanService setAutoCommit: autoCommit.
		self assert: RowanService autoCommit equals: autoCommit.]
]

{ #category : 'tests' }
RowanAnsweringServiceTest >> test_loadedPackageExists [
	self jadeiteIssueTested: #issue205 withTitle: 'misspelled extension category name causes trouble'.
	Rowan packageNames do:[:packageName |
		self assert: (service loadedPackageExists: packageName) answer].
	self deny: (service loadedPackageExists: 'AAAA') answer.
	self deny: (service loadedPackageExists: 'AJsfdjsdf') answer.
	self assert: (service loadedPackageExists: 'Rowan-Kernel') answer. 
	self assert: (service loadedPackageExists: 'rowan-kernel') answer. "lower case is accepted"
]

{ #category : 'tests' }
RowanAnsweringServiceTest >> test_matchingPattern [
	| randomSymbol | 
	self jadeiteIssueTested: #issue235 withTitle: 'Need Find Class/Method from console'.
	service selectorsMatchingPattern: #('size'). 
	self assert: service answer size equals: 1. 
	self assert: service answer first = #size.

	service selectorsMatchingPattern: #('SIZE').  "no case match"
	self assert: service answer size equals: 1. 
	self assert: service answer first = #size.

	service selectorsMatchingPattern: #('test_matching' $*). 
	self assert: service answer size equals: 1.
	self assert: service answer first = #test_matchingPattern.

	service selectorsMatchingPattern: #($* 'test_matching' ). 
	self assert: service answer size equals: 0.

	"sorted result"
	service selectorsMatchingPattern: #('size' $*). 
	self assert: service answer size > 1. 
	1 to: service answer size - 1 do:[:idx | 
		self assert: (service answer at: idx) < (service answer at: idx + 1)].
]

{ #category : 'tests' }
RowanAnsweringServiceTest >> test_maxPrint [
	self jadeiteIssueTested: #issue398 withTitle: 'String Inspectors don''t display lfs'.
	self assert: (service printStringOf: 123 asOop toMaxSize: 5) equals: '123'.
	self assert: (service printStringOf: 123 asOop toMaxSize: 2) equals: '12...'.
	self assert: (service printStringOf: 'abc' asOop toMaxSize: 2) equals: '''a...'.
	self assert: (service printStringOf: Object new asOop toMaxSize: 25) equals: 'anObject'.
	self assert: (service printStringOf: 'ab
cd' asOop toMaxSize: 25) equals: '''ab
cd'''.  "includes lf (10) - no ? substitution"
]

{ #category : 'tests' }
RowanAnsweringServiceTest >> test_setAutoCommit [
	| autoCommit | 
	self jadeiteIssueTested: #issue396 withTitle: 'Ability to turn on autocommit would be nice'.
	autoCommit := RowanService autoCommit. 
	[service setAutoCommit: true. 
	self assert: RowanService autoCommit. 
	self assert: (SessionTemps current at: #'Jadeite_AutoCommit').
	service setAutoCommit: false. 
	self deny: RowanService autoCommit. 
	self deny: (SessionTemps current at: #'Jadeite_AutoCommit')] ensure: [
		RowanService setAutoCommit: autoCommit.
		self assert: RowanService autoCommit equals: autoCommit.]
]
