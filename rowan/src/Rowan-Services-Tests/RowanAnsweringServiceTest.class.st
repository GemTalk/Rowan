Class {
	#name : 'RowanAnsweringServiceTest',
	#superclass : 'RowanServicesTest',
	#category : 'Rowan-Services-Tests'
}

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
