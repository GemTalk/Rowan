Class {
	#name : 'RowanDictionaryServiceTest',
	#superclass : 'RowanServicesTest',
	#category : 'Rowan-Services-Tests'
}

{ #category : 'tests' }
RowanDictionaryServiceTest >> test_insertRemoveDictionary [
	| dictionaryService browserService |
	dictionaryService := RowanDictionaryService new name: 'FnoodleDictionary'. 
	dictionaryService insertAt: 1. 
	self assert: Rowan image symbolList first name equals: #FnoodleDictionary.

	browserService := RowanBrowserService new. 
	browserService removeDictionariesNamed: #('FnoodleDictionary').
	self deny: Rowan image symbolList first name equals: #FnoodleDictionary.
]
