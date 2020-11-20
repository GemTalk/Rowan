"
No class-specific documentation for RowanDictionaryService, hierarchy is:
Object
  RowanService( definition updates command commandArgs updateType organizer)
    RowanDictionaryService( name classes hierarchyServices globals defaultTemplate)

"
Class {
	#name : 'RowanDictionaryService',
	#superclass : 'RowanService',
	#instVars : [
		'name',
		'classes',
		'hierarchyServices',
		'globals',
		'defaultTemplate'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'accessing' }
RowanDictionaryService >> classes [
	^classes
]

{ #category : 'accessing' }
RowanDictionaryService >> classes: object [
	classes := object
]

{ #category : 'client commands' }
RowanDictionaryService >> classHierarchy [
	| theClasses |
	self update. 
	theClasses := classes collect:[:classService | classService theClass].
	"reuse behavior in package service for now" 
	hierarchyServices := (RowanPackageService new classes: classes) classHierarchy: theClasses. 
	RowanCommandResult addResult: self.
]

{ #category : 'Updating' }
RowanDictionaryService >> defaultTemplate: newValue [
	defaultTemplate := newValue
]

{ #category : 'command support' }
RowanDictionaryService >> genericClassCreationTemplate [

	^self browserTool classCreationTemplateForSubclassOf: 'Object' category: nil packageName: nil
]

{ #category : 'initialization' }
RowanDictionaryService >> initialize [

	self setDefaultTemplate
]

{ #category : 'client commands' }
RowanDictionaryService >> insertAt: index [

	| theDictionary |
	theDictionary := SymbolDictionary new. 
	theDictionary at: name asSymbol put: theDictionary. 
	System myUserProfile insertDictionary: theDictionary at: index. 
	RowanBrowserService new updateDictionaries.
]

{ #category : 'testing' }
RowanDictionaryService >> isDictionaryService [
  ^ true
]

{ #category : 'accessing' }
RowanDictionaryService >> name [
	^name
]

{ #category : 'accessing' }
RowanDictionaryService >> name: object [
	name := object
]

{ #category : 'client commands' }
RowanDictionaryService >> removeClass: classService [
	self removeClassNamed: classService name. 
	self setDefaultTemplate.
	classService updateType: #removedClass:.
	RowanCommandResult addResult: classService
]

{ #category : 'client commands' }
RowanDictionaryService >> removeClassNamed: className [

	self browserTool removeClassNamed: className.

]

{ #category : 'client commands' }
RowanDictionaryService >> removeGlobalNamed: symbol [
	| dictionary |
	dictionary := (System myUserProfile resolveSymbol: name asSymbol) value.
	dictionary ifNotNil: [
		dictionary removeKey: symbol ifAbsent:[]].
	self update.
]

{ #category : 'perform' }
RowanDictionaryService >> servicePerform: symbol withArguments: collection [
  self isUpdatingButFoundToBeDeleted
    ifTrue: [ ^ self handleDeletedService ].
  super servicePerform: symbol withArguments: collection.
]

{ #category : 'client commands' }
RowanDictionaryService >> setDefaultTemplate [

	defaultTemplate := self genericClassCreationTemplate.
]

{ #category : 'updates' }
RowanDictionaryService >> update [
	| dictionary sorted |
	super update.
	classes := Array new.
	sorted := SortedCollection sortBlock: [ :x :y | x first < y first ].
	dictionary := Rowan image symbolList objectNamed: name.
	dictionary ifNil: [ ^ self ].
	(dictionary isKindOf: SymbolDictionary)
		ifFalse: [ ^ self ].
	dictionary
		keysAndValuesDo: [ :key :value | 
			value isClass
				ifTrue: [ 
					| classService |
					classService := RowanClassService new name: key asString.
					classService versions: value classHistory size.
					classService version: (value classHistory indexOf: value).
					classes add: classService ]
				ifFalse: [ 
					| printString theKey |
					printString := [ 
					value printString charSize > 1
						ifTrue: [ '<<unprintable string. charSize > 1>>' ]
						ifFalse: [ value printString ] ]
						on: Error
						do: [ :ex | 'unprintable string. Error - <' , ex printString , '>' ].
					key charSize = 1
						ifTrue: [ theKey := key ]
						ifFalse: [ theKey := '<<unprintable string. charSize > 1>>' ].
					sorted
						add:
							(Array
								with: name , '.' , theKey
								with: value class name
								with: value asOop
								with: printString) ] ].
	globals := sorted asArray.
	RowanCommandResult addResult: self
]

{ #category : 'testing' }
RowanDictionaryService >> wasDeleted [
  ^ (Rowan globalNamed: name) isNil
]
