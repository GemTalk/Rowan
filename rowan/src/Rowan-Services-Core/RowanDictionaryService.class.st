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
	System commitTransaction.
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

	super servicePerform: symbol withArguments: collection.
	self update.
]

{ #category : 'client commands' }
RowanDictionaryService >> setDefaultTemplate [

	defaultTemplate := self genericClassCreationTemplate.
]

{ #category : 'updates' }
RowanDictionaryService >> update [
  | dictionary sorted |
  classes := Array new.
  sorted := SortedCollection sortBlock: [ :x :y | x first < y first ].
  dictionary := Rowan image symbolList objectNamed: name.
  dictionary ifNil: [ ^ self ].
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
          | printString |
          printString := [ value printString ]
            on: Error
            do: [ :ex | 'unprintable string. Error - <' , ex printString , '>' ].
          sorted
            add:
              (Array
                with: name , '.' , key
                with: value class name
                with: value asOop
                with: printString) ] ].
  globals := sorted asArray.
  RowanCommandResult addResult: self
]
