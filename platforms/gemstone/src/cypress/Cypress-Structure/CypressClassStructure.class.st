"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressClassStructure',
	#superclass : 'CypressStructure',
	#instVars : [
		'instanceMethods',
		'classMethods',
		'comment',
		'isClassExtension'
	],
	#category : 'Cypress-Structure'
}

{ #category : 'instance creation' }
CypressClassStructure class >> fromClassDefinition: classDefinition [

	^self new
		fromClassDefinition: classDefinition;
		yourself

]

{ #category : 'converting' }
CypressClassStructure >> asCypressClassDefinition [

	self isClassExtension ifTrue: [^CypressError signal: 'Extensions cannot have class definitions'].
	^CypressClassDefinition
		name: self className
		superclassName: self superclassName
		category: self category
		instVarNames: self instanceVariableNames
		classInstVarNames: self classInstanceVariableNames
		classVarNames: self classVariableNames
		poolDictionaryNames: self poolDictionaryNames
		comment: self comment
		subclassType: self subclassType

]

{ #category : 'accessing' }
CypressClassStructure >> category [

	^self properties
		at: 'category'
		ifAbsent: [self packageStructure packageName]

]

{ #category : 'accessing' }
CypressClassStructure >> category: aString [

	^self properties at: 'category' put: aString

]

{ #category : 'accessing' }
CypressClassStructure >> classInstanceVariableNames [

	^self properties at: 'classinstvars' ifAbsent: [#()]

]

{ #category : 'accessing' }
CypressClassStructure >> classInstanceVariableNames: someStrings [

	^self properties at: 'classinstvars' put: someStrings

]

{ #category : 'converting' }
CypressClassStructure >> classInstanceVariablesString [
  ^ self stringForVariables: self classInstanceVariableNames

]

{ #category : 'querying' }
CypressClassStructure >> classMethodNamed: methodName [

	^self classMethods
		at: methodName
		ifAbsentPut: [CypressMethodStructure named: methodName]

]

{ #category : 'accessing' }
CypressClassStructure >> classMethods [

	classMethods ifNil: [ classMethods := Dictionary new ].
	^classMethods

]

{ #category : 'accessing' }
CypressClassStructure >> className [

	^self name

]

{ #category : 'accessing' }
CypressClassStructure >> classVariableNames [

	^self properties at: 'classvars' ifAbsent: [#()]

]

{ #category : 'accessing' }
CypressClassStructure >> classVariableNames: someStrings [

	^self properties at: 'classvars' put: someStrings

]

{ #category : 'converting' }
CypressClassStructure >> classVariablesString [
  ^ self stringForVariables: self classVariableNames asSortedCollection

]

{ #category : 'accessing' }
CypressClassStructure >> comment [

	comment ifNil: [ comment := '' ].
	^comment

]

{ #category : 'accessing' }
CypressClassStructure >> comment: aString [

	comment := aString

]

{ #category : 'initialization' }
CypressClassStructure >> fromClassDefinition: classDefinition [

	self
		isClassExtension: false;
		name: classDefinition name;
		category: classDefinition category;
		comment: classDefinition comment;
		superclassName: classDefinition superclassName;
		instanceVariableNames: classDefinition instVarNames;
		classInstanceVariableNames: classDefinition classInstVarNames;
		classVariableNames: classDefinition classVarNames;
		poolDictionaryNames: classDefinition poolDictionaryNames;
		subclassType: classDefinition subclassType.

]

{ #category : 'querying' }
CypressClassStructure >> instanceMethodNamed: methodName [

	^self instanceMethods
		at: methodName 
		ifAbsentPut: [CypressMethodStructure named: methodName]

]

{ #category : 'accessing' }
CypressClassStructure >> instanceMethods [

	instanceMethods ifNil: [ instanceMethods := Dictionary new ].
	^instanceMethods

]

{ #category : 'accessing' }
CypressClassStructure >> instanceVariableNames [

	^self properties at: 'instvars' ifAbsent: [#()]

]

{ #category : 'accessing' }
CypressClassStructure >> instanceVariableNames: someStrings [

	^self properties at: 'instvars' put: someStrings

]

{ #category : 'converting' }
CypressClassStructure >> instanceVariablesString [

	^self stringForVariables: self instanceVariableNames

]

{ #category : 'accessing' }
CypressClassStructure >> isClassExtension [

        isClassExtension ifNil: [ isClassExtension := true ].
        ^isClassExtension

]

{ #category : 'accessing' }
CypressClassStructure >> isClassExtension: aBoolean [

	isClassExtension := aBoolean

]

{ #category : 'accessing' }
CypressClassStructure >> name [

	^self properties at: 'name'

]

{ #category : 'accessing' }
CypressClassStructure >> name: aString [

	self properties at: 'name' put: aString

]

{ #category : 'converting' }
CypressClassStructure >> poolDictionariesString [
  ^ self stringForVariables: self poolDictionaryNames

]

{ #category : 'accessing' }
CypressClassStructure >> poolDictionaryNames [

	^self properties at: 'pools' ifAbsent: [#()]

]

{ #category : 'accessing' }
CypressClassStructure >> poolDictionaryNames: someStrings [

	^self properties at: 'pools' put: someStrings

]

{ #category : 'accessing' }
CypressClassStructure >> subclassType [

	^self properties at: '_gs_subclassType' ifAbsent: ['']

]

{ #category : 'accessing' }
CypressClassStructure >> subclassType: aString [

	aString isEmpty
		ifTrue: [self properties removeKey: '_gs_subclassType' ifAbsent: []]
		ifFalse: [self properties at: '_gs_subclassType' put: aString]

]

{ #category : 'accessing' }
CypressClassStructure >> superclassName [

	^self properties at: 'super'

]

{ #category : 'accessing' }
CypressClassStructure >> superclassName: aString [

	^self properties at: 'super' put: aString

]
