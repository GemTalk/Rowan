"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressClassDefinition',
	#superclass : 'CypressDefinition',
	#instVars : [
		'category',
		'classInstVarNames',
		'classVarNames',
		'comment',
		'defaultSymbolDictionaryName',
		'instVarNames',
		'name',
		'poolDictionaryNames',
		'subclassType',
		'superclassName',
		'gs_options',
		'gs_constraints'
	],
	#category : 'Cypress-Definitions'
}

{ #category : 'instance creation' }
CypressClassDefinition class >> forClass: aClass [

	| superclassname |
	superclassname := aClass superclass
				ifNil: ['nil']
				ifNotNil: [:sClass | sClass name].
	^self
		name: aClass name
		superclassName: superclassname
		category: aClass category
		instVarNames: aClass instVarNames
		classInstVarNames: aClass class instVarNames
		classVarNames: aClass classVarNames
		poolDictionaryNames: aClass sharedPools
		comment: aClass rwComment
		subclassType: (self subclassTypeOf: aClass)

]

{ #category : 'private' }
CypressClassDefinition class >> subclassTypeOf: aClass [
	"Answer a description of the argument to identify whether it is a regular class,
	 a byte subclass, or an indexable subclass."

	^(aClass isBytes and: [aClass superClass isBytes not])
		ifTrue: ['byteSubclass']
		ifFalse: 
			[(aClass isIndexable and: [aClass superClass isIndexable not])
				ifTrue: ['indexableSubclass']
				ifFalse: ['']]

]

{ #category : 'comparing' }
CypressClassDefinition >> = aDefinition [
  ^ super = aDefinition
    and: [ 
      superclassName = aDefinition superclassName
        and: [ 
          category = aDefinition category
            and: [ 
              instVarNames = aDefinition instVarNames
                and: [ 
                  classInstVarNames = aDefinition classInstVarNames
                    and: [ 
                      classVarNames asSortedCollection = aDefinition classVarNames asSortedCollection
                        and: [ 
                          poolDictionaryNames = aDefinition poolDictionaryNames
                            and: [ comment = aDefinition comment ] ] ] ] ] ] ]

]

{ #category : 'loading' }
CypressClassDefinition >> actualClass [

	^self resolveGlobalNamed: self name

]

{ #category : 'loading' }
CypressClassDefinition >> actualClassOrNil [

	^self resolveGlobalNamed: self name or: [nil]

]

{ #category : 'converting' }
CypressClassDefinition >> asCypressClassDefinition [

	^self

]

{ #category : 'accessing' }
CypressClassDefinition >> category [

	^category

]

{ #category : 'visiting' }
CypressClassDefinition >> classDefinition: classBlock methodDefinition: methodBlock [

	classBlock value: self

]

{ #category : 'accessing' }
CypressClassDefinition >> classDefinitionCreationString [

	| stream symbolDict |
	stream := WriteStreamPortable on: (String new: 100).
	stream
		nextPutAll: '(CypressClassDefinition';
		lf;
		tab;
		nextPutAll: 'name: ' , self className printString;
		lf;
		tab;
		nextPutAll: 'superclassName: ' , self superclassName printString;
		lf;
		tab;
		nextPutAll: 'category: ' , self category printString;
		lf;
		tab;
		nextPutAll: 'instVarNames: #(' , self instanceVariablesString , ')';
		lf;
		tab;
		nextPutAll: 'classInstVarNames: #(' , self classInstanceVariablesString
					, ')';
		lf;
		tab;
		nextPutAll: 'classVarNames: #(' , self classVariablesString , ')';
		lf;
		tab;
		nextPutAll: 'poolDictionaryNames: #(' , self poolDictionariesString , ')';
		lf;
		tab;
		nextPutAll: 'comment: ' , self comment printString;
		lf;
		tab;
		nextPutAll: 'subclassType: ' , self subclassType printString , ')';
		lf;
		tab;
		tab;
		yourself.
	symbolDict := self symbolDictionaryForClassNamed: self className.
	self actualClassOrNil isNil
		ifTrue: 
			[stream
				nextPutAll: 'loadClassDefinition.';
				yourself]
		ifFalse: 
			[stream
				nextPutAll: 'loadClassDefinition: ' , symbolDict name asString printString
							, '.';
				yourself].
	^stream contents

]

{ #category : 'private' }
CypressClassDefinition >> classInstanceVariablesString [
  ^ self stringForVariables: self classInstVarNames

]

{ #category : 'accessing' }
CypressClassDefinition >> classInstVarNames [
  ^ classInstVarNames

]

{ #category : 'accessing' }
CypressClassDefinition >> className [

	^self name

]

{ #category : 'loading' }
CypressClassDefinition >> classNeedingMigration: aClass [
  "right now we will create classes without doing a migration ..."

  

]

{ #category : 'private' }
CypressClassDefinition >> classVariablesString [
  ^ self stringForVariables: self classVarNames asSortedCollection

]

{ #category : 'accessing' }
CypressClassDefinition >> classVarNames [
  ^ classVarNames sort

]

{ #category : 'accessing' }
CypressClassDefinition >> comment [

	^comment

]

{ #category : 'loading' }
CypressClassDefinition >> createOrReviseByteClass [
	"To be resolved:
		- the question of an 'environment' in which to create the class.
		- the question of which SymbolDictionary in which to create the class.
	 These are perhaps the same question."

	| superClass |
	superClass := self resolveGlobalNamed: self superclassName.
	^(superClass
		byteSubclass: self name
		classVars: (self classVarNames collect: [:each | each asSymbol])
		classInstVars: (self classInstVarNames collect: [:each | each asSymbol])
		poolDictionaries: self poolDictionaryList
		inDictionary: (self symbolDictionaryForClassNamed: self name)
		options: #())
			category: category;
			comment: self comment

]

{ #category : 'loading' }
CypressClassDefinition >> createOrReviseIndexableClass [
	"To be resolved:
		- the question of an 'environment' in which to create the class.
		- the question of which SymbolDictionary in which to create the class.
	 These are perhaps the same question."

	| superClass |
	superClass := self resolveGlobalNamed: self superclassName.
	^(superClass
		indexableSubclass: self name
		instVarNames: (self instVarNames collect: [:each | each asSymbol])
		classVars: (self classVarNames collect: [:each | each asSymbol])
		classInstVars: (self classInstVarNames collect: [:each | each asSymbol])
		poolDictionaries: self poolDictionaryList
		inDictionary: (self symbolDictionaryForClassNamed: self name)
		options: #())
			category: category;
			comment: self comment

]

{ #category : 'loading' }
CypressClassDefinition >> createOrReviseRegularClass [
	"To be resolved:
		- the question of an 'environment' in which to create the class.
		- the question of which SymbolDictionary in which to create the class.
	 These are perhaps the same question."

	| superClass |
	superClass := self resolveGlobalNamed: self superclassName.
	^(superClass
		subclass: self name
		instVarNames: (self instVarNames collect: [:each | each asSymbol])
		classVars: (self classVarNames collect: [:each | each asSymbol])
		classInstVars: (self classInstVarNames collect: [:each | each asSymbol])
		poolDictionaries: self poolDictionaryList
		inDictionary: (self symbolDictionaryForClassNamed: self name)
		options: #())
			category: category;
			comment: self comment

]

{ #category : 'accessing' }
CypressClassDefinition >> defaultSymbolDictionaryName [
  ^ defaultSymbolDictionaryName ifNil: [ super defaultSymbolDictionaryName ]

]

{ #category : 'accessing' }
CypressClassDefinition >> defaultSymbolDictionaryName: aSymbol [
  defaultSymbolDictionaryName := aSymbol

]

{ #category : 'accessing' }
CypressClassDefinition >> description [

	^ Array with: name

]

{ #category : 'loading' }
CypressClassDefinition >> failedCompiledMethods: someCompiledMethods [

	someCompiledMethods isEmpty ifTrue: [^self].
	self halt: 'not implemented yet'

]

{ #category : 'accessing' }
CypressClassDefinition >> gs_constraints [

	^gs_constraints ifNil: [ gs_constraints := #() ]

]

{ #category : 'accessing' }
CypressClassDefinition >> gs_constraints: aCollection [

	gs_constraints := aCollection

]

{ #category : 'accessing' }
CypressClassDefinition >> gs_options [

	^gs_options ifNil: [ gs_options := #() ]

]

{ #category : 'accessing' }
CypressClassDefinition >> gs_options: aCollection [

	gs_options := aCollection

]

{ #category : 'accessing' }
CypressClassDefinition >> gs_reservedOop [
	^ ''
]

{ #category : 'comparing' }
CypressClassDefinition >> hash [

	| hash |
	hash := name hash.
	hash := superclassName hash bitOr: hash.
	hash := (category ifNil: ['']) hash bitOr: hash.
	instVarNames , classInstVarNames, classVarNames, poolDictionaryNames
		do: [:vName | hash := vName hash bitOr: hash].
	^hash

]

{ #category : 'private' }
CypressClassDefinition >> instanceVariablesString [
    ^ self stringForVariables: self instVarNames

]

{ #category : 'accessing' }
CypressClassDefinition >> instVarNames [
  ^ instVarNames

]

{ #category : 'loading' }
CypressClassDefinition >> loadClassDefinition [
  "Create a new version of the defined class. If the class already exists,
	 copy the behaviors and state from the old version."

  ^ self loadClassDefinition: self defaultSymbolDictionaryName

]

{ #category : 'loading' }
CypressClassDefinition >> loadClassDefinition: aDefaultSymbolDictionaryName [
	"Create a new version of the defined class. If the class already exists,
	 copy the behaviors and state from the old version."

	| newClass oldClass |
        self defaultSymbolDictionaryName: aDefaultSymbolDictionaryName.
	oldClass := self actualClassOrNil.
	newClass := self createOrReviseClass.
	(oldClass isNil or: [newClass == oldClass]) ifTrue: [^self].
	self classNeedingMigration: newClass.
	self
		recompileWithSubclassesFrom: oldClass
		to: newClass
		symbolList: System myUserProfile symbolList.

]

{ #category : 'accessing' }
CypressClassDefinition >> name [

	^name

]

{ #category : 'initialization' }
CypressClassDefinition >> name: aClassName superclassName: aSuperclassName category: aCategory instVarNames: someInstanceVariableNames classInstVarNames: someClassInstanceVariableNames classVarNames: someClassVariableNames poolDictionaryNames: somePoolDictionaryNames comment: aComment subclassType: aSubclassType [

	name := aClassName.
	superclassName := aSuperclassName.
	category := aCategory.
	instVarNames := someInstanceVariableNames.
	classInstVarNames := someClassInstanceVariableNames.
	classVarNames := someClassVariableNames.
	poolDictionaryNames := somePoolDictionaryNames.
	comment := aComment.
	subclassType := aSubclassType asString

]

{ #category : 'initialization' }
CypressClassDefinition >> name: aClassName superclassName: aSuperclassName category: aCategory instVarNames: someInstanceVariableNames 
	classInstVarNames: someClassInstanceVariableNames classVarNames: someClassVariableNames 
	poolDictionaryNames: somePoolDictionaryNames gs_options: someGs_options gs_constraints: someGs_constraints
	comment: aComment subclassType: aSubclassType [

	name := aClassName.
	superclassName := aSuperclassName.
	category := aCategory.
	instVarNames := someInstanceVariableNames.
	classInstVarNames := someClassInstanceVariableNames.
	classVarNames := someClassVariableNames.
	poolDictionaryNames := somePoolDictionaryNames.
	gs_options := someGs_options.
	gs_constraints := someGs_constraints.
	comment := aComment.
	subclassType := aSubclassType asString

]

{ #category : 'loading' }
CypressClassDefinition >> poolDictionariesForNames: pdNames [
  | ar existingDict symList sharedPool |
  ar := Array new.
  symList := System myUserProfile symbolList.
  pdNames
    do: [ :poolName | 
      existingDict := symList objectNamed: poolName.
      existingDict
        ifNil: [ 
          | pool |
          pool := SymbolDictionary new.
          pool name: poolName asSymbol.
          ar add: pool ]
        ifNotNil: [ 
          (existingDict isKindOf: SymbolDictionary)
            ifTrue: [ ar add: existingDict ]
            ifFalse: [ 
              sharedPool ifNil: [ sharedPool := symList objectNamed: #'SharedPool' ].
              ((existingDict isKindOf: Class)
                and: [ existingDict isSubclassOf: sharedPool ])
                ifTrue: [ 
                  | cvars pName |
                  ar add: (cvars := existingDict _createClassVarsDict).
                  pName := poolName asSymbol.	"only change dictionary name if needed , to avoid SecurityError"
                  cvars name ~~ pName
                    ifTrue: [ cvars name: pName ] ] ] ] ].
  ^ ar

]

{ #category : 'private' }
CypressClassDefinition >> poolDictionariesString [
  ^ self stringForVariables: self poolDictionaryNames

]

{ #category : 'loading' }
CypressClassDefinition >> poolDictionaryList [

  ^ self poolDictionariesForNames: self poolDictionaryNames

]

{ #category : 'accessing' }
CypressClassDefinition >> poolDictionaryNames [

	^poolDictionaryNames

]

{ #category : 'printString' }
CypressClassDefinition >> printDetailsOn: aStream [

	aStream nextPutAll: self name

]

{ #category : 'dependency' }
CypressClassDefinition >> provisions [
	"Answer list of global names defined by this definition"

	^{ self name }

]

{ #category : 'loading' }
CypressClassDefinition >> recompileWithSubclassesFrom: oldClass to: newClass symbolList: aSymbolList [

	| olds news removedClassVariables removedSharedPools organizer subclasses newSubclass |
	olds := oldClass _classVars ifNil: [#()] ifNotNil: [:vars | vars keys].
	news := newClass _classVars ifNil: [#()] ifNotNil: [:vars | vars keys].
	removedClassVariables := self elementsIn: olds butNotIn: news.
	removedSharedPools := self elementsIn: oldClass sharedPools
				butNotIn: newClass sharedPools.
	self failedCompiledMethods: (newClass
				_copyMethodsAndVariablesFrom: oldClass
				except: 
					{$V.
					removedClassVariables.
					$P.
					removedSharedPools}
				dictionaries: aSymbolList).
	organizer := ClassOrganizer new.
	subclasses := organizer subclassesOf: oldClass.


	"Do this -after- #subclassesOf:, which has the side effect of replacing the new
	  class with the old class in the organizer"
	organizer addClass: newClass.

	"Iterate over all the first-level subclasses of the old class to create new subclasses"
	subclasses do: 
			[:oldSubclass |
			newSubclass := 
					[oldSubclass definition evaluateInContext: nil symbolList: aSymbolList]
							on: Error
							do: [:ex | ex return: nil].
			(newSubclass notNil and: [newSubclass ~~ oldSubclass])
				ifTrue: 
					[self
						classNeedingMigration: newSubclass;
						recompileWithSubclassesFrom: oldSubclass
							to: newSubclass
							symbolList: aSymbolList]]

]

{ #category : 'dependency' }
CypressClassDefinition >> requirements [
	"Answer list of global names required by this definition"

  self superclassName = 'nil'
    ifTrue: [ ^ #() ].
  ^{self superclassName}

]

{ #category : 'accessing' }
CypressClassDefinition >> subclassType [

	^subclassType

]

{ #category : 'accessing' }
CypressClassDefinition >> superclassName [

	^superclassName

]

{ #category : 'loading' }
CypressClassDefinition >> unloadDefinition [
	"GemStone could hold multiple definitions of the same class name.
	 Ignore aliased references.
	 Unload only the first one resolved.
	 It is an error if there is not at least one SymbolDictionary holding a
	 class with that name."

	| dictionarySymbolPair |
	dictionarySymbolPair := ((System myUserProfile symbolList
				dictionariesAndSymbolsOf: self actualClass)
					select: [:each | each last = self name asSymbol]) first.
	dictionarySymbolPair first removeKey: dictionarySymbolPair last

]
