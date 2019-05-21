"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressTopazFileoutWriter',
	#superclass : 'CypressAbstractFileoutWriter',
	#category : 'Cypress-GemStoneFileServer'
}

{ #category : 'writing - private' }
CypressTopazFileoutWriter >> fileOut: aString methodsPreambleFor: classStructure on: aStream [

	aStream
		nextPutAll: '! ------------------- ', aString, ' methods for ', classStructure name; lf;
		lf

]

{ #category : 'writing - private' }
CypressTopazFileoutWriter >> fileOutClassDeclaration: classStructure on: aStream [
  aStream
    nextPutAll: 'doit';
    lf;
    nextPutAll: '(' , classStructure superclassName;
    lf.
  self
    writeClassTypeMessage: classStructure
    on: aStream
    hasInstanceVariables: [ aStream
        nextPutAll:
            '	instVarNames: #( ' , classStructure instanceVariablesString , ' )';
        lf ].
  aStream
    nextPutAll: '	classVars: #( ' , classStructure classVariablesString , ' )';
    lf;
    nextPutAll:
        '	classInstVars: #( ' , classStructure classInstanceVariablesString , ' )';
    lf;
    nextPutAll: '	poolDictionaries: #()';
    lf;
    nextPutAll: '	inDictionary: UserGlobals';
    lf;
    nextPutAll: '	options: #())';
    lf;
    nextPutAll: '		category: ' , classStructure category printString , ';';
    lf;
    nextPutAll: '		comment: ' , classStructure comment printString , ';';
    lf;
    nextPutAll: '		immediateInvariant.';
    lf;
    nextPutAll: 'true.';
    lf;
    nextPutAll: '%';
    lf;
    lf

]

{ #category : 'writing - private' }
CypressTopazFileoutWriter >> fileOutClassesPreambleOn: aStream [

	aStream
		nextPutAll: '! Class Declarations'; lf;
		lf

]

{ #category : 'writing - private' }
CypressTopazFileoutWriter >> fileOutClassInitializerFor: classStructure on: aStream [

	aStream
		nextPutAll: classStructure className, ' initialize.'; lf

]

{ #category : 'writing - private' }
CypressTopazFileoutWriter >> fileOutClassInitializersPostambleOn: aStream [

	aStream
                nextPutAll: 'true.'; lf;
		nextPutAll: '%'; lf;
		lf

]

{ #category : 'writing - private' }
CypressTopazFileoutWriter >> fileOutClassInitializersPreambleOn: aStream [

	aStream
		nextPutAll: '! Class initializers '; lf;
		lf;
		nextPutAll: 'doit'; lf

]

{ #category : 'writing - private' }
CypressTopazFileoutWriter >> fileOutExtensionsPreambleOn: aStream [

	aStream
		nextPutAll: '! Class Extensions'; lf;
		lf

]

{ #category : 'writing - private' }
CypressTopazFileoutWriter >> fileOutMethod: methodStructure on: aStream [

	aStream
		nextPutAll: 'category: ', methodStructure category printString; lf;
		nextPutAll: (methodStructure isMetaclass ifTrue: ['classmethod: '] ifFalse: ['method: ']), methodStructure classStructure className; lf;
		nextPutAll: methodStructure source.
	methodStructure source last = Character lf
		ifFalse: [aStream lf].
	aStream nextPutAll: '%'; lf;
		lf

]

{ #category : 'writing - private' }
CypressTopazFileoutWriter >> fileOutPackagePostambleOn: aStream [

	aStream
		lf;
		lf;
		nextPutAll: '! End of Package: ', self packageName; lf;
		lf;
		lf

]

{ #category : 'writing - private' }
CypressTopazFileoutWriter >> fileOutPackagePreambleOn: aStream [

	aStream
		nextPutAll: '! Package: ', self packageName; lf;
		lf;
		lf;
		nextPutAll: '! Remove existing behavior from package ', self packageName; lf;
		nextPutAll: '!!!! This can be cleaned up when some package functionality is moved to the base system.'; lf;
		lf;
		nextPutAll: 'doit'; lf;
		nextPutAll: '| packageName |'; lf;
		nextPutAll: 'packageName := ', self packageName printString, '.'; lf;
		nextPutAll: 'System myUserProfile symbolList do: [:symDict |'; lf;
		nextPutAll: '	symDict do: [:possibleClass |'; lf;
		nextPutAll: '			| toRemove |'; lf;
		nextPutAll: '		possibleClass isBehavior ifTrue: ['; lf;
		nextPutAll: '			{possibleClass. possibleClass class} do: [:aClass |'; lf;
		nextPutAll: '				aClass category = packageName'; lf;
		nextPutAll: '					ifTrue: ['; lf;
		nextPutAll: '							"*anythingbutpackagename[-anything]"'; lf;
		nextPutAll: '						toRemove := aClass categoryNames select: '; lf;
		nextPutAll: '										[:each |'; lf;
		nextPutAll: '										each isEmpty not and: ['; lf;
		nextPutAll: '											(each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])'; lf;
		nextPutAll: '														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]])'; lf;
		nextPutAll: '											or: [each first ~= $*]]]'; lf;
		nextPutAll: '					]'; lf;
		nextPutAll: '					ifFalse: ['; lf;
		nextPutAll: '							"*packagename[-anything]"'; lf;
		nextPutAll: '						toRemove := aClass categoryNames select: '; lf;
		nextPutAll: '										[:each |'; lf;
		nextPutAll: '										each isEmpty not and: ['; lf;
		nextPutAll: '											each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])'; lf;
		nextPutAll: '														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]]]]'; lf;
		nextPutAll: '					].'; lf;
		nextPutAll: '				toRemove do: [:each | aClass removeCategory: each].'; lf;
		nextPutAll: '			]'; lf;
		nextPutAll: '		]'; lf;
		nextPutAll: '	]'; lf;
		nextPutAll: '].'; lf;
                nextPutAll: 'true.'; lf;
		nextPutAll: '%'; lf;
		lf;
		lf

]

{ #category : 'writing - private' }
CypressTopazFileoutWriter >> fileOutPreambleType: aString for: classStructure on: aStream [

	aStream
		nextPutAll: '! ', aString, ' for ', classStructure name; lf;
		lf

]

{ #category : 'accessing' }
CypressTopazFileoutWriter >> packageNameExtension [

	^'.gs'

]

{ #category : 'writing - private' }
CypressTopazFileoutWriter >> writeClassTypeMessage: classStructure on: aStream hasInstanceVariables: instanceVariableBlock [
  | classType classTypeMessage hasInstanceVariables |
  hasInstanceVariables := true.
  classType := classStructure subclassType.
  classType = 'indexableSubclass'
    ifTrue: [ classTypeMessage := 'indexableSubclass: ' ]
    ifFalse: [ classType = 'byteSubclass'
        ifTrue: [ classTypeMessage := 'byteSubclass: '.
          hasInstanceVariables := false ]
        ifFalse: [ classType = ''
            ifTrue: [ classTypeMessage := 'subclass: ' ]
            ifFalse: [ self error: 'unknown subclass type: ' , classType ] ] ].
  aStream
    tab;
    nextPutAll: classTypeMessage , classStructure className asString printString;
    lf.
  hasInstanceVariables
    ifTrue: [ instanceVariableBlock value ]

]
