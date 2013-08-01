! Package: Cypress-Structure


! Remove existing behavior from package Cypress-Structure
!!!! This can be cleaned up when some package functionality is moved to the base system.

doit
| packageName |
packageName := 'Cypress-Structure'.
System myUserProfile symbolList do: [:symDict |
	symDict do: [:possibleClass |
			| toRemove |
		possibleClass isBehavior ifTrue: [
			{possibleClass. possibleClass class} do: [:aClass |
				aClass category = packageName
					ifTrue: [
							"*anythingbutpackagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										(each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]])
										or: [each first ~= $*]]
					]
					ifFalse: [
							"*packagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]]]
					].
				toRemove do: [:each | aClass removeCategory: each].
			]
		]
	]
].
%


! Class Declarations

doit
(Error
	subclass: 'CypressJsonError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Structure';
		comment: '';
		immediateInvariant.
%

doit
(CypressObject
	subclass: 'CypressStructure'
	instVarNames: #( name properties packageStructure )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Structure';
		comment: '';
		immediateInvariant.
%

doit
(CypressStructure
	subclass: 'CypressPackageStructure'
	instVarNames: #( classes extensions )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Structure';
		comment: '';
		immediateInvariant.
%

doit
(CypressStructure
	subclass: 'CypressMethodStructure'
	instVarNames: #( source isMetaclass classStructure )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Structure';
		comment: '';
		immediateInvariant.
%

doit
(CypressStructure
	subclass: 'CypressClassStructure'
	instVarNames: #( instanceMethods classMethods comment isClassExtension )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Structure';
		comment: '';
		immediateInvariant.
%

doit
(Object
	subclass: 'CypressJsonParser'
	instVarNames: #( stream )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Structure';
		comment: '';
		immediateInvariant.
%

! Class Implementation for CypressJsonError

! Class Implementation for CypressStructure

! ------------------- Class methods for CypressStructure

category: 'instance creation'
set compile_env: 0
classmethod: CypressStructure
fromJs: jsObject

	^(self new) 
		fromJs: jsObject asCypressPropertyObject;
		yourself
%

category: 'instance creation'
set compile_env: 0
classmethod: CypressStructure
named: aString

	^(self new)
		name: aString;
		yourself
%

! ------------------- Instance methods for CypressStructure

category: 'initialization'
set compile_env: 0
method: CypressStructure
fromJs: jsObject

	self subclassResponsibility
%

category: 'accessing'
set compile_env: 0
method: CypressStructure
name

	^name
%

category: 'accessing'
set compile_env: 0
method: CypressStructure
name: aString 

	name := aString
%

category: 'accessing'
set compile_env: 0
method: CypressStructure
packageStructure
	^packageStructure
%

category: 'accessing'
set compile_env: 0
method: CypressStructure
packageStructure: aCypressPackageStructure
	packageStructure := aCypressPackageStructure
%

category: 'writing'
set compile_env: 0
method: CypressStructure
path: aFSPath file: aFilename write: writeBlock

	| fs stream |
	fs := aFSPath fs.
	stream := fs createWriteStream: (aFSPath resolve: aFilename).
	writeBlock value: stream.
	stream end.
%

category: 'printing'
set compile_env: 0
method: CypressStructure
printDetailsOn: aStream

	aStream nextPutAll: self name.
%

category: 'accessing'
set compile_env: 0
method: CypressStructure
properties

	properties ifNil: [ properties := Dictionary new ].
	^properties
%

category: 'accessing'
set compile_env: 0
method: CypressStructure
properties: aDictionary

	properties := aDictionary
%

category: 'writing'
set compile_env: 0
method: CypressStructure
writeJsonOn: aStream

	self writeJsonOn: aStream indent: 0.
%

category: 'writing'
set compile_env: 0
method: CypressStructure
writeJsonOn: aStream  indent: indent

	self subclassResponsibility
%

! Class Implementation for CypressPackageStructure

! ------------------- Class methods for CypressPackageStructure

category: 'instance creation'
set compile_env: 0
classmethod: CypressPackageStructure
fileOutsForPackagesNamed: someNames

	^someNames inject: Dictionary new
		into: 
			[:result :each |
			result
				at: each
					put: (String streamContents: 
								[:stream |
								(self fromPackage: (CypressPackageDefinition named: each))
									fileOutOn: stream]);
				yourself]
%

category: 'instance creation'
set compile_env: 0
classmethod: CypressPackageStructure
fromJson: aJsonString

	^self fromJs: (CypressJsonParser parse: aJsonString)
%

category: 'instance creation'
set compile_env: 0
classmethod: CypressPackageStructure
fromPackage: aCypressPackageDefinition

	^(self new) 
		fromPackage: aCypressPackageDefinition;
		yourself
%

! ------------------- Instance methods for CypressPackageStructure

category: 'conversion'
set compile_env: 0
method: CypressPackageStructure
asCypressJson

	| stream |
    	stream := WriteStream on: String new.
    	self writeJsonOn: stream.
	^stream contents
%

category: 'accessing'
set compile_env: 0
method: CypressPackageStructure
classes

	classes ifNil: [ classes := OrderedCollection new ].
	^classes
%

category: 'accessing'
set compile_env: 0
method: CypressPackageStructure
extensions

	extensions ifNil: [ extensions := OrderedCollection new ].
	^extensions
%

category: 'filing out - private'
set compile_env: 0
method: CypressPackageStructure
fileOut: aString implementationsFrom: someClassStructures on: aStream

	someClassStructures
		do: [:each | self fileOutType: aString implementationOf: each on: aStream]
%

category: 'filing out - private'
set compile_env: 0
method: CypressPackageStructure
fileOut: aString methods: someMethodStructures on: aStream

	someMethodStructures isEmpty ifTrue: [^self].
	self
		fileOut: aString methodsPreambleFor: someMethodStructures any classStructure on: aStream;
		fileOutMethods: someMethodStructures on: aStream
%

category: 'filing out - private'
set compile_env: 0
method: CypressPackageStructure
fileOut: aString methodsPreambleFor: classStructure on: aStream

	aStream
		nextPutAll: '! ------------------- ', aString, ' methods for ', classStructure name; lf;
		lf
%

category: 'filing out - private'
set compile_env: 0
method: CypressPackageStructure
fileOutClassDeclaration: classStructure on: aStream

	aStream
		nextPutAll: 'doit'; lf;
		nextPutAll: '(', classStructure superclassName; lf;
		nextPutAll: '	subclass: ', classStructure className asString printString; lf;
		nextPutAll: '	instVarNames: #( ', classStructure instanceVariablesString, ' )'; lf;
		nextPutAll: '	classVars: #( ', classStructure classVariablesString, ' )'; lf;
		nextPutAll: '	classInstVars: #( ', classStructure classInstanceVariablesString, ' )'; lf;
		nextPutAll: '	poolDictionaries: #()'; lf;
		nextPutAll: '	inDictionary: UserGlobals'; lf;
		nextPutAll: '	options: #())'; lf;
		nextPutAll: '		category: ', classStructure category printString, ';'; lf;
		nextPutAll: '		comment: ', classStructure comment printString, ';'; lf;
		nextPutAll: '		immediateInvariant.'; lf;
		nextPutAll: '%'; lf;
		lf.
%

category: 'filing out - private'
set compile_env: 0
method: CypressPackageStructure
fileOutClassDeclarationsOn: aStream

	self classes
		do: [:classStructure | self fileOutClassDeclaration: classStructure on: aStream]
%

category: 'filing out - private'
set compile_env: 0
method: CypressPackageStructure
fileOutClassesOn: aStream

	self
		fileOutClassesPreambleOn: aStream;
		fileOutClassDeclarationsOn: aStream;
		fileOutClassImplementationsOn: aStream
%

category: 'filing out - private'
set compile_env: 0
method: CypressPackageStructure
fileOutClassesPreambleOn: aStream

	aStream
		nextPutAll: '! Class Declarations'; lf;
		lf
%

category: 'filing out - private'
set compile_env: 0
method: CypressPackageStructure
fileOutClassImplementationsOn: aStream

	self
		fileOut: 'Class Implementation'
		implementationsFrom: self classes
		on: aStream
%

category: 'filing out - private'
set compile_env: 0
method: CypressPackageStructure
fileOutExtensionImplementationsOn: aStream

	self
		fileOut: 'Class Extension'
		implementationsFrom: self extensions
		on: aStream
%

category: 'filing out - private'
set compile_env: 0
method: CypressPackageStructure
fileOutExtensionsOn: aStream

	self
		fileOutExtensionsPreambleOn: aStream;
		fileOutExtensionImplementationsOn: aStream
%

category: 'filing out - private'
set compile_env: 0
method: CypressPackageStructure
fileOutExtensionsPreambleOn: aStream

	aStream
		nextPutAll: '! Class Extensions'; lf;
		lf
%

category: 'filing out - private'
set compile_env: 0
method: CypressPackageStructure
fileOutMethod: methodStructure on: aStream

	aStream
		nextPutAll: 'category: ', methodStructure category printString; lf;
		nextPutAll: 'set compile_env: 0'; lf;
		nextPutAll: (methodStructure isMetaclass ifTrue: ['classmethod: '] ifFalse: ['method: ']), methodStructure classStructure className; lf;
		nextPutAll: methodStructure source.
	methodStructure source last = Character lf
		ifFalse: [aStream lf].
	aStream nextPutAll: '%'; lf;
		lf
%

category: 'filing out - private'
set compile_env: 0
method: CypressPackageStructure
fileOutMethods: someMethodStructures on: aStream

	(someMethodStructures
		asSortedCollection: [:a :b | a selector <= b selector])
			do: [:methodStructure | self fileOutMethod: methodStructure on: aStream]
%

category: 'filing out'
set compile_env: 0
method: CypressPackageStructure
fileOutOn: aStream

	self
		fileOutPackagePreambleOn: aStream;
		fileOutClassesOn: aStream;
		fileOutExtensionsOn: aStream
%

category: 'filing out - private'
set compile_env: 0
method: CypressPackageStructure
fileOutPackagePreambleOn: aStream

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
		nextPutAll: '										(each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])'; lf;
		nextPutAll: '														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]])'; lf;
		nextPutAll: '										or: [each first ~= $*]]'; lf;
		nextPutAll: '					]'; lf;
		nextPutAll: '					ifFalse: ['; lf;
		nextPutAll: '							"*packagename[-anything]"'; lf;
		nextPutAll: '						toRemove := aClass categoryNames select: '; lf;
		nextPutAll: '										[:each |'; lf;
		nextPutAll: '										each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])'; lf;
		nextPutAll: '														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]]]'; lf;
		nextPutAll: '					].'; lf;
		nextPutAll: '				toRemove do: [:each | aClass removeCategory: each].'; lf;
		nextPutAll: '			]'; lf;
		nextPutAll: '		]'; lf;
		nextPutAll: '	]'; lf;
		nextPutAll: '].'; lf;
		nextPutAll: '%'; lf;
		lf;
		lf
%

category: 'filing out - private'
set compile_env: 0
method: CypressPackageStructure
fileOutPreambleType: aString for: classStructure on: aStream

	aStream
		nextPutAll: '! ', aString, ' for ', classStructure name; lf;
		lf
%

category: 'filing out - private'
set compile_env: 0
method: CypressPackageStructure
fileOutType: aString implementationOf: classStructure on: aStream

	self
		fileOutPreambleType: aString
			for: classStructure
			on: aStream;
		fileOut: 'Class'
			methods: classStructure classMethods
			on: aStream;
		fileOut: 'Instance'
			methods: classStructure instanceMethods
			on: aStream
%

category: 'initialization'
set compile_env: 0
method: CypressPackageStructure
fromJs: jsObject

	name := jsObject at: 'name'.
	(jsObject at: 'contents') do: [:jsClassObject| | classStructure objectName |
		classStructure := (CypressClassStructure new)
                		packageStructure: self;
				yourself.
                ((objectName := jsClassObject at: 'name') endsWith: '.extension')
			ifTrue: [ 
				classStructure isClassExtension: true.
				self extensions add: classStructure ]
			ifFalse: [
				((objectName := jsClassObject at: 'name') endsWith: '.class')
					ifTrue: [ 
						classStructure isClassExtension: false.
						self classes add: classStructure ]].
		classStructure fromJs: jsClassObject].
	properties := jsObject at: 'properties.json'
%

category: 'initialization'
set compile_env: 0
method: CypressPackageStructure
fromPackage: aCypressPackageDefinition

	| snapshot classMap classDefinitions classStructure |
	snapshot := aCypressPackageDefinition snapshot.
	name := aCypressPackageDefinition name, '.package'.
	properties := Dictionary new.
	classDefinitions := OrderedCollection new.
	classMap := Dictionary new.
	snapshot definitions do: [:definition |  
			definition 
				classDefinition: [:classDefinition |  classDefinitions add: classDefinition ] 
				methodDefinition: [:methodDefinition | 
					(classMap 
						at: methodDefinition className 
						ifAbsentPut: [Set new]) 
							add: methodDefinition. ]].
	classDefinitions do: [:classDefinition |
		classStructure := (CypressClassStructure fromClassDefinition: classDefinition)
			packageStructure: self.
		(classMap removeKey: classDefinition className ifAbsent: [#()]) do: [:methodDefinition | | methodStructure |
			methodStructure := (CypressMethodStructure fromMethodDefinition: methodDefinition)
				packageStructure: self;
				classStructure: classStructure.
			(methodDefinition
				instanceMethod: [:instanceMethod | classStructure instanceMethods ] 
				classMethod: [:classMethod | classStructure classMethods ])
					at: methodDefinition selector
					put: methodStructure ].
		self classes add: classStructure ].
	classMap keysAndValuesDo: [:className :methods |
		classStructure := (CypressClassStructure named: className)
			isClassExtension: true;
			packageStructure: self.
		methods do: [:methodDefinition | | methodStructure |
			methodStructure := (CypressMethodStructure fromMethodDefinition: methodDefinition)
				packageStructure: self;
				classStructure: classStructure.
			(methodDefinition
				instanceMethod: [:instanceMethod | classStructure instanceMethods ] 
				classMethod: [:classMethod | classStructure classMethods ])
					at: methodDefinition selector
					put: methodStructure ].
		self extensions add: classStructure ].
%

category: 'accessing'
set compile_env: 0
method: CypressPackageStructure
packageExtension

	^self properties at: 'extension' ifAbsent: ['.package' ]
%

category: 'accessing'
set compile_env: 0
method: CypressPackageStructure
packageName

	^self name copyWithoutSuffix: self packageExtension
%

category: 'accessing'
set compile_env: 0
method: CypressPackageStructure
packageStructure
	^self
%

category: 'snapshotting'
set compile_env: 0
method: CypressPackageStructure
snapshot
	| definitions |
	definitions := OrderedCollection new.
	self classes do: [:classStructure |
        	definitions add: classStructure asCypressClassDefinition.
                (classStructure instanceMethods asSortedCollection: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ].
                (classStructure classMethods asSortedCollection: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ]].
	self extensions do: [:classStructure |
                (classStructure instanceMethods asSortedCollection: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ].
                (classStructure classMethods asSortedCollection: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ]].
	^ CypressSnapshot definitions: definitions
%

category: 'writing'
set compile_env: 0
method: CypressPackageStructure
writeJsonOn: aStream  indent: startIndent

	| indent |
	indent := startIndent.
	aStream 
		tab: indent;
		nextPutAll: '{';
		lf.
	indent := indent + 1.
	aStream
		tab: indent;
		nextPutAll: '"name"';
		nextPutAll: ' : ';
		nextPutAll: '"', self name, '",'.
	aStream
		lf;
		tab: indent;
		nextPutAll: '"contents" : [';
		lf;
		yourself.
	1 to: self classes size do: [:index | | classStructure | 
		classStructure := self classes at: index.
		classStructure writeJsonOn: aStream indent: indent + 1.
		(self extensions size > 0 or: [ index < self classes size]) ifTrue: [ aStream nextPutAll: ','; lf. ]].
	1 to: self extensions size do: [:index | | classStructure | 
		classStructure := self extensions at: index.
		classStructure writeJsonOn: aStream indent: indent + 1.
		index < self extensions size ifTrue: [ aStream nextPutAll: ','; lf.] ].
	aStream
		lf;
		tab: indent;
		nextPutAll: '],';
		lf;
		tab: indent;
		nextPutAll: '"properties.json" : '.
	self properties writeCypressJsonOn: aStream indent: indent.
	indent := indent - 1.
	aStream 
		lf;
		tab: indent;
		nextPutAll: '}'
%

! Class Implementation for CypressMethodStructure

! ------------------- Class methods for CypressMethodStructure

category: 'instance creation'
set compile_env: 0
classmethod: CypressMethodStructure
fromMethodDefinition: methodDefinition

	^self new
		fromMethodDefinition: methodDefinition;
		yourself
%

! ------------------- Instance methods for CypressMethodStructure

category: 'converting'
set compile_env: 0
method: CypressMethodStructure
asCypressMethodDefinition

	^CypressMethodDefinition 
        	className: self classStructure className
		classIsMeta: self isMetaclass
		selector: self selector
		category: self category
		source: self source
%

category: 'accessing'
set compile_env: 0
method: CypressMethodStructure
category

	^self properties at: 'category'
%

category: 'accessing'
set compile_env: 0
method: CypressMethodStructure
category: aString

	self properties at: 'category' put: aString
%

category: 'accessing'
set compile_env: 0
method: CypressMethodStructure
classStructure
	^classStructure
%

category: 'accessing'
set compile_env: 0
method: CypressMethodStructure
classStructure: aCypressClassStructure
	classStructure := aCypressClassStructure
%

category: 'accessing'
set compile_env: 0
method: CypressMethodStructure
cypressSource

	| stream |
	stream := WriteStream on: String new.
	stream 
		nextPutAll: self category;
		lf;
		nextPutAll: self source.
	^stream contents
%

category: 'private'
set compile_env: 0
method: CypressMethodStructure
extractCypressSource: aString
    | stream categoryStream sourceStream readingCategory |
    stream := ReadStream on: aString.
    categoryStream := WriteStream on: String new.
    sourceStream := WriteStream on: String new.
    readingCategory := true.
    [ stream atEnd ]
        whileFalse: [ 
            | char |
            char := stream next.
            readingCategory
                ifTrue: [ 
                    char = Character lf
                        ifTrue: [ readingCategory := false ]
                        ifFalse: [ categoryStream nextPut: char ] ]
                ifFalse: [ sourceStream nextPut: char ] ].
    self category: categoryStream contents.
    self source: sourceStream contents
%

category: 'initialization'
set compile_env: 0
method: CypressMethodStructure
fromJs: jsObject named: methodNameParts
    | ext |
    (ext := methodNameParts at: 2) = '.st'
        ifTrue: [ self extractCypressSource: (jsObject at: 'contents') ]
        ifFalse: [ 
            ext = '.json'
                ifTrue: [ properties := jsObject at: 'contents' ] ]
%

category: 'initialization'
set compile_env: 0
method: CypressMethodStructure
fromMethodDefinition: methodDefinition

	self isMetaclass: methodDefinition classIsMeta.
	self selector: methodDefinition selector.
	self category: methodDefinition category.
	self source: methodDefinition source.
%

category: 'accessing'
set compile_env: 0
method: CypressMethodStructure
isMetaclass

	isMetaclass ifNil: [ isMetaclass := false ].
	^isMetaclass
%

category: 'accessing'
set compile_env: 0
method: CypressMethodStructure
isMetaclass: aBoolean
	isMetaclass := aBoolean
%

category: 'accessing'
set compile_env: 0
method: CypressMethodStructure
selector
    ^ String
        streamContents: [ :stream | 
            self name
                do: [ :chara | 
                    stream
                        nextPut:
                            (chara = $.
                                ifTrue: [ $: ]
                                ifFalse: [ chara ]) ] ]
%

category: 'accessing'
set compile_env: 0
method: CypressMethodStructure
selector: aString
    name := String
        streamContents: [ :stream | 
            aString
                do: [ :chara | 
                    stream
                        nextPut:
                            (chara = $:
                                ifTrue: [ $. ]
                                ifFalse: [ chara ]) ] ]
%

category: 'accessing'
set compile_env: 0
method: CypressMethodStructure
source

	^source
%

category: 'accessing'
set compile_env: 0
method: CypressMethodStructure
source: aString

	source := aString
%

category: 'writing'
set compile_env: 0
method: CypressMethodStructure
writeJsonOn: aStream  indent: startIndent

	| indent |
	indent := startIndent.
	aStream 
		tab: indent;
		nextPutAll: '{';
		lf.
	indent := indent + 1.
	aStream
		tab: indent;
		nextPutAll: '"name"';
		nextPutAll: ' : ';
		nextPutAll: '"', self name, '.st",';
		lf.
	aStream
		tab: indent;
		nextPutAll: '"contents"';
		nextPutAll: ' : '.
	self cypressSource writeCypressJsonOn: aStream indent: indent.
	indent := indent - 1.
	aStream
		lf;
		tab: indent;
		nextPutAll: ' }'
%

! Class Implementation for CypressClassStructure

! ------------------- Class methods for CypressClassStructure

category: 'instance creation'
set compile_env: 0
classmethod: CypressClassStructure
fromClassDefinition: classDefinition

	^self new
		fromClassDefinition: classDefinition;
		yourself
%

! ------------------- Instance methods for CypressClassStructure

category: 'converting'
set compile_env: 0
method: CypressClassStructure
asCypressClassDefinition

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
%

category: 'accessing'
set compile_env: 0
method: CypressClassStructure
category

	^self packageStructure packageName
%

category: 'accessing'
set compile_env: 0
method: CypressClassStructure
classInstanceVariableNames

	^self properties at: 'classinstvars' ifAbsent: [#()]
%

category: 'accessing'
set compile_env: 0
method: CypressClassStructure
classInstanceVariableNames: someStrings

	^self properties at: 'classinstvars' put: someStrings
%

category: 'converting'
set compile_env: 0
method: CypressClassStructure
classInstanceVariablesString

	^self stringForVariables: self classInstanceVariableNames
%

category: 'querying'
set compile_env: 0
method: CypressClassStructure
classMethodNamed: methodName

	^self classMethods
		at: methodName
		ifAbsentPut: [CypressMethodStructure named: methodName]
%

category: 'accessing'
set compile_env: 0
method: CypressClassStructure
classMethods

	classMethods ifNil: [ classMethods := Dictionary new ].
	^classMethods
%

category: 'accessing'
set compile_env: 0
method: CypressClassStructure
className

	^self name
%

category: 'accessing'
set compile_env: 0
method: CypressClassStructure
classVariableNames

	^self properties at: 'classvars' ifAbsent: [#()]
%

category: 'accessing'
set compile_env: 0
method: CypressClassStructure
classVariableNames: someStrings

	^self properties at: 'classvars' put: someStrings
%

category: 'converting'
set compile_env: 0
method: CypressClassStructure
classVariablesString

	^self stringForVariables: self classVariableNames
%

category: 'accessing'
set compile_env: 0
method: CypressClassStructure
comment

	comment ifNil: [ comment := '' ].
	^comment
%

category: 'accessing'
set compile_env: 0
method: CypressClassStructure
comment: aString

	comment := aString
%

category: 'initialization'
set compile_env: 0
method: CypressClassStructure
fromClassDefinition: classDefinition

	self
		isClassExtension: false;
		name: classDefinition name;
		comment: classDefinition comment;
		superclassName: classDefinition superclassName;
		instanceVariableNames: classDefinition instVarNames;
		classInstanceVariableNames: classDefinition classInstVarNames;
		classVariableNames: classDefinition classVarNames;
		poolDictionaryNames: classDefinition poolDictionaryNames.
%

category: 'initialization'
set compile_env: 0
method: CypressClassStructure
fromJs: jsObject

	properties := jsObject at: 'properties.json'.
	(jsObject at: 'class' ifAbsent: [#()]) do: [:jsMethodObject |  | methodNameParts |
		methodNameParts := self splitMethodNameFor: jsMethodObject.
		(self classMethodNamed: (methodNameParts at: 1)) 
			packageStructure: self packageStructure;
			classStructure: self;
			isMetaclass: true;
			fromJs: jsMethodObject named: methodNameParts ].
	(jsObject at: 'instance' ifAbsent: [#()]) do: [:jsMethodObject |  | methodNameParts |
		methodNameParts := self splitMethodNameFor: jsMethodObject.
		(self instanceMethodNamed: (methodNameParts at: 1)) 
			packageStructure: self packageStructure;
			classStructure: self;
			fromJs: jsMethodObject named: methodNameParts ].	
	comment := jsObject at: 'README.md' ifAbsent: ['']
%

category: 'querying'
set compile_env: 0
method: CypressClassStructure
instanceMethodNamed: methodName

	^self instanceMethods
		at: methodName 
		ifAbsentPut: [CypressMethodStructure named: methodName]
%

category: 'accessing'
set compile_env: 0
method: CypressClassStructure
instanceMethods

	instanceMethods ifNil: [ instanceMethods := Dictionary new ].
	^instanceMethods
%

category: 'accessing'
set compile_env: 0
method: CypressClassStructure
instanceVariableNames

	^self properties at: 'instvars' ifAbsent: [#()]
%

category: 'accessing'
set compile_env: 0
method: CypressClassStructure
instanceVariableNames: someStrings

	^self properties at: 'instvars' put: someStrings
%

category: 'converting'
set compile_env: 0
method: CypressClassStructure
instanceVariablesString

	^self stringForVariables: self instanceVariableNames
%

category: 'accessing'
set compile_env: 0
method: CypressClassStructure
isClassExtension

        isClassExtension ifNil: [ isClassExtension := true ].
        ^isClassExtension
%

category: 'accessing'
set compile_env: 0
method: CypressClassStructure
isClassExtension: aBoolean

	isClassExtension := aBoolean
%

category: 'accessing'
set compile_env: 0
method: CypressClassStructure
name

	^self properties at: 'name'
%

category: 'accessing'
set compile_env: 0
method: CypressClassStructure
name: aString

	self properties at: 'name' put: aString
%

category: 'converting'
set compile_env: 0
method: CypressClassStructure
poolDictionariesString

	^self stringForVariables: self poolDictionaryNames
%

category: 'accessing'
set compile_env: 0
method: CypressClassStructure
poolDictionaryNames

	^self properties at: 'pools' ifAbsent: [#()]
%

category: 'accessing'
set compile_env: 0
method: CypressClassStructure
poolDictionaryNames: someStrings

	^self properties at: 'pools' put: someStrings
%

category: 'private'
set compile_env: 0
method: CypressClassStructure
splitMethodName: methodName

	| extension |
	extension := #('.json' '.st')
		detect: [:each | methodName endsWith: each] 
		ifNone: [CypressError signal: 'invalid structure element: ', methodName].
	^Array
		with: (methodName copyWithoutSuffix: extension)
		with: extension.
%

category: 'private'
set compile_env: 0
method: CypressClassStructure
splitMethodNameFor: jsMethodObject

	^self splitMethodName: (jsMethodObject at: 'name')
%

category: 'accessing'
set compile_env: 0
method: CypressClassStructure
superclassName

	^self properties at: 'super'
%

category: 'accessing'
set compile_env: 0
method: CypressClassStructure
superclassName: aString

	^self properties at: 'super' put: aString
%

category: 'writing'
set compile_env: 0
method: CypressClassStructure
writeJson: aString methods: someMethodStructures on: aStream  indent: indent

	| methods |
	aStream
		tab: indent;
		nextPutAll: '"', aString, '" : [';
		lf;
		yourself.
	(someMethodStructures asSortedCollection: [:a :b | a selector <= b selector])
	doWithIndex: [:methodStructure :index |
		index > 1 ifTrue: [ aStream nextPutAll: ','; lf ].
		methodStructure writeJsonOn: aStream indent: indent + 1].
	aStream
		tab: indent;
		nextPutAll: '],';
		lf;
		yourself.
%

category: 'writing'
set compile_env: 0
method: CypressClassStructure
writeJsonCommentOn: aStream  indent: indent

	self isClassExtension ifTrue: [^self].
	aStream
		tab: indent;
		nextPutAll: '"README.md" : ';
		yourself.
	self comment writeCypressJsonOn: aStream indent: indent.
	aStream
		nextPutAll: ',';
		lf;
		yourself.

%

category: 'writing'
set compile_env: 0
method: CypressClassStructure
writeJsonNameOn: aStream  indent: indent

	aStream
		tab: indent;
		nextPutAll: '"name"';
		nextPutAll: ' : ';
		nextPutAll: '"', self name, (self isClassExtension ifTrue: [ '.extension' ] ifFalse: [ '.class' ]), '",';
		lf.
%

category: 'writing'
set compile_env: 0
method: CypressClassStructure
writeJsonOn: aStream indent: startIndent

	| indent |
	aStream
		tab: startIndent;
		nextPutAll: '{';
		lf.
	indent := startIndent + 1.
	self
		writeJsonNameOn: aStream indent: indent;
		writeJson: 'instance' methods: self instanceMethods on: aStream indent: indent;
		writeJson: 'class' methods: self classMethods on: aStream indent: indent;
		writeJsonCommentOn: aStream indent: indent;
		writeJsonPropertiesOn: aStream indent: indent.
	aStream
		lf;
		tab: startIndent;
		nextPutAll: ' }'
%

category: 'writing'
set compile_env: 0
method: CypressClassStructure
writeJsonPropertiesOn: aStream  indent: indent

	aStream
		tab: indent;
		nextPutAll: '"properties.json" : ';
		yourself.
	self properties writeCypressJsonOn: aStream indent: indent.
%

! Class Implementation for CypressJsonParser

! ------------------- Class methods for CypressJsonParser

category: 'instance creation'
set compile_env: 0
classmethod: CypressJsonParser
new

	CypressJsonError signal: 'Instantiate the parser with a stream.'
%

category: 'instance creation'
set compile_env: 0
classmethod: CypressJsonParser
on: aStream
	^ self basicNew initializeOn: aStream
%

category: 'accessing'
set compile_env: 0
classmethod: CypressJsonParser
parse: aString
	^ self parseStream: aString readStream
%

category: 'accessing'
set compile_env: 0
classmethod: CypressJsonParser
parseStream: aStream
	^ (self on: aStream) parse
%

! ------------------- Instance methods for CypressJsonParser

category: 'adding'
set compile_env: 0
method: CypressJsonParser
addProperty: anAssociation to: anObject
	"Add the property anAssociation described with key and value to anObject. Subclasses might want to refine this implementation."
	
	^ anObject 
		add: anAssociation;
		yourself
%

category: 'adding'
set compile_env: 0
method: CypressJsonParser
addValue: anObject to: aCollection
	"Add anObject to aCollection. Subclasses might want to refine this implementation."

	^ aCollection copyWith: anObject
%

category: 'creating'
set compile_env: 0
method: CypressJsonParser
createArray
	"Create an empty collection. Subclasses might want to refine this implementation."

	^ Array new
%

category: 'creating'
set compile_env: 0
method: CypressJsonParser
createFalse
	"Create the false literal. Subclasses might want to refine this implementation."
	
	^ false
%

category: 'creating'
set compile_env: 0
method: CypressJsonParser
createNull
	"Create the null literal. Subclasses might want to refine this implementation."

	^ nil
%

category: 'creating'
set compile_env: 0
method: CypressJsonParser
createObject
	"Create an empty object. Subclasses might want to refine this implementation."
	
	^ Dictionary new
%

category: 'creating'
set compile_env: 0
method: CypressJsonParser
createProperty: aKey with: aValue
	"Create an empty attribute value pair. Subclasses might want to refine this implementation."
	
	^ aKey -> aValue
%

category: 'creating'
set compile_env: 0
method: CypressJsonParser
createString: aString
	"Create a string literal. Subclasses might want to refine this implementation."

	^ aString
%

category: 'creating'
set compile_env: 0
method: CypressJsonParser
createTrue
	"Create the true literal. Subclasses might want to refine this implementation."

	^ true
%

category: 'private'
set compile_env: 0
method: CypressJsonParser
expect: aString
	"Expects aString and consume input, throw an error otherwise."

	^(self match: aString)
		ifFalse: [CypressJsonError signal: aString , ' expected']
%

category: 'initialization'
set compile_env: 0
method: CypressJsonParser
initializeOn: aStream
	stream := aStream
%

category: 'private'
set compile_env: 0
method: CypressJsonParser
match: aString
	"Tries to match aString, consume input and answer true if successful."
	
	| position |
	position := stream position.
	aString do: [ :each |
		(stream atEnd or: [ stream next ~= each ]) ifTrue: [ 
			stream position: position.
			^ false ] ].
	self whitespace.
	^ true
%

category: 'parsing'
set compile_env: 0
method: CypressJsonParser
parse

	| result |
	result := self
				whitespace;
				parseValue.
	stream atEnd ifFalse: [CypressJsonError signal: 'end of input expected'].
	^result
%

category: 'parsing'
set compile_env: 0
method: CypressJsonParser
parseArray

	| result |
	self expect: '['.
	result := self createArray.
	(self match: ']') ifTrue: [^result].
	[stream atEnd] whileFalse: 
			[result := self addValue: self parseValue to: result.
			(self match: ']') ifTrue: [^result].
			self expect: ','].
	CypressJsonError signal: 'end of array expected'
%

category: 'parsing-internal'
set compile_env: 0
method: CypressJsonParser
parseCharacter
	| char |
	(char := stream next) = $\ 
		ifFalse: [ ^ char ].
	(char := stream next) = $" 
		ifTrue: [ ^ char ].
	char = $\
		ifTrue: [ ^ char ].
	char = $/
		ifTrue: [ ^ char ].
	char = $b
		ifTrue: [ ^ Character backspace ].
	char = $f
		ifTrue: [ ^ Character newPage ].
	char = $n
		ifTrue: [ ^ Character lf ].
	char = $r
		ifTrue: [ ^ Character cr ].
	char = $t
		ifTrue: [ ^ Character tab ].
	char = $u
		ifTrue: [ ^ self parseCharacterHex ].
	CypressJsonError signal: 'invalid escape character \' , (String with: char)
%

category: 'parsing-internal'
set compile_env: 0
method: CypressJsonParser
parseCharacterHex
	| value |
	value := self parseCharacterHexDigit.
	3 timesRepeat: [ value := (value << 4) + self parseCharacterHexDigit ].
	^ Character codePoint: value
%

category: 'parsing-internal'
set compile_env: 0
method: CypressJsonParser
parseCharacterHexDigit
    | digit |
    stream atEnd
        ifFalse: [ 
            digit := stream next codePoint.
            (digit between: 48 and: 57)
                ifTrue: [ ^ digit - 48 ].	"$0"	"$9"
            (digit between: 65 and: 70)
                ifTrue: [ ^ digit - 55 ].	"$A"	"$F"
            (digit between: 97 and: 102)
                ifTrue: [ ^ digit - 87 ]	"$a"	"$f" ].
    CypressJsonError signal: 'hex-digit expected'
%

category: 'parsing-internal'
set compile_env: 0
method: CypressJsonParser
parseNumber
	| negated number |
	negated := stream peek = $-.
	negated ifTrue: [ stream next ].
	number := self parseNumberInteger.
	(stream peek = $.) ifTrue: [
		stream next. 
		number := number + self parseNumberFraction ].
	(stream peek = $e or: [ stream peek = $E ]) ifTrue: [
		stream next.
		number := number * self parseNumberExponent ].
	negated ifTrue: [ number := number negated ].
	self whitespace.
	^ number
%

category: 'parsing-internal'
set compile_env: 0
method: CypressJsonParser
parseNumberExponent
    | number negated |
    number := 0.
    negated := stream peek = $-.
    (negated or: [ stream peek = $+ ])
        ifTrue: [ stream next ].
    [ stream atEnd not and: [ stream peek isDigit ] ] whileTrue: [ number := 10 * number + (stream next codePoint - 48) ].
    negated
        ifTrue: [ number := number negated ].
    ^ 10 raisedTo: number
%

category: 'parsing-internal'
set compile_env: 0
method: CypressJsonParser
parseNumberFraction
    | number power |
    number := 0.
    power := 1.0.
    [ stream atEnd not and: [ stream peek isDigit ] ]
        whileTrue: [ 
            number := 10 * number + (stream next codePoint - 48).
            power := power * 10.0 ].
    ^ number / power
%

category: 'parsing-internal'
set compile_env: 0
method: CypressJsonParser
parseNumberInteger
    | number |
    number := 0.
    [ stream atEnd not and: [ stream peek isDigit ] ] whileTrue: [ number := 10 * number + (stream next codePoint - 48) ].
    ^ number
%

category: 'parsing'
set compile_env: 0
method: CypressJsonParser
parseObject

	| result |
	self expect: '{'.
	result := self createObject.
	(self match: '}') ifTrue: [^result].
	[stream atEnd] whileFalse: 
			[result := self addProperty: self parseProperty to: result.
			(self match: '}') ifTrue: [^result].
			self expect: ','].
	CypressJsonError signal: 'end of object expected'
%

category: 'parsing-internal'
set compile_env: 0
method: CypressJsonParser
parseProperty
	| name value |
	name := self parseString.
	self expect: ':'.
	value := self parseValue.
	^ self createProperty: name with: value.
%

category: 'parsing-internal'
set compile_env: 0
method: CypressJsonParser
parseString
	| result |
	self expect: '"'.
	result := WriteStream on: String new.
	[ stream atEnd or: [ stream peek = $" ] ] 
		whileFalse: [ result nextPut: self parseCharacter ].
	^ self expect: '"'; createString: result contents
%

category: 'parsing'
set compile_env: 0
method: CypressJsonParser
parseValue
	| char |
	stream atEnd ifFalse: [ 
		char := stream peek.
		char = ${
			ifTrue: [ ^ self parseObject ].
		char = $[
			ifTrue: [ ^ self parseArray ].
		char = $"
			ifTrue: [ ^ self parseString ].
		(char = $- or: [ char between: $0 and: $9 ])
			ifTrue: [ ^ self parseNumber ].
		(self match: 'true')
			ifTrue: [ ^ self createTrue ].
		(self match: 'false')
			ifTrue: [ ^ self createFalse ].
		(self match: 'null')
			ifTrue: [ ^ self createNull ] ].
	CypressJsonError signal: 'invalid input'
%

category: 'private'
set compile_env: 0
method: CypressJsonParser
whitespace
	"Strip whitespaces from the input stream."

	[ stream atEnd not and: [ stream peek isSeparator ] ]
		whileTrue: [ stream next ]
%

! Class Extensions

! Class Extension for Boolean

! ------------------- Instance methods for Boolean

category: '*Cypress-Structure'
set compile_env: 0
method: Boolean
writeCypressJsonOn: aStream indent: startIndent

	aStream nextPutAll: self printString
%

! Class Extension for OrderedCollection

! ------------------- Class methods for OrderedCollection

category: '*Cypress-Structure'
set compile_env: 0
classmethod: OrderedCollection
new: size streamContents: aOneArgBlock

	^self withAll: (super new: size streamContents: aOneArgBlock)
%

category: '*Cypress-Structure'
set compile_env: 0
classmethod: OrderedCollection
streamSpecies

	^Array
%

! Class Extension for Dictionary

! ------------------- Instance methods for Dictionary

category: '*Cypress-Structure'
set compile_env: 0
method: Dictionary
asCypressPropertyObject

	| result |
	result := self class new: self size.
	self associationsDo: [:assoc | result at: assoc key put: assoc value asCypressPropertyObject].
	^result.
%

category: '*Cypress-Structure'
set compile_env: 0
method: Dictionary
writeCypressJsonOn: aStream indent: startIndent

	| indent cnt |
	indent := startIndent.
	aStream
		nextPutAll: '{';
		lf.
	cnt := 0.
	indent := indent + 1.
	self keys asSortedCollection do: 
			[:key |
			| value |
			value := self at: key.
			cnt := cnt + 1.
			aStream tab: indent.
			key writeCypressJsonOn: aStream indent: indent.
			aStream nextPutAll: ' : '.
			value writeCypressJsonOn: aStream indent: indent.
			cnt < self size
				ifTrue: 
					[aStream
						nextPutAll: ',';
						lf]].
	self size = 0 ifTrue: [aStream tab: indent].
	aStream nextPutAll: ' }'
%

! Class Extension for Symbol

! ------------------- Class methods for Symbol

category: '*Cypress-Structure'
set compile_env: 0
classmethod: Symbol
new: size streamContents: aOneArgBlock

	^(super new: size streamContents: aOneArgBlock) asSymbol
%

category: '*Cypress-Structure'
set compile_env: 0
classmethod: Symbol
streamSpecies

	^String
%

! Class Extension for String

! ------------------- Instance methods for String

category: '*Cypress-Structure'
set compile_env: 0
method: String
asCypressPropertyObject

	^self unescapePercents withUnixLineEndings
%

category: '*Cypress-Structure'
set compile_env: 0
method: String
escapePercents
	"Answer a new string with 'dangerous' characters escaped to their %XX form,
	 for use in HTTP transactions."

	^String streamContents: 
			[:stream |
			self do: 
					[:c |
					c isSafeForHTTP
						ifTrue: [stream nextPut: c]
						ifFalse: 
							[stream nextPut: $%.
							c codePoint // 16 printOn: stream base: 16 showRadix: false.
							c codePoint \\ 16 printOn: stream base: 16 showRadix: false]]]
%

category: '*Cypress-Structure'
set compile_env: 0
method: String
unescapePercents
	"decode string including %XX form
	 (adapted from Pharo 2.0)"

	| unescaped char asciiVal specialChars oldPos pos |
	unescaped := ReadWriteStream on: String new.
	specialChars := '+%' asSet.
	oldPos := 1.
	
	[pos := self indexOfAnyOf: specialChars startingAt: oldPos.
	pos > 0]
			whileTrue: 
				[unescaped nextPutAll: (self copyFrom: oldPos to: pos - 1).
				char := self at: pos.
				(char = $% and: [pos + 2 <= self size])
					ifTrue: 
						[asciiVal := ((self at: pos + 1) asUppercase digitValueInRadix: 16) * 16
									+ ((self at: pos + 2) asUppercase digitValueInRadix: 16).
						asciiVal > 255 ifTrue: [^self].
						unescaped nextPut: (Character withValue: asciiVal).
						pos := pos + 3.
						pos <= self size ifFalse: [char := nil].
						oldPos := pos]
					ifFalse: 
						[char = $+
							ifTrue: [unescaped nextPut: Character space]
							ifFalse: [unescaped nextPut: char].
						oldPos := pos + 1]].
	oldPos <= self size
		ifTrue: [unescaped nextPutAll: (self copyFrom: oldPos to: self size)].
	^unescaped contents
%

category: '*Cypress-Structure'
set compile_env: 0
method: String
writeCypressJsonOn: aStream indent: startIndent

	aStream
		nextPutAll: '"';
		nextPutAll: self withUnixLineEndings encodeAsUTF8 escapePercents;
		nextPutAll: '"'
%

! Class Extension for Interval

! ------------------- Class methods for Interval

category: '*Cypress-Structure'
set compile_env: 0
classmethod: Interval
streamSpecies

	^Array
%

! Class Extension for Behavior

! ------------------- Instance methods for Behavior

category: '*Cypress-Structure'
set compile_env: 0
method: Behavior
methodDictionary

	^self methodDictForEnv: 0
%

! Class Extension for SequenceableCollection

! ------------------- Class methods for SequenceableCollection

category: '*Cypress-Structure'
set compile_env: 0
classmethod: SequenceableCollection
new: newSize streamContents: aOneArgBlock

	| stream |
	stream := WriteStream on: (self streamSpecies new: newSize).
	aOneArgBlock value: stream.
	stream position = newSize
		ifTrue: [ ^stream originalContents ]
		ifFalse: [ ^stream contents ]
%

category: '*Cypress-Structure'
set compile_env: 0
classmethod: SequenceableCollection
streamContents: aOneArgBlock

	^ self new: 100 streamContents: aOneArgBlock
%

category: '*Cypress-Structure'
set compile_env: 0
classmethod: SequenceableCollection
streamSpecies
	"Answer the class that is used for streaming.
	 If overridden, consider overriding #new:streamContents:."

	^self
%

! ------------------- Instance methods for SequenceableCollection

category: '*Cypress-Structure'
set compile_env: 0
method: SequenceableCollection
beginsWith: aSequence
	"Answer whether the first elements of the receiver are the same as aSequence."

	^(self indexOfSubCollection: aSequence startingAt: 1) = 1
%

category: '*Cypress-Structure'
set compile_env: 0
method: SequenceableCollection
copyWithoutSuffix: aSequence
	"Answer a copy of the receiver excluding the specified suffix.
	 If the suffix does not match, answer a copy of the receiver."

	^self copyWithoutSuffix: aSequence or: [self copy].
%

category: '*Cypress-Structure'
set compile_env: 0
method: SequenceableCollection
copyWithoutSuffix: aSequence or: aBlock
	"Answer a copy of the receiver excluding the specified suffix.
	 If the suffix does not match, answer the result of evaluating aBlock."

	(self endsWith: aSequence) ifFalse: [^aBlock value].
	^self copyFrom: 1 to: self size - aSequence size.
%

category: '*Cypress-Structure'
set compile_env: 0
method: SequenceableCollection
endsWith: aSequence
	"Answer whether the last elements of the receiver are the same as aSequence."

	| expectedStart |
	expectedStart := self size - aSequence size + 1 max: 1.
	^expectedStart
		= (self indexOfSubCollection: aSequence startingAt: expectedStart)
%

category: '*Cypress-Structure'
set compile_env: 0
method: SequenceableCollection
indexOfAnyOf: aCollection startingAt: start
	"Answer the index of the first occurence of any element included in aCollection after start within the receiver.
	If the receiver does not contain anElement, answer zero, which is an invalid index."

	^self indexOfAnyOf: aCollection startingAt: start ifAbsent: [0]
%

category: '*Cypress-Structure'
set compile_env: 0
method: SequenceableCollection
indexOfAnyOf: aCollection startingAt: start ifAbsent: exceptionBlock
	"Answer the index of the first occurence of any element included in aCollection after start within the receiver.
	If the receiver does not contain anElement, answer the result of evaluating the argument, exceptionBlock.
	Note: it is user responsibility to provide aCollection that behaves relatevily fast when asked for includes: (like a Set)"

	start to: self size do:
		[:index |
		(aCollection includes: (self at: index)) ifTrue: [^ index]].
	^ exceptionBlock value
%

! Class Extension for Array

! ------------------- Instance methods for Array

category: '*Cypress-Structure'
set compile_env: 0
method: Array
asCypressPropertyObject

	^self collect: [:each | each asCypressPropertyObject]
%

category: '*Cypress-Structure'
set compile_env: 0
method: Array
writeCypressJsonOn: aStream indent: startIndent

	| indent |
	aStream
		nextPutAll: '[';
		lf.
	indent := startIndent + 1.
	1 to: self size
		do: 
			[:index |
			| item |
			item := self at: index.
			aStream tab: indent.
			item writeCypressJsonOn: aStream indent: indent.
			index < self size
				ifTrue: 
					[aStream
						nextPutAll: ',';
						lf]].
	self size = 0 ifTrue: [aStream tab: indent].
	aStream nextPutAll: ' ]'
%

! Class Extension for Object

! ------------------- Instance methods for Object

category: '*Cypress-Structure'
set compile_env: 0
method: Object
asCypressPropertyObject

	^self
%

category: '*Cypress-Structure'
set compile_env: 0
method: Object
writeCypressJsonOn: fileStream

	self writeCypressJsonOn: fileStream indent: 0
%

! Class Extension for SortedCollection

! ------------------- Class methods for SortedCollection

category: '*Cypress-Structure'
set compile_env: 0
classmethod: SortedCollection
new: size streamContents: aOneArgBlock

	^self withAll: (super new: size streamContents: aOneArgBlock)
%

category: '*Cypress-Structure'
set compile_env: 0
classmethod: SortedCollection
streamSpecies

	^Array
%

! Class Extension for ByteArray

! ------------------- Instance methods for ByteArray

category: '*Cypress-Structure'
set compile_env: 0
method: ByteArray
escapePercents
	"Answer a new string with 'dangerous' characters escaped to their %XX form,
	 for use in HTTP transactions."

	^String streamContents: 
			[:stream |
			self do: 
					[:each |
					| c |
					(c := Character withValue: each) isSafeForHTTP
						ifTrue: [stream nextPut: c]
						ifFalse: 
							[stream nextPut: $%.
							each // 16 printOn: stream base: 16 showRadix: false.
							each \\ 16 printOn: stream base: 16 showRadix: false]]]
%

! Class Extension for Number

! ------------------- Instance methods for Number

category: '*Cypress-Structure'
set compile_env: 0
method: Number
writeCypressJsonOn: aStream indent: startIndent

	aStream nextPutAll: self printString
%

! Class Extension for Character

! ------------------- Instance methods for Character

category: '*Cypress-Structure'
set compile_env: 0
method: Character
isSafeForHTTP
	"Answer whether a character is 'safe', or needs to be escaped when used, eg, in a URL."

	^self codePoint < 128
		and: [self isAlphaNumeric or: ['.-_' includes: self]]
%

