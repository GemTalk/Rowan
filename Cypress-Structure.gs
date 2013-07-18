doit
(Object subclass: 'CypressStructure'  instVarNames: #( name properties packageStructure)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Structure'
.
%
doit
(CypressStructure subclass: 'CypressClassStructure'  instVarNames: #( instanceMethods classMethods comment                    isClassExtension)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Structure'
.
%
doit
(CypressStructure subclass: 'CypressMethodStructure'  instVarNames: #( source isMetaclass classStructure)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Structure'
.
%
doit
(CypressStructure subclass: 'CypressPackageStructure'  instVarNames: #( classes extensions)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Structure'
.
%
doit
CypressStructure immediateInvariant.
%
doit
CypressClassStructure immediateInvariant.
%
doit
CypressMethodStructure immediateInvariant.
%
doit
CypressPackageStructure immediateInvariant.
%

! Remove existing behavior from CypressStructure
doit
CypressStructure removeAllMethods.
CypressStructure class removeAllMethods.
%
! ------------------- Class methods for CypressStructure
category: 'instance creation'
set compile_env: 0
classmethod: CypressStructure
fromJs: jsObject

	^(self new) 
		fromJs: jsObject asCypressPropertyObject;
		yourself
%
! ------------------- Instance methods for CypressStructure
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
category: 'initialization'
set compile_env: 0
method: CypressStructure
fromJs: jsObject

	self subclassResponsibility
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

! Remove existing behavior from CypressClassStructure
doit
CypressClassStructure removeAllMethods.
CypressClassStructure class removeAllMethods.
%
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
	^self properties at: 'classinstvars' ifAbsent: ['']
%
category: 'accessing'
set compile_env: 0
method: CypressClassStructure
classInstanceVariableNames: aString
	^self properties at: 'classinstvars' put: aString
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

	^self properties at: 'instvars' ifAbsent: ['']
%
category: 'accessing'
set compile_env: 0
method: CypressClassStructure
instanceVariableNames: aString

	^self properties at: 'instvars' put: aString
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
category: 'converting'
set compile_env: 0
method: CypressClassStructure
asCypressClassDefinition
	self isClassExtension ifTrue: [ ^nil ].
	^CypressClassDefinition
		name: self className
		superclassName: self superclassName
		category: self category 
		instVarNames: self instanceVariableNames
		classInstVarNames: self classInstanceVariableNames
		comment: self comment
%
category: 'initialization'
set compile_env: 0
method: CypressClassStructure
fromClassDefinition: classDefinition

	self isClassExtension: false.
	self name: classDefinition name.
	self comment: classDefinition comment.
  	self superclassName: classDefinition superclassName.
	self instanceVariableNames: classDefinition instVarNames.
	self classInstanceVariableNames: classDefinition classInstVarNames.
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
category: 'private'
set compile_env: 0
method: CypressClassStructure
splitMethodName: methodName

	| ext  |
	ext := '.json'.
	(   '*' , ext match: methodName)
		ifFalse: [
			ext := '.st'.
			('*' , ext match: methodName)
				ifFalse: [ self error: 'invalid structure element: ', methodName ] ].
	^{methodName copyFrom: 1 to: (methodName size - ext size). ext}
%
category: 'private'
set compile_env: 0
method: CypressClassStructure
splitMethodNameFor: jsMethodObject

	^self splitMethodName: (jsMethodObject at: 'name')
%
category: 'querying'
set compile_env: 0
method: CypressClassStructure
classMethodNamed: methodName

	^self classMethods 
		at: methodName 
		ifAbsent: [ self classMethods at: methodName put: (CypressMethodStructure new name: methodName) ]
%
category: 'querying'
set compile_env: 0
method: CypressClassStructure
instanceMethodNamed: methodName

	^self instanceMethods 
		at: methodName 
		ifAbsent: [ self instanceMethods at: methodName put: (CypressMethodStructure new name: methodName) ]
%
category: 'writing'
set compile_env: 0
method: CypressClassStructure
writeJsonOn: aStream  indent: startIndent

	| indent methods |
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
		nextPutAll: '"', self name, (self isClassExtension ifTrue: [ '.extension' ] ifFalse: [ '.class' ]), '",';
		lf.
	aStream
		tab: indent;
		nextPutAll: '"instance" : [';
		lf;
		yourself.
	methods := self instanceMethods values asArray sorted: [:a :b | a selector <= b selector].
	1 to: methods size do: [:index | | methodStructure | 
		methodStructure := methods at: index.
		methodStructure writeJsonOn: aStream indent: indent + 1.
		index < methods size ifTrue: [ aStream nextPutAll: ','; lf ]].
	aStream
		tab: indent;
		nextPutAll: '],';
		lf;
		yourself.
	aStream
		tab: indent;
		nextPutAll: '"class" : [';
		lf;
		yourself.
	methods := self classMethods values asArray sorted: [:a :b | a selector <= b selector].
	1 to: methods size do: [:index | | methodStructure | 
		methodStructure := methods at: index.
		methodStructure writeJsonOn: aStream indent: indent + 1.
		index < methods size ifTrue: [ aStream nextPutAll: ','; lf ]].
	aStream
		tab: indent;
		nextPutAll: ']'.
	self isClassExtension
		ifFalse: [ 
			aStream
				nextPutAll: ',';
				lf;
				tab: indent;
				nextPutAll: '"README.md" : ';
				yourself.
			self comment writeCypressJsonOn: aStream indent: indent ].
	aStream
		nextPutAll: ',';
		lf;
		tab: indent;
		nextPutAll: '"properties.json" : ';
		yourself.
	self properties writeCypressJsonOn: aStream indent: indent.
	indent := indent - 1.
	aStream
		lf;
		tab: indent;
		nextPutAll: ' }'
%

! Remove existing behavior from CypressMethodStructure
doit
CypressMethodStructure removeAllMethods.
CypressMethodStructure class removeAllMethods.
%
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

! Remove existing behavior from CypressPackageStructure
doit
CypressPackageStructure removeAllMethods.
CypressPackageStructure class removeAllMethods.
%
! ------------------- Class methods for CypressPackageStructure
category: 'instance creation'
set compile_env: 0
classmethod: CypressPackageStructure
fromJson: aJsonString

	^self fromJs: (MCFileTreeJsonParser parse: aJsonString)
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

	^self name copyFrom: 1 to: (self name size - self packageExtension size)
%
category: 'accessing'
set compile_env: 0
method: CypressPackageStructure
packageStructure
	^self
%
category: 'conversion'
set compile_env: 0
method: CypressPackageStructure
asCypressJson

	| stream |
    	stream := WriteStream on: String new.
    	self writeJsonOn: stream.
	^stream contents
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
                (  '*.extension' match:(objectName := jsClassObject at: 'name') )
			ifTrue: [ 
				classStructure isClassExtension: true.
				self extensions add: classStructure ]
			ifFalse: [
				( '*.class' match: objectName)
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
	classDefinitions := Set new.
	classMap := Dictionary new.
	snapshot definitions do: [:definition |  
			definition 
				classDefinition: [:classDefinition |  classDefinitions add: classDefinition ] 
				methodDefinition: [:methodDefinition | 
					(classMap 
						at: methodDefinition className 
						ifAbsent: [classMap at: methodDefinition className put: Set new]) 
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
		classStructure := (CypressClassStructure new name: className)
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
category: 'snapshotting'
set compile_env: 0
method: CypressPackageStructure
snapshot
	| definitions map  |
	definitions := OrderedCollection new.
	self classes do: [:classStructure |
        	definitions add: classStructure asCypressClassDefinition.
                (classStructure instanceMethods values asArray sorted: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ].
                (classStructure classMethods values asArray sorted: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ]].
	self extensions do: [:classStructure |
                (classStructure instanceMethods values asArray sorted: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ].
                (classStructure classMethods values asArray sorted: [:a :b | a selector <= b selector]) do: [:methodStructure |
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
