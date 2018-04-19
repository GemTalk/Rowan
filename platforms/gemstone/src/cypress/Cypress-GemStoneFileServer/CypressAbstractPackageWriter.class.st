"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressAbstractPackageWriter',
	#superclass : 'CypressAbstractPackageFiler',
	#classInstVars : [
		'specials'
	],
	#category : 'Cypress-GemStoneFileServer'
}

{ #category : 'initialization' }
CypressAbstractPackageWriter class >> initializeSpecials [
	"Valid binarySelector characters  '!' | '%' | '&' | '*' | '+' | ','' | '/' | '<' | '=' | '>' | '?' | '@' | '\' | '~' | '|' | '-'"

	| map |
	map := Dictionary new.
	map
		at: $! put: 'bang';
		at: $% put: 'percent';
		at: $& put: 'and';
		at: $* put: 'star';
		at: $+ put: 'plus';
		at: $, put: 'comma';
		at: $- put: 'minus';
		at: $/ put: 'slash';
		at: $< put: 'less';
		at: $= put: 'equals';
		at: $> put: 'more';
		at: $? put: 'wat';
		at: $@ put: 'at';
		at: $\ put: 'backslash';
		at: $| put: 'pipe';
		at: $~ put: 'tilde'.
	map keys do: [:key | map at: (map at: key) put: key].
	^map

]

{ #category : 'accessing' }
CypressAbstractPackageWriter class >> specials [

	^specials ifNil: [specials := self initializeSpecials]

]

{ #category : 'private' }
CypressAbstractPackageWriter >> determinePackageDirectory [

	^self fileUtils ensureDirectoryExists: (self fileUtils
				directoryFromPath: self packageStructure name
				relativeTo: self repository directoryPath)

]

{ #category : 'private' }
CypressAbstractPackageWriter >> directoryForDirectoryNamed: directoryNameOrPath [

	^directoryNameOrPath = '.'
		ifTrue: [self fileUtils ensureDirectoryExists: self packageDirectory]
		ifFalse: [self subPackageFileDirectoryFor: directoryNameOrPath]

]

{ #category : 'private' }
CypressAbstractPackageWriter >> fileNameForSelector: selector [

	^selector last = $:
		ifTrue: [selector copyReplacing: $: with: $.]
		ifFalse: 
			[(selector first isLetter or: [selector first = $_])
				ifTrue: [selector]
				ifFalse: 
					[| specials output |
					specials := self class specials.
					output := WriteStreamPortable on: (String new: 100).
					output nextPut: $^.
					selector do: [:each | output nextPutAll: (specials at: each)]
						separatedBy: [output nextPut: $.].
					output contents]]

]

{ #category : 'writing' }
CypressAbstractPackageWriter >> removeOldReplacingWithNew [
  self fileUtils deleteAll: self packageDirectory.
  self writePropertiesFile.
  self writePackageStructure

]

{ #category : 'private' }
CypressAbstractPackageWriter >> subPackageFileDirectoryFor: directoryNameOrPath [

	| dir |
	dir := self fileUtils directoryFromPath: directoryNameOrPath
				relativeTo: self packageDirectory.
	self fileUtils ensureDirectoryExists: dir.
	^dir

]

{ #category : 'writing' }
CypressAbstractPackageWriter >> writeClassComment: classStructure on: fileStream [

	fileStream
		nextPutAll: (CypressObject normalizeLineEndingsOf: classStructure comment)

]

{ #category : 'writing' }
CypressAbstractPackageWriter >> writeClassStructure: classStructure to: classPath [

	self
		writeInDirectoryName: classPath
			fileName: 'README'
			extension: '.md'
			visit: [:fileStream | self writeClassComment: classStructure on: fileStream];
		writeInDirectoryName: classPath
			fileName: 'properties'
			extension: self propertiesFileNameExtension
			visit: [:fileStream | classStructure properties _writeCypressJsonOn: fileStream]

]

{ #category : 'writing' }
CypressAbstractPackageWriter >> writeExtensionClassStructure: classStructure to: classPath [

	self
		writeInDirectoryName: classPath
		fileName: 'properties'
		extension: self propertiesFileNameExtension
		visit: 
			[:fileStream |
			(Dictionary with: 'name' -> classStructure className)
				_writeCypressJsonOn: fileStream]

]

{ #category : 'private' }
CypressAbstractPackageWriter >> writeInDirectoryName: directoryNameOrPath fileName: fileName extension: ext visit: visitBlock [

	| directory |
	directory := self directoryForDirectoryNamed: directoryNameOrPath.
	self fileUtils
		writeStreamFor: fileName , ext
		in: directory
		do: [:fileStream | visitBlock value: fileStream]

]

{ #category : 'writing' }
CypressAbstractPackageWriter >> writeMethodStructure: methodStructure onStream: fileStream [

	self subclassResponsibility: #writeMethodStructure:onStream:

]

{ #category : 'writing' }
CypressAbstractPackageWriter >> writeMethodStructure: methodStructure to: methodPath [

	| filename |
	filename := self fileNameForSelector: methodStructure selector.
	self
		writeInDirectoryName: methodPath
		fileName: filename
		extension: '.st'
		visit: [:fileStream | self writeMethodStructure: methodStructure onStream: fileStream]

]

{ #category : 'writing' }
CypressAbstractPackageWriter >> writePackageStructure [

	self
		writePackageStructureClasses: self packageStructure classes
			isClassExtension: false;
		writePackageStructureClasses: self packageStructure extensions
			isClassExtension: true

]

{ #category : 'writing' }
CypressAbstractPackageWriter >> writePackageStructure: aPackageStructure [

	self
		packageStructure: aPackageStructure;
		packageDirectory: self determinePackageDirectory;
		removeOldReplacingWithNew

]

{ #category : 'writing' }
CypressAbstractPackageWriter >> writePackageStructureClasses: classStructures isClassExtension: isClassExtension [

	| classDirExtension |
	classDirExtension := isClassExtension
				ifTrue: ['.extension']
				ifFalse: ['.class'].
	classStructures do: 
			[:classStructure |
			| classPath instanceMethodPath classMethodPath |
			classPath := classStructure className , classDirExtension
						, self fileUtils pathNameDelimiter asString.
			isClassExtension
				ifTrue: [self writeExtensionClassStructure: classStructure to: classPath]
				ifFalse: [self writeClassStructure: classStructure to: classPath].
			instanceMethodPath := classPath , 'instance' , self fileUtils pathNameDelimiter asString.
			classStructure instanceMethods
				do: [:methodStructure | self writeMethodStructure: methodStructure to: instanceMethodPath].
			classMethodPath := classPath , 'class' , self fileUtils pathNameDelimiter asString.
			classStructure classMethods
				do: [:methodStructure | self writeMethodStructure: methodStructure to: classMethodPath]]

]

{ #category : 'writing' }
CypressAbstractPackageWriter >> writePropertiesFile [

	self
		writeInDirectoryName: '.'
		fileName: 'properties'
		extension: self propertiesFileNameExtension
		visit: [:fileStream | Dictionary new _writeCypressJsonOn: fileStream]

]
