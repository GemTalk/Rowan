  set u SystemUser p swordfish
  login

# set rowanCompile to true 
#
  run
  UserGlobals at: #rowanCompile put: true.
  System commit
%

category: '*tonel-gemstone-kernel'
method: CharacterCollection
findString: subString startingAt: startIndex caseSensitive: aBoolean

	^ self _findString: subString startingAt: startIndex ignoreCase: aBoolean not
%

method: CharacterCollection
withGemstoneLineEndings

	"assume the string is textual, and that CR, LF, and CRLF are all 
  valid line endings.  Replace each occurence with a single LF"

	| cr lf crlf inPos outPos outString lineEndPos newOutPos |
	cr := Character cr.
	lf := Character lf.
	crlf := ByteArray new.
	crlf
		add: cr asciiValue;
		add: lf asciiValue.

	inPos := 1.
	outPos := 1.
	outString := self class _newString: self size.

	[ 
	lineEndPos := self indexOfAnyOf: crlf startingAt: inPos ifAbsent: [ 0 ].
	lineEndPos ~= 0 ]
		whileTrue: [ 
			newOutPos := outPos + (lineEndPos - inPos + 1).
			outString
				replaceFrom: outPos
				to: newOutPos - 2
				with: self
				startingAt: inPos.
			outString at: newOutPos - 1 put: lf.
			outPos := newOutPos.

			((self at: lineEndPos) = cr
				and: [ lineEndPos < self size and: [ (self at: lineEndPos + 1) = lf ] ])
				ifTrue: [ 
					"CRLF ending"
					inPos := lineEndPos + 2 ]
				ifFalse: [ 
					"CR or LF ending"
					inPos := lineEndPos + 1 ] ].	"no more line endings.  copy the rest"
	newOutPos := outPos + (self size - inPos + 1).
	outString
		replaceFrom: outPos
		to: newOutPos - 1
		with: self
		startingAt: inPos.

	^ outString copyFrom: 1 to: newOutPos - 1
%

method: Symbol
keywords

	"Answer an array of the keywords that compose the receiver."

	| kwd char keywords |
	keywords := Array new.
			kwd := WriteStreamPortable on: String new.
			1 to: self size do: [ :i | 
				kwd nextPut: (char := self at: i).
				char = $:
					ifTrue: [ 
						keywords add: kwd contents.
						kwd reset ] ].
			kwd position = 0
				ifFalse: [ keywords add: kwd contents ].
	(keywords size >= 1 and: [ (keywords at: 1) = ':' ])
		ifTrue: [ 
			"Has an initial keyword, as in #:if:then:else:"
			keywords := keywords allButFirst ].
	(keywords size >= 2 and: [ (keywords at: keywords size - 1) = ':' ])
		ifTrue: [ 
			"Has a final keyword, as in #nextPut::andCR"
			keywords := keywords
				copyReplaceFrom: keywords size - 1
				to: keywords size
				with: {(':' , keywords last)} ].
	^ keywords
%

method: CharacterCollection
endsWith: suffix

	"Answer whether the tail end of the receiver is the same as suffix.
	 The comparison is case-sensitive."

	| ofs |
	suffix size == 0
		ifTrue: [ ^ false ].
	(ofs := self size - suffix size) < 0
		ifTrue: [ ^ false ].
	^ self at: ofs + 1 equals: suffix	"
  'Elvis' endsWith: 'vis'
  'Elvis' endsWith: ''
"
%
method: Collection
select: selectBlock thenDo: doBlock
  "Utility method to improve readability."

  ^ (self select: selectBlock) do: doBlock
%
method: SequenceableCollection
beginsWith: aSequenceableCollection

	(aSequenceableCollection isEmpty
		or: [ self size < aSequenceableCollection size ])
		ifTrue: [ ^ false ].
	aSequenceableCollection
		withIndexDo: [ :each :index | 
			(self at: index) ~= each
				ifTrue: [ ^ false ] ].
	^ true
%
method: SequenceableCollection
withIndexDo: elementAndIndexBlock

	"Just like with:do: except that the iteration index supplies the second argument to the block."

	1 to: self size do: [ :index | elementAndIndexBlock value: (self at: index) value: index ]
%
method: CharacterCollection
unescapePercents

	"change each %XY substring to the character with ASCII value XY in hex.  This is the opposite of #encodeForHTTP"

	| ans c asciiVal pos oldPos specialChars |
	ans _ WriteStream on: String new.
	oldPos _ 1.
	specialChars _ '+%'.

	[ 
	pos _ self indexOfAnyOf: specialChars startingAt: oldPos.
	pos > 0 ]
		whileTrue: [ 
			ans nextPutAll: (self copyFrom: oldPos to: pos - 1).
			c _ self at: pos.
			c = $+
				ifTrue: [ ans nextPut: $  ]
				ifFalse: [ 
					(c = $% and: [ pos + 2 <= self size ])
						ifTrue: [ 
							asciiVal _ ((self at: pos + 1) asUppercase digitValueInRadix: 16) * 16
								+ ((self at: pos + 2) asUppercase digitValueInRadix: 16).
							pos _ pos + 2.
							asciiVal > 255
								ifTrue: [ ^ self ].	"not really an escaped string"
							ans nextPut: (Character value: asciiVal) ]
						ifFalse: [ ans nextPut: c ] ].
			oldPos _ pos + 1 ].
	ans nextPutAll: (self copyFrom: oldPos to: self size).
	^ ans contents
%
method: CharacterCollection
indexOfAnyOf: specialChars startingAt: oldPos

	oldPos to: self size do: [ :i | 
		(specialChars includes: (self at: i))
			ifTrue: [ ^ i ] ].
	^ 0
%
method: SequenceableCollection
copyUpTo: anObject

	"Answer all elements up to but not including anObject. If there
  is no such object, answer a copy of the receiver."

	| idx |
	idx := self indexOf: anObject startingAt: 1.
	idx == 0
		ifTrue: [ ^ self copy ]
		ifFalse: [ ^ self copyFrom: 1 to: idx - 1 ]
%
method: SequenceableCollection
writeStreamPortable

	^ WriteStreamPortable on: self
%
method: CharacterCollection
trimLeft: aLeftBlock right: aRightBlock

	"Trim characters satisfying the condition given in aLeftBlock from the left side and aRightBlock from the right sides of the receiving string."

	| left right |
	left := 1.
	right := self size.
	[ left <= right and: [ aLeftBlock value: (self at: left) ] ]
		whileTrue: [ left := left + 1 ].
	[ left <= right and: [ aRightBlock value: (self at: right) ] ]
		whileTrue: [ right := right - 1 ].
	^ self copyFrom: left to: right
%
method: CharacterCollection
trimBoth: aBlock

	"Trim characters satisfying the condition given in aBlock from both sides of the receiving string."

	^ self trimLeft: aBlock right: aBlock
%
method: CharacterCollection
trimBoth

	"Trim separators from both sides of the receiving string."

	^ self trimBoth: [ :char | char isSeparator ]
%
method: CharacterCollection
trimLeft: aBlock

	"Trim characters satisfying the condition given in aBlock from the left side of the receiving string."

	^ self trimLeft: aBlock right: [ :char | false ]
%
method: CharacterCollection
trimLeft

	"Trim separators from the left side of the receiving string."

	^ self trimLeft: [ :char | char isSeparator ]
%
method: CharacterCollection
trimRight: aBlock

	"Trim characters satisfying the condition given in aBlock from the right side of the receiving string."

	^ self trimLeft: [ :char | false ] right: aBlock
%
method: CharacterCollection
trimRight

	"Trim separators from the right side of the receiving string."

	^ self trimRight: [ :char | char isSeparator ]
%
method: SequenceableCollection
second

	"Answer the second element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 2
%
method: SequenceableCollection
third

	"Answer the third element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 3
%
method: SequenceableCollection
fourth

	"Answer the fourth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 4
%
method: SequenceableCollection
fifth

	"Answer the fifth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 5
%
method: SequenceableCollection
sixth

	"Answer the sixth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 6
%
method: SequenceableCollection
allButLast: n

	"Answer a copy of the receiver containing all but the last n
	elements. Raise an error if there are not enough elements."

	^ self copyFrom: 1 to: self size - n
%
method: SequenceableCollection
allButLast

	"Answer a copy of the receiver containing all but the last
	element. Raise an error if there are not enough elements."

	^ self allButLast: 1
%
method: CharacterCollection
substrings: separators 
	"Answer an array containing the substrings in the receiver separated 
	by the elements of separators."
	| result sourceStream subStringStream |
	
	(separators isString or: [ separators allSatisfy: [ :element | element isCharacter ] ])
		ifFalse: [ ^ self error: 'separators must be Characters.' ].
	sourceStream := self readStream.
	result := OrderedCollection new.
	subStringStream := String new writeStreamPortable.
	[ sourceStream atEnd ] whileFalse: [
		| char |
		char := sourceStream next.
		(separators includes: char)
			ifTrue: [
				subStringStream isEmpty ifFalse: [
					result add: subStringStream contents.
					subStringStream := String new writeStreamPortable ] ]
			ifFalse: [
				subStringStream nextPut: char ] ].
	subStringStream isEmpty ifFalse: [
		result add: subStringStream contents ].
	^ result asArray
%
method: Collection
ifNotEmpty: aBlock

	^ self size == 0
		ifFalse: [ aBlock cull: self ]
%
method: ExecBlock
cull: anArg

	"Return the value of the receiver evaluated with 0 or 1 arguments. If the block
   expects 1 argument pass anArg as the value of the argument"

	^ self argumentCount == 0
		ifTrue: [ self value ]
		ifFalse: [ self value: anArg ]
%
method: ExecBlock
cull: firstArg cull: secondArg

       "Return the value of the receiver evaluated with between 0 and 2 arguments,
   discarding arguments not needed by the receiver."

       | nargs |
       (nargs := self argumentCount) < 2
               ifTrue: [ 
                       nargs == 1
                               ifTrue: [ ^ self value: firstArg ].
                       ^ self value ].
       ^ self value: firstArg value: secondArg
%
method: Object
putOn: aStream

	^ aStream nextPut: self
%
method: SequenceableCollection
putOn: aStream

	self do: [ :each | each putOn: aStream ]
%
method: CharacterCollection
putOn: aStream

	^ aStream nextPutAll: self
%
method: Stream
<< items

	items putOn: self
%
method: Object
isCollection

	"Return true if the receiver is some sort of Collection and responds to basic collection messages such as #size and #do:"

	^ false
%
method: Collection
isCollection

	"Return true if the receiver is some sort of Collection and responds to basic collection messages such as #size and #do:"

	^ true
%
method: Collection
sort: aSortBlock

	"Sort this array using aSortBlock. The block should take two arguments
	and return true if the first element should preceed the second one."

	^ self sortWithBlock: aSortBlock
%
method: SequenceableCollection
sort

	"Sort this array into ascending order using the '<=' operator."

	^ self sort: [ :a :b | a <= b ]
%
method: CharacterCollection
withoutGemstoneLineEndings

	"assume the string is textual, and that CR, LF, and CRLF are all 
	valid line endings.  Remove each occurence. "

	| cr lf crlf inPos outPos outString lineEndPos newOutPos |
	cr := Character cr.
	lf := Character lf.
	crlf := ByteArray new.
	crlf
		add: cr asciiValue;
		add: lf asciiValue.

	inPos := 1.
	outPos := 1.
	outString := self class _newString: self size.

	[ 
	lineEndPos := self indexOfAnyOf: crlf startingAt: inPos ifAbsent: [ 0 ].
	lineEndPos ~= 0 ]
		whileTrue: [ 
			newOutPos := outPos + (lineEndPos - inPos + 1).
			outString
				replaceFrom: outPos
				to: newOutPos - 2
				with: self
				startingAt: inPos.
			outPos := newOutPos - 1.

			((self at: lineEndPos) = cr
				and: [ lineEndPos < self size and: [ (self at: lineEndPos + 1) = lf ] ])
				ifTrue: [ 
					"CRLF ending"
					inPos := lineEndPos + 2 ]
				ifFalse: [ 
					"CR or LF ending"
					inPos := lineEndPos + 1 ] ].	"no more line endings.  copy the rest"
	newOutPos := outPos + (self size - inPos + 1).
	outString
		replaceFrom: outPos
		to: newOutPos - 1
		with: self
		startingAt: inPos.

	^ outString copyFrom: 1 to: newOutPos - 1
%
method: CharacterCollection
indexOfAnyOf: aByteArray startingAt: start ifAbsent: aBlock

	"returns the index of the first character in the given set, starting from start"

	| ans |
	ans := self class
		findFirstInString: self
		inSet: aByteArray asByteArray byteArrayMap
		startingAt: start.
	ans = 0
		ifTrue: [ ^ aBlock value ]
		ifFalse: [ ^ ans ]
%
method: ByteArray
byteArrayMap

	"return a ByteArray mapping each ascii value to a 1 if that ascii value is in the set, and a 0 if it isn't.  Intended for use by primitives only"

	| map |
	map := ByteArray new: 256 withAll: 0.
	self do: [ :ascii | map at: ascii + 1 put: 1 ].
	^ map
%
classmethod: SequenceableCollection
new: size withAll: value

	"Answer an instance of me, with number of elements equal to size, each 
	of which refers to the argument, value."

	^ (self new: size)
		atAllPut: value;
		yourself
%
category: '*ston-gemstonecommon'
classmethod: CharacterCollection
findFirstInString: aString inSet: inclusionMap startingAt: start

	"Trivial, non-primitive version"

	| i stringSize ascii |
	inclusionMap size ~= 256
		ifTrue: [ ^ 0 ].

	i := start.
	stringSize := aString size.
	[ 
	i <= stringSize
		and: [ 
			ascii := (aString at: i) asciiValue.
			ascii < 256
				ifTrue: [ (inclusionMap at: ascii + 1) = 0 ]
				ifFalse: [ true ] ] ]
		whileTrue: [ i := i + 1 ].

	i > stringSize
		ifTrue: [ ^ 0 ].
	^ i
%

# new method compatable with 3.4 and 3.5
#
category: 'Rowan 3.4 compatability method'
classmethod: GsPackagePolicy
currentOrNil

	"Returns nil or the previously installed and enabled GsPackagePolicy."

	| pp |
	pp := ((GsSession currentSession objectNamed: #'UserGlobals') ifNil: [ ^ nil ])
		at: self globalName
		otherwise: nil.
	pp
		ifNotNil: [ 
			pp enabled
				ifTrue: [ ^ pp ] ].
	^ nil
%

  commit

  run
	| session symbolList |
	session := GsCurrentSession currentSession.
	symbolList := session symbolList.
	#( #RowanKernel #RowanLoader #RowanTools)
		do: [:symbolName | 
			| newDict size |
			newDict := SymbolDictionary new
				name: symbolName;
				objectSecurityPolicy: symbolList objectSecurityPolicy;
				yourself.
			size := System myUserProfile symbolList size.
			System myUserProfile insertDictionary: newDict at: size + 1 ].
%
  commit

# Bootstrap FileSystem into image
	run
	| packageManager repo |
	packageManager := CypressPackageManager3 new.
	repo := CypressAbstractRepository
		onUrl: (CypressUrl absoluteFromText: 'tonel:$ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/projects/filesystem/rowan/src/'  )
		alias: ''.
	packageManager defaultSymbolDictionaryName: #Globals.
	#( 'FileSystem-GemStone-Kernel' 'Files' 'Files-Tests' 'Kernel-Methods' 'Network-UUID' 'Network-UUID-Tests' 
			'Zinc-Character-Encoding-Core' 'Zinc-Character-Encoding-Tests' 'FileSystem-Core' 'FileSystem-Disk' 
      'FileSystem-Memory' 'FileSystem-Path' 'FileSystem-Tests-Attributes' 'FileSystem-Tests-Core' 
      'FileSystem-Tests-Disk' 'FileSystem-Tests-GemStone' 'FileSystem-Tests-Memory' )
		do: [ :packageName | 
			packageManager
				addResolvedReference:
					(CypressResolvedReference name: packageName repository: repo) ].
	packageManager loadResolvedReferences.
%
commit

# Bootstrap Rowan into image
  run
  UserGlobals 
    at: #CypressBootstrapRowanBlock 
    put: [:symbolDictName :packageNames  |
    | packageManager repo |
    packageManager := CypressPackageManager3 new.
    repo := CypressAbstractRepository
      onUrl: (CypressUrl absoluteFromText: 'tonel:$ROWAN_PROJECTS_HOME/Rowan/rowan/src/'  )
      alias: ''.
    packageManager
      defaultSymbolDictionaryName: symbolDictName asSymbol.
    packageNames
      do: [ :packageName | 
        packageManager
          addResolvedReference:
            (CypressResolvedReference name: packageName repository: repo) ].
    packageManager loadResolvedReferences ].
%
  commit

# Define the classes needed by definitions, configurations and specifications
  run
  CypressBootstrapRowanBlock 
    value: 'RowanKernel'
    value: #('Rowan-Kernel' 'Rowan-Url-Core').
%
  commit

  run
  CypressBootstrapRowanBlock 
    value: 'RowanTools'
    value: #('Rowan-Definitions' 'Rowan-Configurations' 'Rowan-Specifications' 
	).	"Rowan Definitions, Configurations and Specifications"
%
  commit

  run
  CypressBootstrapRowanBlock 
    value: 'RowanKernel'
    value: #('GemStone-Interactions-Core' 'Rowan-Url-3215' 
      'Rowan-Core' 'Rowan-GemStone-Core' 'Rowan-Cypress-Core' 'Rowan-Core-Definitions-Extensions'
      'Rowan-Services-Core' 'Rowan-Url-Extensions'
      'Rowan-Services-Extensions'
	).	"Populate with Rowan implementation classes"
%
  commit

  run
  CypressBootstrapRowanBlock 
    value: 'RowanTools'
    value: #('Rowan-Tools-Core' 'Rowan-Tools-Deprecated' 'Rowan-Definitions-Deprecated'
	'Rowan-Cypress-Definitions' 'Rowan-GemStone-Definitions' 
	'Rowan-GemStone-Specifications'
	).	"Rowan Tools"
%
  commit

  run
  CypressBootstrapRowanBlock
    value: 'UserGlobals'
    value: #( 'Rowan-JadeServer').           "install JadeServer classes"
%
  commit

  run
  CypressBootstrapRowanBlock 
    value: 'RowanKernel'
    value: #('Rowan-Tools-Extensions' 'Rowan-Deprecated' 'Rowan-Tests' 'Rowan-Services-Tests'	
	).	"Tool extension methods for RowanKernel classes "
%
  commit

  run
  CypressBootstrapRowanBlock 
    value: 'RowanLoader'
    value: #('Rowan-GemStone-Loader'
	).		"GemStone Rowan loader classes"
%
  commit

  run
  CypressBootstrapRowanBlock 
    value: 'RowanKernel'
    value: #('Rowan-GemStone-Loader-Extensions'
	).	"Extension methods in non-loader classes"
%
  commit

  run
  CypressBootstrapRowanBlock 
    value: 'Globals'
    value: #('GemStone-Interactions-Kernel' 'Rowan-GemStone-Kernel' 'Rowan-Cypress-Kernel' 
      'Rowan-Tools-Kernel' 
      'Rowan-GemStone-3215'
	).		"Extension methods for GemStone kernel classes"
%
  commit


  run
  UserGlobals removeKey: #CypressBootstrapRowanBlock.
%
  commit

# Install FileSystem, Rowan, Cypress, STON, and Tonel using Rowan to adopt the existing classes and extension
#  methods into the correct package structure
  run
 	| projectSetDefinition gitRepoPath packageCreateTool projectLoadTool loadedProjectInfo |
	projectSetDefinition := RwProjectSetDefinition new.
	loadedProjectInfo := Dictionary new.
	gitRepoPath := '$ROWAN_PROJECTS_HOME/Rowan'.
	{
		{'file:$ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/projects/filesystem/rowan/specs/FileSystemGs.ston'. 'Default'}.
		{'file:$ROWAN_PROJECTS_HOME/Rowan/rowan/specs/Rowan.ston'. 'Load'}.
		{'file:$ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/projects/cypress/specs/Cypress_SystemUser.ston'. 'Default'}.
		{'file:$ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/projects/ston/specs/STON_SystemUser.ston'. 'Bootstrap'}.
		{'file:$ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/projects/tonel/specs/Tonel_SystemUser.ston'. 'Bootstrap'}.
	} 
	do: [:ar |
		"Read project and packages from disk, creating a projectSetDefinition with all 5 projects"
		| specification specUrl readTool |
		specUrl := ar at: 1.
		specification := RwSpecification fromUrl: specUrl.
		specification
			repositoryRootPath: gitRepoPath;
			repositoryUrl: 'cypress:' , gitRepoPath , '/' , specification repoPath , '/';
			register. "Create each of the loaded projects"
		readTool := Rowan projectTools read.
		ar size = 1
			ifTrue: [
				| theProjectSetDefinition |
				theProjectSetDefinition := readTool readProjectSetForProjectNamed: specification specName.
				theProjectSetDefinition
					do: [:projectDefinition |
						projectSetDefinition addProject: projectDefinition ].
				loadedProjectInfo at: specification specName put: ((theProjectSetDefinition properties at: 'loadedProjectInfo') at: specification specName) ]
			ifFalse: [
      	| configName groupNames theProjectSetDefinition |
				configName := ar at: 2.
				groupNames := specification defaultGroupNames.
        theProjectSetDefinition := readTool
					readProjectSetForProjectNamed: specification specName 
						withConfigurations: { configName } 
						groupNames: groupNames.
				loadedProjectInfo at: specification specName put: ((theProjectSetDefinition properties at: 'loadedProjectInfo') at: specification specName).
				theProjectSetDefinition
            	do: [:projectDefinition |
              	projectSetDefinition addProject: projectDefinition ] ] ].

	loadedProjectInfo keysAndValuesDo: [:projectName :projectInfo |
			(#('FileSystemGs' 'Rowan') includes: projectName)
				ifTrue: [ 
					"install the packageMapSpecs for this load into the specification prior to the load"
					| projectDefinition spec gemstoneSpec thePackageMapSpecs |
					projectDefinition := projectSetDefinition projectNamed: projectName ifAbsent: [].
					spec := projectDefinition specification.
					thePackageMapSpecs := projectInfo at:  'packageMapSpecs' .
					gemstoneSpec := spec platformSpec at: 'gemstone'.
					(thePackageMapSpecs at: #defaultSymbolDictName otherwise: nil) 
						ifNotNil: [:name | gemstoneSpec defaultSymbolDictName: name ].
					(thePackageMapSpecs at: #defaultUseSessionMethodsForExtensions otherwise: nil) 
						ifNotNil: [:boolean | 
							gemstoneSpec defaultUseSessionMethodsForExtensions: boolean  ].
					(thePackageMapSpecs at: #packageNameToPlatformPropertiesMap otherwise: nil) 
						ifNotNil: [:map | gemstoneSpec packageNameToPlatformPropertiesMap: map] ] ].

	Rowan image newOrExistingSymbolDictionaryNamed: 'RowanKernel'.
	Rowan image newOrExistingSymbolDictionaryNamed: 'RowanLoader'.
	Rowan image newOrExistingSymbolDictionaryNamed: 'RowanTools'.

	packageCreateTool := Rowan packageTools create.
	projectSetDefinition projects 
		do: [:projectDefinition |
			"The loaded project was created by the earlier #register,
				traverse the package definitions and create loaded packages for each"
			| specification projectName |
			projectName := projectDefinition name.
			specification := (Rowan image loadedProjectNamed: projectName) specification.
			projectDefinition packageNames
				do: [:packageName |
					packageCreateTool createLoadedPackageNamed: packageName inProjectNamed: projectName ] ].

	"Adopt the project set definition"
	Rowan projectTools adopt adoptProjectSetDefinition: projectSetDefinition.

	projectLoadTool := Rowan projectTools load.

	projectSetDefinition projects 
		do: [:projectDefinition |
			"make sure that the loaded SHA is set for each project"
			projectLoadTool specification: projectDefinition specification.
			projectDefinition specification updateLoadedCommitIdForTool: projectLoadTool.
			projectDefinition name = 'Rowan'
				ifTrue: [
					(loadedProjectInfo at: projectDefinition name ifAbsent: [])
						ifNotNil: [:map |
							projectDefinition specification imageSpec
								loadedConfigurationNames: (map at: 'loadedConfigurationNames');
								loadedGroupNames: (map at: 'loadedGroupNames') ] ] ].

	projectSetDefinition deriveLoadedThings do: [:loadedProject |
		"mark projects and packages not dirty"
		loadedProject markNotDirty.
		loadedProject loadedPackages valuesDo: [:loadedPackage | loadedPackage markNotDirty ] ].

	projectSetDefinition projects
		do: [:projectDefinition |
			| audit projectName |
			projectName := projectDefinition name.
			GsFile gciLogServer: '---Auditing project: ', projectName printString.
			audit := Rowan projectTools audit auditForProjectNamed: projectName.
			GsFile gciLogServer: '	-- audit finished '. 
			audit isEmpty ifFalse: [ self error: 'Post load Rowan audit failed for project ', projectName printString ] ]
	
%
  commit

# Install Rowan class in Published symbol dict, so it is availailable to all users
# 
   run
  | rowanAssoc |
  rowanAssoc := RowanKernel associationAt: #Rowan.
  Published add: rowanAssoc.
%
  commit

  logout
  set u DataCurator p swordfish
  login

# set rowanCompile to true 
#
run
UserGlobals at: #rowanCompile put: true.
System commit
%
  commit
  logout
