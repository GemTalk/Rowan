! Package: Cypress-Definitions
! Written: 2013-07-23T16:34:40.55085396766663-07:00


! Remove existing behavior from package Cypress-Definitions
!!!! This can be cleaned up when some package functionality is moved to the base system.

doit
| packageName |
packageName := 'Cypress-Definitions'.
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
	subclass: 'CypressLoaderMissingClasses'
	instVarNames: #( requirementsMap )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Definitions';
		comment: '';
		immediateInvariant.
%

doit
(Error
	subclass: 'CypressError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Definitions';
		comment: '';
		immediateInvariant.
%

doit
(Error
	subclass: 'CypressLoaderError'
	instVarNames: #( patchOperation exception )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Definitions';
		comment: 'CypressLoaderError is used to report a failure applying a specific CypressPatchOperation.
The CypressLoader made a first attempt to apply the Patch Operation and reported a 
CypressLoaderErrorNotification, set aside the Patch Operation, and has retried it after applying
all other Patch Operations.


Instance Variables:

patchOperation:		the CypressPatchOperation that could not be applied.
exception:			the Error which occurred while trying to apply the Patch Operation.
';
		immediateInvariant.
%

doit
(Notification
	subclass: 'CypressLoaderErrorNotification'
	instVarNames: #( patchOperation exception )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Definitions';
		comment: 'CypressLoaderErrorNotification is used to notify a consumer of the CypressLoader that a particular CypressPatchOperation failed.
As a Notification, it resumes by default, logging the error to the Transcript.


Instance Variables:

patchOperation:		the CypressPatchOperation that could not be applied.
exception:			the Error which occurred while trying to apply the Patch Operation.
';
		immediateInvariant.
%

doit
(Object
	subclass: 'CypressObject'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Definitions';
		comment: '';
		immediateInvariant.
%

doit
(CypressObject
	subclass: 'CypressSnapshot'
	instVarNames: #( definitions )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Definitions';
		comment: '';
		immediateInvariant.
%

doit
(CypressObject
	subclass: 'CypressPatchOperation'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Definitions';
		comment: '';
		immediateInvariant.
%

doit
(CypressPatchOperation
	subclass: 'CypressRemoval'
	instVarNames: #( definition )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Definitions';
		comment: '';
		immediateInvariant.
%

doit
(CypressPatchOperation
	subclass: 'CypressModification'
	instVarNames: #( obsoletion modification )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Definitions';
		comment: '';
		immediateInvariant.
%

doit
(CypressPatchOperation
	subclass: 'CypressAddition'
	instVarNames: #( definition )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Definitions';
		comment: '';
		immediateInvariant.
%

doit
(CypressObject
	subclass: 'CypressPatch'
	instVarNames: #( operations )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Definitions';
		comment: '';
		immediateInvariant.
%

doit
(CypressObject
	subclass: 'CypressPackageDefinition'
	instVarNames: #( name )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Definitions';
		comment: '';
		immediateInvariant.
%

doit
(CypressObject
	subclass: 'CypressDependencySorter'
	instVarNames: #( required provided orderedItems )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Definitions';
		comment: '';
		immediateInvariant.
%

doit
(CypressObject
	subclass: 'CypressDefinitionIndex'
	instVarNames: #( definitionMap )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Definitions';
		comment: '';
		immediateInvariant.
%

doit
(CypressObject
	subclass: 'CypressDefinition'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Definitions';
		comment: '';
		immediateInvariant.
%

doit
(CypressDefinition
	subclass: 'CypressMethodDefinition'
	instVarNames: #( classIsMeta source category selector className )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Definitions';
		comment: '';
		immediateInvariant.
%

doit
(CypressDefinition
	subclass: 'CypressClassDefinition'
	instVarNames: #( name superclassName category comment instVarNames classInstVarNames classVarNames poolDictionaryNames )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Definitions';
		comment: '';
		immediateInvariant.
%

doit
(CypressObject
	subclass: 'CypressLoader'
	instVarNames: #( additions removals unloadable provisions errors methodAdditions requirements exceptionClass )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Definitions';
		comment: '';
		immediateInvariant.
%

! Class Implementation for CypressLoaderMissingClasses

! ------------------- Class methods for CypressLoaderMissingClasses

category: 'instance creation'
set compile_env: 0
classmethod: CypressLoaderMissingClasses
missingRequirementsMap: aDictionary
	"Answer an instance of the receiver initialized on the specified
	 missing requirements. aDictionary maps prerequisite names to
	 a collection of dependent definitions."

	^self new
		initializeRequirementsMap: aDictionary;
		yourself
%

! ------------------- Instance methods for CypressLoaderMissingClasses

category: 'initializing - private'
set compile_env: 0
method: CypressLoaderMissingClasses
initialize

	super initialize.
	gsResumable := true
%

category: 'initializing - private'
set compile_env: 0
method: CypressLoaderMissingClasses
initializeMessageText

	messageText := String streamContents: 
					[:stream |
					stream nextPutAll: 'Missing classes:'.
					self requirementsMap keysAndValuesDo: 
							[:className :definitions |
							stream
								space;
								nextPutAll: className printString , '(' , definitions size printString
											, ')']]
%

category: 'initializing - private'
set compile_env: 0
method: CypressLoaderMissingClasses
initializeRequirementsMap: aDictionary

	self
		requirementsMap: aDictionary;
		initializeMessageText.
%

category: 'accessing'
set compile_env: 0
method: CypressLoaderMissingClasses
requirementsMap
	"The requirements map is a Dictionary mapping missing class
	 names to a collection of dependent definitions."

   ^requirementsMap
%

category: 'updating'
set compile_env: 0
method: CypressLoaderMissingClasses
requirementsMap: aDictionary
	"The requirements map is a Dictionary mapping missing class
	 names to a collection of dependent definitions."

	requirementsMap := aDictionary
%

! Class Implementation for CypressError

! Class Implementation for CypressLoaderError

! ------------------- Class methods for CypressLoaderError

category: 'instance creation'
set compile_env: 0
classmethod: CypressLoaderError
patchOperation: aPatchOperation exception: anException

	^self new
		initializePatchOperation: aPatchOperation exception: anException;
		yourself
%

! ------------------- Instance methods for CypressLoaderError

category: 'accessing'
set compile_env: 0
method: CypressLoaderError
exception
	"Answer the original exception raised when applying the Patch Operation."

	^exception
%

category: 'updating'
set compile_env: 0
method: CypressLoaderError
exception: anException
	"Assign the original exception raised when applying the Patch Operation."

	exception := anException
%

category: 'initializing - private'
set compile_env: 0
method: CypressLoaderError
initialize

	super initialize.
	gsResumable := true
%

category: 'initializing - private'
set compile_env: 0
method: CypressLoaderError
initializeMessageText

	messageText := String streamContents: 
					[:stream |
					stream
						nextPutAll: self patchOperation printString;
						nextPutAll: ' failed because ';
						nextPutAll: self exception printString]
%

category: 'initializing - private'
set compile_env: 0
method: CypressLoaderError
initializePatchOperation: aPatchOperation exception: anException

	self
		patchOperation: aPatchOperation;
		exception: anException;
		initializeMessageText
%

category: 'handling'
set compile_env: 0
method: CypressLoaderError
logNotification: aString

	GsFile gciLogServer: aString.
	Transcript cr; nextPutAll: aString.
%

category: 'accessing'
set compile_env: 0
method: CypressLoaderError
patchOperation
	"Answer the Patch Operation that could not be applied."

	^patchOperation
%

category: 'updating'
set compile_env: 0
method: CypressLoaderError
patchOperation: aCypressPatchOperation
	"Assign the Patch Operation that could not be applied."

	patchOperation := aCypressPatchOperation
%

! Class Implementation for CypressLoaderErrorNotification

! ------------------- Class methods for CypressLoaderErrorNotification

category: 'instance creation'
set compile_env: 0
classmethod: CypressLoaderErrorNotification
patchOperation: aPatchOperation exception: anException

	^self new
		initializePatchOperation: aPatchOperation exception: anException;
		yourself
%

! ------------------- Instance methods for CypressLoaderErrorNotification

category: 'handling'
set compile_env: 0
method: CypressLoaderErrorNotification
defaultAction
	"Log the notification to the GCI log and the Transcript, then resume."

	self logNotification: 'Notice: ' , self asString.
	^super defaultAction
%

category: 'accessing'
set compile_env: 0
method: CypressLoaderErrorNotification
exception
	"Answer the original exception raised when applying the Patch Operation."

	^exception
%

category: 'updating'
set compile_env: 0
method: CypressLoaderErrorNotification
exception: anException
	"Assign the original exception raised when applying the Patch Operation."

	exception := anException
%

category: 'initializing - private'
set compile_env: 0
method: CypressLoaderErrorNotification
initializeMessageText

	messageText := String streamContents: 
					[:stream |
					stream
						nextPutAll: self patchOperation printString;
						nextPutAll: ' failed because ';
						nextPutAll: self exception printString]
%

category: 'initializing - private'
set compile_env: 0
method: CypressLoaderErrorNotification
initializePatchOperation: aPatchOperation exception: anException

	self
		patchOperation: aPatchOperation;
		exception: anException;
		initializeMessageText
%

category: 'handling'
set compile_env: 0
method: CypressLoaderErrorNotification
logNotification: aString

	GsFile gciLogServer: aString.
	Transcript cr; nextPutAll: aString.
%

category: 'accessing'
set compile_env: 0
method: CypressLoaderErrorNotification
patchOperation
	"Answer the Patch Operation that could not be applied."

	^patchOperation
%

category: 'updating'
set compile_env: 0
method: CypressLoaderErrorNotification
patchOperation: aCypressPatchOperation
	"Assign the Patch Operation that could not be applied."

	patchOperation := aCypressPatchOperation
%

! Class Implementation for CypressObject

! ------------------- Class methods for CypressObject

category: 'converting'
set compile_env: 0
classmethod: CypressObject
normalizeLineEndings: aString
	"Answer a copy of aString with the line endings normalized to
	 correspond to the current platform, regardless of how they were
	 saved. For example, Squeak uses CR and would normalize with
	 #withSqueakLineEndings, for example.

	 GemStone Smalltalk uses the Unix line ending of LF."

	^aString withUnixLineEndings.
%

! ------------------- Instance methods for CypressObject

category: 'sorting'
set compile_env: 0
method: CypressObject
addClasses: subs to: order fromRelevantClasses: classSet organizedBy: org

	1 to: subs size
		do: 
			[:i |
			| assoc class |
			class := subs at: i.
			(classSet includesIdentical: class) ifTrue: [order add: class].
			assoc := org associationAt: class otherwise: nil.
			assoc ~~ nil
				ifTrue: 
					[self
						addClasses: assoc value
						to: order
						fromRelevantClasses: classSet
						organizedBy: org]]
%

category: 'accessing'
set compile_env: 0
method: CypressObject
allClasses

	^System myUserProfile symbolList allSatisfying: [:each | each isBehavior]
%

category: 'accessing'
set compile_env: 0
method: CypressObject
classesInPackageNamed: aString

	^(System myUserProfile symbolList allSatisfying: 
			[:each |
			each isBehavior and: [each category = aString or: [each category beginsWith: aString, '-']]])
		sortAscending: #('name')
%

category: 'sorting'
set compile_env: 0
method: CypressObject
determineClassHierarchicalOrder: someClasses
	"Returns an ordered collection of the specified classes such that
	 hierarchical dependencies come first."

	| org order classSet block |
	org := Dictionary new.
	org at: #nil put: ClassSet new.
	classSet := ClassSet new.
	someClasses do: 
			[:each |
			| sub |
			sub := each.
			sub isBehavior
				ifTrue: 
					[| superCls |
					classSet add: sub.
					
					[superCls := sub superClass.
					superCls ~~ nil] whileTrue: 
								[| assoc |
								assoc := org associationAt: superCls otherwise: nil.
								assoc
									ifNil: 
										[assoc := Association newWithKey: superCls value: ClassSet new.
										org add: assoc].
								assoc value add: sub.
								sub := superCls].
					(org at: #nil) add: sub]].

	"Order the subclass sets and weed out unwanted classes."
	order := Array new.
	self
		addClasses: (org at: #nil)
		to: order
		fromRelevantClasses: classSet
		organizedBy: org.
	^order
%

category: 'initializing'
set compile_env: 0
method: CypressObject
initialize
	"Placeholder: #initialize is not defined by Object in GemStone Smalltalk."
%

category: 'converting'
set compile_env: 0
method: CypressObject
normalizeLineEndings: aString
	"Answer a copy of aString with the line endings normalized to
	 correspond to the current platform, regardless of how they were
	 saved. For example, Squeak uses CR and would normalize with
	 #withSqueakLineEndings, for example."

	^self class normalizeLineEndings: aString.
%

category: 'printing'
set compile_env: 0
method: CypressObject
printDetailsOn: aStream
%

category: 'printing'
set compile_env: 0
method: CypressObject
printOn: aStream

	| className |
	className := self class name.
	aStream
		nextPutAll: (className first isVowel ifTrue:[ 'an ' ] ifFalse:[ 'a ' ]);
		nextPutAll: className;
		nextPutAll: '('.
	self printDetailsOn: aStream.
	aStream nextPutAll: ')'.
%

category: 'accessing'
set compile_env: 0
method: CypressObject
resolveGlobalNamed: aString

	^self resolveGlobalNamed: aString
		or: [CypressError signal: 'Could not resolve global named ' , aString printString]
%

category: 'accessing'
set compile_env: 0
method: CypressObject
resolveGlobalNamed: aString or: aBlock

	^((System myUserProfile resolveSymbol: aString) ifNil: [^aBlock value])
		value
%

category: 'converting'
set compile_env: 0
method: CypressObject
stringForVariables: variableList

	^String streamContents: 
			[:stream |
			variableList do: [:each | stream nextPutAll: each]
				separatedBy: [stream space]]
%

! Class Implementation for CypressSnapshot

! ------------------- Class methods for CypressSnapshot

category: 'instance creation'
set compile_env: 0
classmethod: CypressSnapshot
definitions: aDefinitions

	^(self new) definitions: aDefinitions
%

! ------------------- Instance methods for CypressSnapshot

category: 'comparing'
set compile_env: 0
method: CypressSnapshot
= other
	^ definitions asArray = other definitions asArray
%

category: 'enumerating'
set compile_env: 0
method: CypressSnapshot
classDefinitions: classBlock methodDefinitions: methodBlock

	self definitions do: [:definition |
		definition classDefinition: classBlock methodDefinition: methodBlock]
%

category: 'accessing'
set compile_env: 0
method: CypressSnapshot
definitions

	^definitions
%

category: 'accessing'
set compile_env: 0
method: CypressSnapshot
definitions: aDefinitions

	definitions := aDefinitions
%

category: 'patching'
set compile_env: 0
method: CypressSnapshot
patchRelativeToBase: aSnapshot
	^ CypressPatch fromBase: aSnapshot toTarget: self
%

category: 'loading'
set compile_env: 0
method: CypressSnapshot
updatePackage: aPackage
	"Answer the loader used to apply the update."

	^CypressLoader updatePackage: aPackage withSnapshot: self
%

! Class Implementation for CypressPatchOperation

! ------------------- Instance methods for CypressPatchOperation

category: 'comparing'
set compile_env: 0
method: CypressPatchOperation
= aPatchOperation
	^aPatchOperation isKindOf: self class
%

category: 'applying'
set compile_env: 0
method: CypressPatchOperation
applyTo: aCypressLoader

	self subclassResponsibility
%

category: 'accessing'
set compile_env: 0
method: CypressPatchOperation
description

	self subclassResponsibility
%

category: 'comparing'
set compile_env: 0
method: CypressPatchOperation
hash
    ^ self description hash
%

category: 'loading'
set compile_env: 0
method: CypressPatchOperation
loadClassDefinition

	self subclassResponsibility
%

category: 'loading'
set compile_env: 0
method: CypressPatchOperation
loadMethodDefinition
	self subclassResponsibility
%

category: 'loading'
set compile_env: 0
method: CypressPatchOperation
postLoadDefinition
	self subclassResponsibility
%

category: 'printing'
set compile_env: 0
method: CypressPatchOperation
printDetailsOn: aStream

	aStream nextPutAll: self description.
%

category: 'dependency'
set compile_env: 0
method: CypressPatchOperation
provisions
	"Answer list of global names defined by this definition"

	self subclassResponsibility
%

category: 'dependency'
set compile_env: 0
method: CypressPatchOperation
requirements
	"Answer list of global names required by this definition"

	self subclassResponsibility
%

category: 'loading'
set compile_env: 0
method: CypressPatchOperation
unloadDefinition

	CypressError signal: 'inappropriate to send #unloadDefinition to an addition or modification operation'
%

! Class Implementation for CypressRemoval

! ------------------- Class methods for CypressRemoval

category: 'instance creation'
set compile_env: 0
classmethod: CypressRemoval
of: aDefinition
	^ self new definition: aDefinition
%

! ------------------- Instance methods for CypressRemoval

category: 'comparing'
set compile_env: 0
method: CypressRemoval
= aPatchOperation
	^(super = aPatchOperation) and: [self definition = aPatchOperation definition]
%

category: 'applying'
set compile_env: 0
method: CypressRemoval
applyTo: aCypressLoader

	aCypressLoader applyRemoval: self
%

category: 'accessing'
set compile_env: 0
method: CypressRemoval
definition

	^definition
%

category: 'initialization'
set compile_env: 0
method: CypressRemoval
definition: aDefinition

	definition := aDefinition
%

category: 'accessing'
set compile_env: 0
method: CypressRemoval
description

	^'remove: ', self definition printString
%

category: 'loading'
set compile_env: 0
method: CypressRemoval
loadClassDefinition
	
	CypressError signal: 'inappropriate to send #loadClassDefinition to a removal operation'
%

category: 'loading'
set compile_env: 0
method: CypressRemoval
loadMethodDefinition
	
	CypressError signal: 'inappropriate to send #loadMethodDefinition to a removal operation'
%

category: 'loading'
set compile_env: 0
method: CypressRemoval
postLoadDefinition
	
	CypressError signal: 'inappropriate to send #postLoadDefinition to a removal operation'
%

category: 'dependency'
set compile_env: 0
method: CypressRemoval
provisions
	"Answer list of global names defined by this definition"

	^#()
%

category: 'dependency'
set compile_env: 0
method: CypressRemoval
requirements
	"Answer list of global names required by this definition"

	^#()
%

category: 'loading'
set compile_env: 0
method: CypressRemoval
unloadDefinition

	self definition unloadDefinition.
%

! Class Implementation for CypressModification

! ------------------- Class methods for CypressModification

category: 'instance creation'
set compile_env: 0
classmethod: CypressModification
of: base to: target
	^ self new base: base target: target
%

! ------------------- Instance methods for CypressModification

category: 'initialization'
set compile_env: 0
method: CypressModification
= aPatchOperation
	^(super = aPatchOperation) and: [self obsoletion = aPatchOperation obsoletion and: [ self modification = aPatchOperation modification]]
%

category: 'applying'
set compile_env: 0
method: CypressModification
applyTo: aCypressLoader

	aCypressLoader applyModification: self
%

category: 'initialization'
set compile_env: 0
method: CypressModification
base: base target: target

	obsoletion := base.
	modification := target.
%

category: 'accessing'
set compile_env: 0
method: CypressModification
description
    ^ 'modify from: ' , self obsoletion printString , ' to: ' , self modification printString
%

category: 'loading'
set compile_env: 0
method: CypressModification
loadClassDefinition

	self modification loadClassDefinition.
%

category: 'loading'
set compile_env: 0
method: CypressModification
loadMethodDefinition

	self modification loadMethodDefinition.
%

category: 'accessing'
set compile_env: 0
method: CypressModification
modification

	^modification
%

category: 'accessing'
set compile_env: 0
method: CypressModification
obsoletion

	^obsoletion
%

category: 'loading'
set compile_env: 0
method: CypressModification
postLoadDefinition
	self modification postLoadOver: self obsoletion
%

category: 'dependency'
set compile_env: 0
method: CypressModification
provisions
	"Answer list of global names defined by this definition"

	^self modification provisions
%

category: 'dependency'
set compile_env: 0
method: CypressModification
requirements
	"Answer list of global names required by this definition"

	^self modification requirements
%

! Class Implementation for CypressAddition

! ------------------- Class methods for CypressAddition

category: 'instance creation'
set compile_env: 0
classmethod: CypressAddition
of: aDefinition
	^ self new definition: aDefinition
%

! ------------------- Instance methods for CypressAddition

category: 'comparing'
set compile_env: 0
method: CypressAddition
= aPatchOperation
	^(super = aPatchOperation) and: [self definition = aPatchOperation definition]
%

category: 'applying'
set compile_env: 0
method: CypressAddition
applyTo: aCypressLoader

	aCypressLoader applyAddition: self
%

category: 'accessing'
set compile_env: 0
method: CypressAddition
definition

	^definition
%

category: 'initialization'
set compile_env: 0
method: CypressAddition
definition: aDefinition

	definition := aDefinition
%

category: 'accessing'
set compile_env: 0
method: CypressAddition
description
    ^ 'add: ' , self definition printString
%

category: 'loading'
set compile_env: 0
method: CypressAddition
loadClassDefinition

	self definition loadClassDefinition.
%

category: 'loading'
set compile_env: 0
method: CypressAddition
loadMethodDefinition

	self definition loadMethodDefinition.
%

category: 'loading'
set compile_env: 0
method: CypressAddition
postLoadDefinition
	self definition postLoadOver: nil
%

category: 'dependency'
set compile_env: 0
method: CypressAddition
provisions
	"Answer list of global names defined by this definition"

	^self definition provisions
%

category: 'dependency'
set compile_env: 0
method: CypressAddition
requirements
	"Answer list of global names required by this definition"

	^self definition requirements
%

! Class Implementation for CypressPatch

! ------------------- Class methods for CypressPatch

category: 'instance creation'
set compile_env: 0
classmethod: CypressPatch
fromBase: baseSnapshot toTarget: targetSnapshot
	^ (self new)
		fromBase: baseSnapshot
		toTarget: targetSnapshot
%

! ------------------- Instance methods for CypressPatch

category: 'applying'
set compile_env: 0
method: CypressPatch
applyTo: aCypressLoader
	operations do: [:ea | ea applyTo: aCypressLoader].
%

category: 'initialization'
set compile_env: 0
method: CypressPatch
fromBase: baseSnapshot toTarget: targetSnapshot
	| base target |	
	operations := OrderedCollection new.
	base := CypressDefinitionIndex definitions: baseSnapshot definitions.
	target := CypressDefinitionIndex definitions: targetSnapshot definitions.
	
	target definitions do:
		[:t |
		base
			definitionLike: t
			ifPresent: [:b | (b isSameRevisionAs: t) ifFalse: [operations add: (CypressModification of: b to: t)]]
			ifAbsent: [operations add: (CypressAddition of: t)]].
		
	base definitions do:
		[:b |
		target
			definitionLike: b
			ifPresent: [:t | ]
			ifAbsent: [operations add: (CypressRemoval of: b)]]
%

category: 'accessing'
set compile_env: 0
method: CypressPatch
operations

	^operations
%

! Class Implementation for CypressPackageDefinition

! ------------------- Class methods for CypressPackageDefinition

category: 'unknown'
set compile_env: 0
classmethod: CypressPackageDefinition
fileOutsForPackagesNamed: someNames

	^someNames inject: Dictionary new
		into: 
			[:result :each |
			result
				at: each
					put: (String streamContents: 
								[:stream |
								(CypressPackageStructure fromPackage: (self named: each))
									fileOutOn: stream]);
				yourself]
%

category: 'instance creation'
set compile_env: 0
classmethod: CypressPackageDefinition
named: aString

	^self new
		name: aString;
		yourself.
%

! ------------------- Instance methods for CypressPackageDefinition

category: 'comparing'
set compile_env: 0
method: CypressPackageDefinition
= other
	^ other species = self species and: [other name sameAs: name]
%

category: 'snapshotting'
set compile_env: 0
method: CypressPackageDefinition
addClass: aClass toDefinitions: definitions

	definitions add: aClass asCypressClassDefinition
%

category: 'snapshotting'
set compile_env: 0
method: CypressPackageDefinition
addExtensionMethodsFromClass: aClass toMap: classMap

	| defs map |
	defs := classMap at: aClass theNonMetaClass ifAbsent: [OrderedCollection new.].
	map := Dictionary new.
	aClass categorysDo: 
			[:category :selectors |
			(category asLowercase beginsWith: '*' , self name asLowercase)
				ifTrue: [map at: category put: selectors asSortedCollection]].
	map keys asSortedCollection do: 
			[:category |
			(map at: category)
				do: [:selector | defs add: (aClass compiledMethodAt: selector) asCypressMethodDefinition]].
	defs notEmpty ifTrue: [classMap at: aClass theNonMetaClass put: defs]
%

category: 'snapshotting'
set compile_env: 0
method: CypressPackageDefinition
addMethodsFromClass: aClass toDefinitions: definitions
	"Add only those methods which are not extensions from other packages."

	((aClass methodDictionary reject: [:each | each category first = $*])
		asSortedCollection: [:a :b | a selector <= b selector])
			do: [:method | definitions add: method asCypressMethodDefinition]
%

category: 'accessing'
set compile_env: 0
method: CypressPackageDefinition
classes

	^self classesInPackageNamed: self name
%

category: 'accessing'
set compile_env: 0
method: CypressPackageDefinition
name
	^ name
%

category: 'accessing'
set compile_env: 0
method: CypressPackageDefinition
name: aString
	name := aString
%

category: 'printing'
set compile_env: 0
method: CypressPackageDefinition
printDetailsOn: aStream

	aStream nextPutAll: name
%

category: 'snapshotting'
set compile_env: 0
method: CypressPackageDefinition
snapshot

	| classDefinitions methodDefinitions map classMap |
	classDefinitions := OrderedCollection new.
	methodDefinitions := OrderedCollection new.
	(self determineClassHierarchicalOrder: self classes) do: 
			[:cls |
			self
				addClass: cls toDefinitions: classDefinitions;
				addMethodsFromClass: cls toDefinitions: methodDefinitions;
				addMethodsFromClass: cls class toDefinitions: methodDefinitions].
	classMap := Dictionary new.
	self allClasses do: 
			[:each |
			self
				addExtensionMethodsFromClass: each toMap: classMap;
				addExtensionMethodsFromClass: each class toMap: classMap].
	(self determineClassHierarchicalOrder: classMap keys)
		do: [:aClass | methodDefinitions addAll: (classMap at: aClass)].
	^CypressSnapshot definitions: classDefinitions, methodDefinitions
%

! Class Implementation for CypressDependencySorter

! ------------------- Instance methods for CypressDependencySorter

category: 'building'
set compile_env: 0
method: CypressDependencySorter
add: aPatchOperation
	| requirements |
	requirements := self unresolvedRequirementsFor: aPatchOperation.
	requirements isEmpty
		ifTrue: [self addToOrder: aPatchOperation]
		ifFalse: [self addRequirements: requirements for: aPatchOperation].
	^ aPatchOperation
%

category: 'building'
set compile_env: 0
method: CypressDependencySorter
addAll: aCollection
	aCollection do: [:aPatchOperation | self add: aPatchOperation ]
%

category: 'private'
set compile_env: 0
method: CypressDependencySorter
addExternalProvisions: aCollection
	(aCollection intersection: self externalRequirements)
		do: [:globalName | self addProvision: globalName]
%

category: 'private'
set compile_env: 0
method: CypressDependencySorter
addProvision: aGlobalName
	| newlySatisfied |
	self provided add: aGlobalName.
	newlySatisfied := self required removeKey: aGlobalName ifAbsent: [#()].
	self addAll: newlySatisfied.
%

category: 'private'
set compile_env: 0
method: CypressDependencySorter
addRequirement: globalName for: aPatchOperation
	(self itemsRequiring: globalName) add: aPatchOperation
%

category: 'private'
set compile_env: 0
method: CypressDependencySorter
addRequirements: aCollection for: aPatchOperation
	aCollection do: [:globalName | self addRequirement: globalName for: aPatchOperation]
%

category: 'private'
set compile_env: 0
method: CypressDependencySorter
addToOrder: aPatchOperation
	self orderedItems add: aPatchOperation.
	aPatchOperation provisions do: [:globalName | self addProvision: globalName ].
%

category: 'accessing'
set compile_env: 0
method: CypressDependencySorter
externalRequirements
	| unloaded providedByUnloaded |
	unloaded := self itemsWithMissingRequirements.
	providedByUnloaded := (unloaded gather: [:e | e provisions]) asSet.
	^ self required keys reject: [:globalName | providedByUnloaded includes: globalName ]
%

category: 'private'
set compile_env: 0
method: CypressDependencySorter
itemsRequiring: globalName
	^ self required at: globalName ifAbsentPut: [Set new]
%

category: 'accessing'
set compile_env: 0
method: CypressDependencySorter
itemsWithMissingRequirements
	| patchOperations |
	patchOperations := Set new.
	self required values do: [:aSetOfPatchOperations | patchOperations addAll: aSetOfPatchOperations ].
	^ patchOperations
%

category: 'accessing'
set compile_env: 0
method: CypressDependencySorter
orderedItems
	"ordered list of patch operations"

	orderedItems ifNil: [ orderedItems := OrderedCollection new ].
	^orderedItems
%

category: 'accessing'
set compile_env: 0
method: CypressDependencySorter
provided
	"set of global names provided by definitions already loaded"

	provided ifNil: [ provided := Set new ].
	^provided
%

category: 'accessing'
set compile_env: 0
method: CypressDependencySorter
required
	"dictionary of required global name mapped to list of definitions that require the global"

	required ifNil: [ required := Dictionary new ].
	^required
%

category: 'private'
set compile_env: 0
method: CypressDependencySorter
unresolvedRequirementsFor: aPatchOperation
	"Answer a list of global names that are required by <aPatchOperation>, but not 
	 provided by patchOperations that have already been processed"

	^ aPatchOperation requirements difference: self provided
%

! Class Implementation for CypressDefinitionIndex

! ------------------- Class methods for CypressDefinitionIndex

category: 'instance creation'
set compile_env: 0
classmethod: CypressDefinitionIndex
definitions: aCollection
	^ self new addAll: aCollection
%

! ------------------- Instance methods for CypressDefinitionIndex

category: 'adding'
set compile_env: 0
method: CypressDefinitionIndex
add: aDefinition
	^ self definitionMap at: aDefinition description put: aDefinition
%

category: 'adding'
set compile_env: 0
method: CypressDefinitionIndex
addAll: aCollection
	aCollection do: [:ea | self add: ea]
%

category: 'querying'
set compile_env: 0
method: CypressDefinitionIndex
definitionLike: aDefinition ifPresent: foundBlock ifAbsent: errorBlock
	| definition |
	definition := self definitionMap at: aDefinition description ifAbsent: [].
	^ definition
		ifNil: errorBlock
		ifNotNil: [foundBlock value: definition]
%

category: 'accessing'
set compile_env: 0
method: CypressDefinitionIndex
definitionMap
	definitionMap ifNil: [ definitionMap := Dictionary new ].
	^ definitionMap
%

category: 'accessing'
set compile_env: 0
method: CypressDefinitionIndex
definitions
	^self definitionMap values
%

category: 'removing'
set compile_env: 0
method: CypressDefinitionIndex
remove: aDefinition
	self definitionMap removeKey: aDefinition description ifAbsent: []
%

! Class Implementation for CypressDefinition

! ------------------- Instance methods for CypressDefinition

category: 'comparing'
set compile_env: 0
method: CypressDefinition
= aDefinition

	^(aDefinition isKindOf: CypressDefinition)
		and: [aDefinition description = self description]
%

category: 'loading'
set compile_env: 0
method: CypressDefinition
actualClass

	self subclassResponsibility
%

category: 'visiting'
set compile_env: 0
method: CypressDefinition
classDefinition: classBlock methodDefinition: methodBlock
	"default is noop"
%

category: 'accessing'
set compile_env: 0
method: CypressDefinition
description
	self subclassResponsibility
%

category: 'comparing'
set compile_env: 0
method: CypressDefinition
hash
    ^ self description hash
%

category: 'testing'
set compile_env: 0
method: CypressDefinition
isSameRevisionAs: aDefinition
	^ self = aDefinition
%

category: 'loading'
set compile_env: 0
method: CypressDefinition
loadClassDefinition
	"default is to do nothing"
%

category: 'loading'
set compile_env: 0
method: CypressDefinition
loadMethodDefinition
	"default is to do nothing"
%

category: 'loading'
set compile_env: 0
method: CypressDefinition
postLoad
	"noop"
%

category: 'loading'
set compile_env: 0
method: CypressDefinition
postLoadOver: aDefinition

	self postLoad
%

category: 'dependency'
set compile_env: 0
method: CypressDefinition
provisions
	"Answer list of global names defined by this definition"

	^#()
%

category: 'dependency'
set compile_env: 0
method: CypressDefinition
requirements
	"Answer list of global names required by this definition"

	^#()
%

category: 'loading'
set compile_env: 0
method: CypressDefinition
unloadDefinition

	self subclassResponsibility
%

! Class Implementation for CypressMethodDefinition

! ------------------- Class methods for CypressMethodDefinition

category: 'instance creation'
set compile_env: 0
classmethod: CypressMethodDefinition
className: aName classIsMeta: isMetaclass selector: aSelector category: aCategory source: aSource

	^self new
		className: aName asString
		classIsMeta: isMetaclass
		selector: aSelector asString
		category: aCategory asString
		source: (self normalizeLineEndings: aSource)
%

! ------------------- Instance methods for CypressMethodDefinition

category: 'comparing'
set compile_env: 0
method: CypressMethodDefinition
= aDefinition
    ^ super = aDefinition
        and: [ aDefinition source = self source
                and: [ aDefinition category = self category ] ]
%

category: 'loading'
set compile_env: 0
method: CypressMethodDefinition
actualClass

	| cls |
	cls := self theNonMetaClass.
	^self classIsMeta
		ifTrue: [ cls class ]
		ifFalse: [ cls  ].
%

category: 'converting'
set compile_env: 0
method: CypressMethodDefinition
asCypressMethodDefinition

	^self
%

category: 'accessing'
set compile_env: 0
method: CypressMethodDefinition
category

	^category
%

category: 'visiting'
set compile_env: 0
method: CypressMethodDefinition
classDefinition: classBlock methodDefinition: methodBlock

	methodBlock value: self
%

category: 'accessing'
set compile_env: 0
method: CypressMethodDefinition
classIsMeta

	^classIsMeta
%

category: 'accessing'
set compile_env: 0
method: CypressMethodDefinition
className

	^className
%

category: 'initialization'
set compile_env: 0
method: CypressMethodDefinition
className: aName classIsMeta: isMetaclass selector: aSelector category: aCategory source: aSource

	className := aName.
	classIsMeta := isMetaclass.
	selector := aSelector.
	category := aCategory.
	source := self normalizeLineEndings: aSource
%

category: 'accessing'
set compile_env: 0
method: CypressMethodDefinition
description
	^ Array	
		with: className
		with: selector
		with: classIsMeta
%

category: 'comparing'
set compile_env: 0
method: CypressMethodDefinition
hash

	| hash |
	hash := classIsMeta asString hash.
	hash := source hash bitOr: hash.
	hash := category hash bitOr: hash.
	hash := className hash bitOr: hash.
	^hash
%

category: 'visiting'
set compile_env: 0
method: CypressMethodDefinition
instanceMethod: instanceBlock classMethod: classBlock

	^(self classIsMeta
		ifTrue: [ classBlock ]
		ifFalse: [ instanceBlock ]) value: self
%

category: 'testing'
set compile_env: 0
method: CypressMethodDefinition
isInitializer
	^ self selector = 'initialize' and: [self classIsMeta]
%

category: 'loading'
set compile_env: 0
method: CypressMethodDefinition
loadMethodDefinition

	self actualClass
		compileMethod: self source
		dictionaries: System myUserProfile symbolList
		category: self category
		environmentId: 0
%

category: 'loading'
set compile_env: 0
method: CypressMethodDefinition
postLoadOver: aDefinition

	super postLoadOver: aDefinition.
	(self isInitializer
		and: [ aDefinition isNil or: [ self source ~= aDefinition source ]]) 
			ifTrue: [ self theNonMetaClass initialize ].
%

category: 'printing'
set compile_env: 0
method: CypressMethodDefinition
printDetailsOn: aStream

	aStream
		nextPutAll: self className;
		nextPutAll: (self classIsMeta ifTrue: [' class'] ifFalse: ['']);
		nextPutAll: '>>';
		nextPutAll: self selector.
%

category: 'dependency'
set compile_env: 0
method: CypressMethodDefinition
requirements
	"Answer list of global names required by this definition"

	^{self className}
%

category: 'accessing'
set compile_env: 0
method: CypressMethodDefinition
selector

	^selector
%

category: 'accessing'
set compile_env: 0
method: CypressMethodDefinition
source

	^source
%

category: 'loading'
set compile_env: 0
method: CypressMethodDefinition
theNonMetaClass

	^self resolveGlobalNamed: self className
%

category: 'loading'
set compile_env: 0
method: CypressMethodDefinition
unloadDefinition

	self actualClass removeSelector: self selector asSymbol
%

! Class Implementation for CypressClassDefinition

! ------------------- Class methods for CypressClassDefinition

category: 'instance creation'
set compile_env: 0
classmethod: CypressClassDefinition
name: aClassName superclassName: aSuperclassName category: aCategory instVarNames: someInstanceVariableNames classInstVarNames: someClassInstanceVariableNames classVarNames: someClassVariableNames poolDictionaryNames: somePoolDictionaryNames comment: aComment

	^self new
		name: aClassName asString
		superclassName: aSuperclassName asString
		category: aCategory asString
		instVarNames: (someInstanceVariableNames asArray
				collect: [:each | each asString])
		classInstVarNames: (someClassInstanceVariableNames asArray
				collect: [:each | each asString])
		classVarNames: (someClassVariableNames asArray
				collect: [:each | each asString])
		poolDictionaryNames: (somePoolDictionaryNames asArray
				collect: [:each | each asString])
		comment: (self normalizeLineEndings: aComment)
%

! ------------------- Instance methods for CypressClassDefinition

category: 'comparing'
set compile_env: 0
method: CypressClassDefinition
= aDefinition
	^(super = aDefinition)
		and: [superclassName = aDefinition superclassName
		and: [category = aDefinition category
		and: [instVarNames = aDefinition instVarNames
		and: [classInstVarNames = aDefinition classInstVarNames
		and: [classVarNames = aDefinition classVarNames
		and: [poolDictionaryNames = aDefinition poolDictionaryNames
		and: [comment = aDefinition comment]]]]]]]
%

category: 'loading'
set compile_env: 0
method: CypressClassDefinition
actualClass

	^self resolveGlobalNamed: self name
%

category: 'loading'
set compile_env: 0
method: CypressClassDefinition
actualClassOrNil

	^self resolveGlobalNamed: self name or: [nil]
%

category: 'converting'
set compile_env: 0
method: CypressClassDefinition
asCypressClassDefinition

	^self
%

category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
category

	^category
%

category: 'visiting'
set compile_env: 0
method: CypressClassDefinition
classDefinition: classBlock methodDefinition: methodBlock

	classBlock value: self
%

category: 'private'
set compile_env: 0
method: CypressClassDefinition
classInstanceVariablesString
    ^ self stringForVariables: self classInstVarNames
%

category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
classInstVarNames

	^classInstVarNames
%

category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
className

	^self name
%

category: 'loading'
set compile_env: 0
method: CypressClassDefinition
classNeedingMigration: aClass

	self halt: 'not implemented yet'
%

category: 'private'
set compile_env: 0
method: CypressClassDefinition
classVariablesString
    ^ self stringForVariables: self classVarNames
%

category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
classVarNames

	^classVarNames
%

category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
comment

	^comment
%

category: 'loading'
set compile_env: 0
method: CypressClassDefinition
createOrReviseClass
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
		poolDictionaries: #()
		inDictionary: UserGlobals
		options: #())
			category: category;
			comment: self comment
%

category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
description

	^ Array with: name
%

category: 'loading'
set compile_env: 0
method: CypressClassDefinition
failedCompiledMethods: someCompiledMethods

	someCompiledMethods isEmpty ifTrue: [^self].
	self halt: 'not implemented yet'
%

category: 'comparing'
set compile_env: 0
method: CypressClassDefinition
hash

	| hash |
	hash := name hash.
	hash := superclassName hash bitOr: hash.
	hash := (category ifNil: ['']) hash bitOr: hash.
	instVarNames , classInstVarNames, classVarNames, poolDictionaryNames
		do: [:vName | hash := vName hash bitOr: hash].
	^hash
%

category: 'private'
set compile_env: 0
method: CypressClassDefinition
instanceVariablesString
    ^ self stringForVariables: self instVarNames
%

category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
instVarNames

	^instVarNames
%

category: 'loading'
set compile_env: 0
method: CypressClassDefinition
loadClassDefinition
	"Create a new version of the defined class. If the class already exists,
	 copy the behaviors and state from the old version."

	| newClass oldClass |
	oldClass := self actualClassOrNil.
	newClass := self createOrReviseClass.
	(oldClass isNil or: [newClass == oldClass]) ifTrue: [^self].
	self classNeedingMigration: newClass.
	self
		recompileWithSubclassesFrom: oldClass
		to: newClass
		symbolList: System myUserProfile symbolList.
%

category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
name

	^name
%

category: 'initialization'
set compile_env: 0
method: CypressClassDefinition
name: aClassName superclassName: aSuperclassName category: aCategory instVarNames: someInstanceVariableNames classInstVarNames: someClassInstanceVariableNames classVarNames: someClassVariableNames poolDictionaryNames: somePoolDictionaryNames comment: aComment

	name := aClassName.
	superclassName := aSuperclassName.
	category := aCategory.
	instVarNames := someInstanceVariableNames.
	classInstVarNames := someClassInstanceVariableNames.
	classVarNames := someClassVariableNames.
	poolDictionaryNames := somePoolDictionaryNames.
	comment := aComment
%

category: 'private'
set compile_env: 0
method: CypressClassDefinition
poolDictionariesString

	^self stringForVariables: self poolDictionaryNames
%

category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
poolDictionaryNames

	^poolDictionaryNames
%

category: 'printString'
set compile_env: 0
method: CypressClassDefinition
printDetailsOn: aStream

	aStream nextPutAll: self name
%

category: 'dependency'
set compile_env: 0
method: CypressClassDefinition
provisions
	"Answer list of global names defined by this definition"

	^{ self name }
%

category: 'loading'
set compile_env: 0
method: CypressClassDefinition
recompileWithSubclassesFrom: oldClass to: newClass symbolList: aSymbolList

	| olds news removedClassVariables removedSharedPools organizer subclasses newSubclass |
	olds := oldClass _classVars ifNil: [#()] ifNotNil: [:vars | vars keys].
	news := newClass _classVars ifNil: [#()] ifNotNil: [:vars | vars keys].
	removedClassVariables := olds difference: news.
	removedSharedPools := oldClass sharedPools difference: newClass sharedPools.
	self failedCompiledMethods: (newClass
				_copyMethodsAndVariablesFrom: oldClass
				except: { $V. removedClassVariables. $P. removedSharedPools }
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
%

category: 'dependency'
set compile_env: 0
method: CypressClassDefinition
requirements
	"Answer list of global names required by this definition"

	^{self superclassName}
%

category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
superclassName

	^superclassName
%

category: 'loading'
set compile_env: 0
method: CypressClassDefinition
unloadDefinition
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
%

! Class Implementation for CypressLoader

! ------------------- Class methods for CypressLoader

category: 'loading'
set compile_env: 0
classmethod: CypressLoader
updatePackage: aPackage withSnapshot: aSnapshot
	"Answer the loader used to apply the update."

	^(self new)
		updatePackage: aPackage withSnapshot: aSnapshot;
		load
%

! ------------------- Instance methods for CypressLoader

category: 'updating'
set compile_env: 0
method: CypressLoader
addFailedPatchOperation: aPatchOperation

	self errors add: aPatchOperation
%

category: 'accessing'
set compile_env: 0
method: CypressLoader
additions

	additions ifNil: [ additions := OrderedCollection new ].
	^additions
%

category: 'loading'
set compile_env: 0
method: CypressLoader
analyze

	self 
		analyzeAdditions;
		analyzeRemovals
%

category: 'loading'
set compile_env: 0
method: CypressLoader
analyzeAdditions

	| sorter |
	sorter := CypressDependencySorter new 
		addAll: self additions;
		addExternalProvisions: self provisions;
		yourself.
	additions := sorter orderedItems.
	requirements := sorter externalRequirements.
	unloadable := sorter required.
%

category: 'loading'
set compile_env: 0
method: CypressLoader
analyzeRemovals

	| sorter |
	sorter := CypressDependencySorter new 
		addAll: self removals;
		yourself.
	removals := sorter orderedItems reverse.
%

category: 'applying'
set compile_env: 0
method: CypressLoader
applyAddition: aCypressPatchOperation

	self additions add: aCypressPatchOperation
%

category: 'applying'
set compile_env: 0
method: CypressLoader
applyModification: aCypressPatchOperation

	self additions add: aCypressPatchOperation
%

category: 'applying'
set compile_env: 0
method: CypressLoader
applyRemoval: aCypressPatchOperation

	self removals add: aCypressPatchOperation
%

category: 'loading'
set compile_env: 0
method: CypressLoader
attemptInitialLoad

	self
		resetErrors;
		notifyOnFailedPatchOperations;
		loadAdditions: self additions;
		unloadRemovals: self removals.
%

category: 'loading'
set compile_env: 0
method: CypressLoader
errorOnFailedPatchOperations

	exceptionClass := CypressLoaderError.
%

category: 'accessing'
set compile_env: 0
method: CypressLoader
errors
	errors ifNil: [self resetErrors].
	^errors
%

category: 'loading'
set compile_env: 0
method: CypressLoader
handlePatchOperation: aPatchOperation failure: anException
	"Signal the loader exception appropriate to the current phase.
	 Note that a handler may suppress the #addFailedPatchOperation: by
	 sending #return or #return: to the resignaled exception. Otherwise,
	 resumption from a resumable resignalled exception will continue through
	 this method."

	(exceptionClass patchOperation: aPatchOperation exception: anException) signal.
	self addFailedPatchOperation: aPatchOperation.
%

category: 'loading'
set compile_env: 0
method: CypressLoader
load

	self
		analyze;
		reportUnloadableDefinitions;
		attemptInitialLoad;
		retryFailedLoads;
		postLoad.
%

category: 'loading'
set compile_env: 0
method: CypressLoader
loadAdditions: somePatchOperations
	"Load class definitions first, then method definitions."

	somePatchOperations
		do: [:each | self loadClassDefinition: each];
		do: [:each | self loadMethodDefinition: each].
%

category: 'operations'
set compile_env: 0
method: CypressLoader
loadClassDefinition: aPatchOperation

	[aPatchOperation loadClassDefinition]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].
%

category: 'operations'
set compile_env: 0
method: CypressLoader
loadMethodDefinition: aPatchOperation

	[aPatchOperation loadMethodDefinition]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].
%

category: 'accessing'
set compile_env: 0
method: CypressLoader
methodAdditions

	^#()
%

category: 'loading'
set compile_env: 0
method: CypressLoader
notifyOnFailedPatchOperations

	exceptionClass := CypressLoaderErrorNotification.
%

category: 'loading'
set compile_env: 0
method: CypressLoader
postLoad
	"This is where the obsoletion is taken into account ..."

	self additions do: [:each | self postLoad: each].
%

category: 'operations'
set compile_env: 0
method: CypressLoader
postLoad: aPatchOperation

	[aPatchOperation postLoadDefinition]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].
%

category: 'accessing'
set compile_env: 0
method: CypressLoader
provisions
	^ provisions ifNil: [provisions := (self allClasses collect: [:cl | cl name asString]) asSet ]
%

category: 'accessing'
set compile_env: 0
method: CypressLoader
removals

	removals ifNil: [ removals := OrderedCollection new ].
	^removals
%

category: 'loading'
set compile_env: 0
method: CypressLoader
reportUnloadableDefinitions

	self unloadable isEmpty ifTrue: [^self].
	(CypressLoaderMissingClasses missingRequirementsMap: unloadable) signal.
%

category: 'accessing'
set compile_env: 0
method: CypressLoader
requirements

	^requirements
%

category: 'loading'
set compile_env: 0
method: CypressLoader
resetErrors

	errors := OrderedCollection new.
%

category: 'loading'
set compile_env: 0
method: CypressLoader
retryFailedLoads
	"In case any of the failed loads were resolved by subsequent
	 patch operations after the initial attempt or by editting of the
	 failed patch operations by exception handling during the notification
	 phase (initial attempt)."

	| failed |
	failed := self errors.
	self
		resetErrors;
		errorOnFailedPatchOperations;
		loadAdditions: (self additions intersection: failed);
		unloadRemovals: (self removals intersection: failed).
%

category: 'accessing'
set compile_env: 0
method: CypressLoader
unloadable

	unloadable ifNil: [ unloadable := OrderedCollection new ].
	^unloadable
%

category: 'operations'
set compile_env: 0
method: CypressLoader
unloadDefinition: aPatchOperation

	[aPatchOperation unloadDefinition]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].
%

category: 'loading'
set compile_env: 0
method: CypressLoader
unloadRemovals: somePatchOperations
	"Removals need to be done after adding classes and methods."

	somePatchOperations
		do: [:each | self unloadDefinition: each].
%

category: 'loading'
set compile_env: 0
method: CypressLoader
updatePackage: aPackage withSnapshot: aSnapshot

	| patch snapshot |
	snapshot := aPackage snapshot.
	patch := aSnapshot patchRelativeToBase: snapshot.
	patch applyTo: self.
	snapshot definitions do: [:ea | self provisions addAll: ea provisions]
%

! Class Extensions

! Class Extension for CharacterCollection

! ------------------- Instance methods for CharacterCollection

category: '*Cypress-Definitions'
set compile_env: 0
method: CharacterCollection
findString: subString startingAt: startIndex caseSensitive: aBoolean
	"If a receiver contains subString beginning at some point at or after
	 startIndex, this returns the index at which subString begins.  If the
	 receiver does not contain subString, this returns 0."

	^self
		_findString: subString
		startingAt: startIndex
		ignoreCase: aBoolean not
%

category: '*Cypress-Definitions'
set compile_env: 0
method: CharacterCollection
withUnixLineEndings
	"Assume the string is textual, and that CR, LF, and CRLF are all valid line endings.
	 Replace each occurence with a single LF."

	| cr lf inPos outPos outString newOutPos indexLF indexCR |
	cr := Character cr.
	indexCR := self indexOf: cr startingAt: 1.
	indexCR = 0 ifTrue: [^self].
	lf := Character lf.
	indexLF := self indexOf: lf startingAt: 1.
	indexLF = 0 ifTrue: [^self copyReplacing: cr with: lf].
	inPos := outPos := 1.
	outString := String new: self size.
	
	["check if next CR is before next LF or if there are no more LF"
	(indexLF = 0 or: [indexCR < indexLF])
		ifTrue: 
			[newOutPos := outPos + 1 + indexCR - inPos.
			outString
				replaceFrom: outPos
				to: newOutPos - 2
				with: self
				startingAt: inPos.
			outString at: newOutPos - 1 put: lf.
			outPos := newOutPos.
			1 + indexCR = indexLF
				ifTrue: 
					["Caught a CR-LF pair"
					inPos := 1 + indexLF.
					indexLF := self indexOf: lf startingAt: inPos]
				ifFalse: [inPos := 1 + indexCR].
			indexCR := self indexOf: cr startingAt: inPos]
		ifFalse: 
			[newOutPos := outPos + 1 + indexLF - inPos.
			outString
				replaceFrom: outPos
				to: newOutPos - 1
				with: self
				startingAt: inPos.
			outPos := newOutPos.
			inPos := 1 + indexLF.
			indexLF := self indexOf: lf startingAt: inPos].
	indexCR = 0]
			whileFalse.

	"no more CR line endings. copy the rest"
	newOutPos := outPos + (self size - inPos + 1).
	outString
		replaceFrom: outPos
		to: newOutPos - 1
		with: self
		startingAt: inPos.
	^outString copyFrom: 1 to: newOutPos - 1
%

! Class Extension for Class

! ------------------- Instance methods for Class

category: '*Cypress-Definitions'
set compile_env: 0
method: Class
asCypressClassDefinition

	^CypressClassDefinition
		name: self name
		superclassName: self superclass name
		category: self category
		instVarNames: self instVarNames
		classInstVarNames: self class instVarNames
		classVarNames: self classVarNames
		poolDictionaryNames: self sharedPools
		comment: self comment.
%

! Class Extension for Collection

! ------------------- Instance methods for Collection

category: '*Cypress-Definitions'
set compile_env: 0
method: Collection
difference: aCollection
	"Answer the set theoretic difference of two collections."

	| set |
	set := self asSet.
	aCollection do: [:each | set remove: each ifAbsent: []].
	^self species withAll: set asArray
%

category: '*Cypress-Definitions'
set compile_env: 0
method: Collection
gather: aBlock

	^Array
		streamContents: [:stream | self do: [:ea | stream nextPutAll: (aBlock value: ea)]]
%

category: '*Cypress-Definitions'
set compile_env: 0
method: Collection
intersection: aCollection
	"Answer the set theoretic intersection of two collections."

	| set outputSet |
	set := self asSet.
	outputSet := Set new.
	aCollection do: 
			[:each |
			((set includes: each) and: [(outputSet includes: each) not])
				ifTrue: [outputSet add: each]].
	^self species withAll: outputSet asArray
%

! Class Extension for SymbolList

! ------------------- Instance methods for SymbolList

category: '*Cypress-Definitions'
set compile_env: 0
method: SymbolList
allSatisfying: aOneArgBlock
	"Answer the elements from the receiver's Symbol Dictionaries
	 which meet the criteria specified in aOneArgBlock."

	| result |
	result := Array new.
	self asArray do: [:each | result addAll: (each select: aOneArgBlock)].
	^result.
%

! Class Extension for GsNMethod

! ------------------- Instance methods for GsNMethod

category: '*Cypress-Definitions'
set compile_env: 0
method: GsNMethod
asCypressMethodDefinition

	^CypressMethodDefinition
		className: self methodClass theNonMetaClass name
		classIsMeta: self methodClass isMeta
		selector: self selector
		category: self category
		source: self sourceString
%

category: '*Cypress-Definitions'
set compile_env: 0
method: GsNMethod
category

	^self inClass categoryOfSelector: self selector
%

category: '*Cypress-Definitions'
set compile_env: 0
method: GsNMethod
methodClass

	^self inClass
%

