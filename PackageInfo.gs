doit
Object subclass: 'PackageInfo'
	instVarNames: #( packageName methodCategoryPrefix preamble
	                  postscript preambleOfRemoval postscriptOfRemoval)
	classVars: #()
	classInstVars: #( default)
	poolDictionaries: #()
	inDictionary: ''
	category: 'PackageInfo-Base'
	options: #()

%

! ------------------- Class comment for PackageInfo
doit
PackageInfo comment: 
''
%

! Remove existing behavior from PackageInfo
doit
PackageInfo removeAllMethods.
PackageInfo class removeAllMethods.
%
! ------------------- Class methods for PackageInfo
category: 'packages access'
set compile_env: 0
classmethod: PackageInfo
allPackages
	^PackageOrganizer default packages
%
category: 'compatibility'
set compile_env: 0
classmethod: PackageInfo
default
	^ self allPackages detect: [:ea | ea class = self] ifNone: [self new register]
%
category: 'class initialization'
set compile_env: 0
classmethod: PackageInfo
initialize
	self allSubclasses do: [:ea | ea new register]
%
category: 'packages access'
set compile_env: 0
classmethod: PackageInfo
named: aString
	^ PackageOrganizer default packageNamed: aString ifAbsent: [(self new packageName: aString) register]
%
category: 'packages access'
set compile_env: 0
classmethod: PackageInfo
registerPackageName: aString
	^ PackageOrganizer default registerPackageNamed: aString
%
! ------------------- Instance methods for PackageInfo
category: 'comparing'
set compile_env: 0
method: PackageInfo
= other
	^ other species = self species and: [other packageName = self packageName]
%
category: 'modifying'
set compile_env: 0
method: PackageInfo
addCoreMethod: aMethodReference
	| category |
	category _ self baseCategoryOfMethod: aMethodReference.
	aMethodReference actualClass organization
		classify: aMethodReference methodSymbol
		under: category
		suppressIfDefault: false
%
category: 'modifying'
set compile_env: 0
method: PackageInfo
addExtensionMethod: aMethodReference
	| category |
	category _ self baseCategoryOfMethod: aMethodReference.
	aMethodReference actualClass 
		classify: aMethodReference methodSymbol
		under: self methodCategoryPrefix, '-', category
%
category: 'modifying'
set compile_env: 0
method: PackageInfo
addMethod: aMethodReference
	(self includesClass: aMethodReference class)
		ifTrue: [self addCoreMethod: aMethodReference]
		ifFalse: [self addExtensionMethod: aMethodReference]
%
category: '*monticello'
set compile_env: 0
method: PackageInfo
allOverridenMethods
	^ MCPlatformSupport allBehaviors gather:
		[:class |
		(self overriddenMethodsInClass: class)]
%
category: 'modifying'
set compile_env: 0
method: PackageInfo
baseCategoryOfMethod: aMethodReference
	| oldCat oldPrefix tokens | 
	oldCat := aMethodReference category.
	((Array with: 'as yet unclassified' with: 'all') includes: oldCat) ifTrue: [ oldCat := '' ].
	tokens := oldCat findTokens: '*-' keep: '*'.

	"Strip off any old prefixes"
	((tokens at: 1 ifAbsent: [ '' ]) = '*') ifTrue: [
		[ ((tokens at: 1 ifAbsent: [ '' ]) = '*') ]
			whileTrue: [ tokens removeFirst ].
		oldPrefix := tokens removeFirst asLowercase.
		[ (tokens at: 1 ifAbsent: [ '' ]) asLowercase = oldPrefix ]
			whileTrue: [ tokens removeFirst ].
	].

	tokens isEmpty ifTrue: [^ 'as yet unclassified'].
	^ String streamContents:
		[ :s |
		tokens
			do: [ :tok | s nextPutAll: tok ]
			separatedBy: [ s nextPut: $- ]]
%
category: 'testing'
set compile_env: 0
method: PackageInfo
category: categoryName matches: prefix
	| prefixSize catSize |
	categoryName ifNil: [ ^false ].
	catSize _ categoryName size.
	prefixSize _ prefix size.
	catSize < prefixSize ifTrue: [ ^false ].
	(categoryName findString: prefix startingAt: 1 caseSensitive: false) = 1
		ifFalse: [ ^false ].
	^(categoryName at: prefix size + 1 ifAbsent: [ ^true ]) = $-
%
category: 'naming'
set compile_env: 0
method: PackageInfo
categoryName
	|category|
	category := self class category.
	^ (category endsWith: '-Info')
		ifTrue: [category copyUpToLast: $-]
		ifFalse: [category]
%
category: 'testing'
set compile_env: 0
method: PackageInfo
changeRecordForOverriddenMethod: aMethodReference

	^nil
%
category: 'listing'
set compile_env: 0
method: PackageInfo
classes
	^(self systemCategories gather:
		[:cat |
		(SystemOrganizer new listAtCategoryNamed: cat)
			collect: [:className | Smalltalk classNamed: className]])
				sortBy: [:a :b | a className <= b className]
%
category: 'listing'
set compile_env: 0
method: PackageInfo
classesAndMetaClasses
	| baseClasses |
	baseClasses := self classes.
	^baseClasses , (baseClasses collect: [:c | c class])
%
category: 'listing'
set compile_env: 0
method: PackageInfo
classesForMethods
	^self classes
%
category: 'testing'
set compile_env: 0
method: PackageInfo
coreCategoriesForClass: aClass
	^ aClass categoryNames select: [:cat | (self isForeignClassExtension: cat) not]
%
category: 'listing'
set compile_env: 0
method: PackageInfo
coreMethods
	^ self classesAndMetaClasses gather: [:class | self coreMethodsForClass: class]
%
category: 'testing'
set compile_env: 0
method: PackageInfo
coreMethodsForClass: aClass
	^ (aClass selectors difference:
		((self foreignExtensionMethodsForClass: aClass) collect: [:r | r methodSymbol]))
			asArray collect: [:sel | self referenceForMethod: sel ofClass: aClass]
%
category: 'testing'
set compile_env: 0
method: PackageInfo
extensionCategoriesForClass: aClass
	^ aClass categoryNames select: [:cat | self isYourClassExtension: cat]
%
category: 'listing'
set compile_env: 0
method: PackageInfo
extensionClasses
	^ self externalClasses reject: [:class | (self extensionCategoriesForClass: class) isEmpty]
%
category: 'listing'
set compile_env: 0
method: PackageInfo
extensionMethods
	^ self externalClasses gather: [:class | self extensionMethodsForClass: class]
%
category: 'testing'
set compile_env: 0
method: PackageInfo
extensionMethodsForClass: aClass
	^ (self extensionCategoriesForClass: aClass)
		gather: [:cat | self methodsInCategory: cat ofClass: aClass ]
%
category: 'testing'
set compile_env: 0
method: PackageInfo
extensionMethodsFromClasses: classes
	^classes
		gather: [:class | self extensionMethodsForClass: class]
%
category: 'dependencies'
set compile_env: 0
method: PackageInfo
externalCallers
	^ self 
		externalRefsSelect: [:literal | literal isKindOf: Symbol] 
		thenCollect: [:l | l].
%
category: '*monticello'
set compile_env: 0
method: PackageInfo
externalClasses
	| myClasses |
	myClasses := self classesAndMetaClasses.
	^ Array streamContents:
		[:s |
		MCPlatformSupport allBehaviors do:
			[:class |
			(myClasses includes: class) ifFalse: [s nextPut: class]]]
%
category: 'naming'
set compile_env: 0
method: PackageInfo
externalName
	^ self packageName
%
category: 'dependencies'
set compile_env: 0
method: PackageInfo
externalRefsSelect: selBlock thenCollect: colBlock
	| pkgMethods dependents refs extMethods otherClasses otherMethods classNames |

	classNames _ self classes collect: [:c | c name].
	extMethods _ self extensionMethods collect: [:mr | mr methodSymbol].
	otherClasses _ self externalClasses difference: self externalSubclasses.
	otherMethods _  otherClasses gather: [:c | c selectors].
	pkgMethods _ self methods asSet collect: [:mr | mr methodSymbol].
	pkgMethods removeAllFoundIn: otherMethods.

	dependents _ Set new.
	otherClasses do: [:c |
		c selectorsAndMethodsDo:
			[:sel :compiled |
			(extMethods includes: sel) ifFalse: 
				[refs _ compiled literals select: selBlock thenCollect: colBlock.
				refs do: [:ea |
					((classNames includes: ea) or: [pkgMethods includes: ea])
							ifTrue: [dependents add: (self referenceForMethod: sel ofClass: c) -> ea]]]]].
	^ dependents
%
category: 'dependencies'
set compile_env: 0
method: PackageInfo
externalSubclasses
	| pkgClasses subClasses |
	pkgClasses _ self classes.
	subClasses _ Set new.
	pkgClasses do: [:c | subClasses addAll: (c allSubclasses)].
	^ subClasses difference: pkgClasses
%
category: 'dependencies'
set compile_env: 0
method: PackageInfo
externalUsers
	^ self 
		externalRefsSelect: [:literal | literal isVariableBinding] 
		thenCollect: [:l | l key]
%
category: 'listing'
set compile_env: 0
method: PackageInfo
foreignClasses
	| s |
	s := IdentitySet new.
	self foreignSystemCategories
		do: [:c | (SystemOrganizer new listAtCategoryNamed: c)
				do: [:cl | 
					| cls | 
					cls := Smalltalk classNamed: cl. 
					s add: cls;
					  add: cls class]].
	^ s
%
category: 'testing'
set compile_env: 0
method: PackageInfo
foreignExtensionCategoriesForClass: aClass
	^ aClass categoryNames select: [:cat | self isForeignClassExtension: cat]
%
category: 'testing'
set compile_env: 0
method: PackageInfo
foreignExtensionMethodsForClass: aClass
	^ (self foreignExtensionCategoriesForClass: aClass)
		gather: [:cat | (aClass selectorsIn: cat)
						  collect: [:sel | self referenceForMethod: sel ofClass: aClass]]
%
category: 'listing'
set compile_env: 0
method: PackageInfo
foreignSystemCategories
	^ SystemOrganizer new categories
		reject: [:cat | self includesSystemCategory: cat]
%
category: 'comparing'
set compile_env: 0
method: PackageInfo
hash
	^ packageName hash
%
category: 'preamble/postscript'
set compile_env: 0
method: PackageInfo
hasPostscript

	^ postscript notNil
%
category: 'preamble/postscript'
set compile_env: 0
method: PackageInfo
hasPostscriptOfRemoval

	^ postscriptOfRemoval notNil
%
category: 'preamble/postscript'
set compile_env: 0
method: PackageInfo
hasPreamble
	^ preamble notNil
%
category: 'preamble/postscript'
set compile_env: 0
method: PackageInfo
hasPreambleOfRemoval

	^ preambleOfRemoval notNil
%
category: 'testing'
set compile_env: 0
method: PackageInfo
includesChangeRecord: aChangeRecord
	^ aChangeRecord methodClass notNil and:
		[self
			includesMethodCategory: aChangeRecord category
			ofClass: aChangeRecord methodClass]
%
category: 'testing'
set compile_env: 0
method: PackageInfo
includesClass: aClass
	^ self includesSystemCategory: aClass theNonMetaClass category
%
category: 'testing'
set compile_env: 0
method: PackageInfo
includesClassNamed: aClassName
	^ self includesSystemCategory: ((SystemOrganizer new categoryOfElement: aClassName) ifNil: [^false])
%
category: 'testing'
set compile_env: 0
method: PackageInfo
includesMethod: aSymbol ofClass: aClass
	aClass ifNil: [^ false].
	^ self
		includesMethodCategory: ((aClass categoryOfSelector: aSymbol)
										ifNil: [' '])
		ofClass: aClass
%
category: 'testing'
set compile_env: 0
method: PackageInfo
includesMethodCategory: categoryName ofClass: aClass
	^ (self isYourClassExtension: categoryName)
		or: [(self includesClass: aClass)
				and: [(self isForeignClassExtension: categoryName) not]]
%
category: 'testing'
set compile_env: 0
method: PackageInfo
includesMethodCategory: categoryName ofClassNamed: aClass
	^ (self isYourClassExtension: categoryName)
		or: [(self includesClassNamed: aClass)
				and: [(self isForeignClassExtension: categoryName) not]]
%
category: 'testing'
set compile_env: 0
method: PackageInfo
includesMethodReference: aMethodRef
	^ self includesMethod: aMethodRef methodSymbol ofClass: aMethodRef actualClass
%
category: 'testing'
set compile_env: 0
method: PackageInfo
includesSystemCategory: categoryName
	^ self category: categoryName matches: self systemCategoryPrefix
%
category: 'testing'
set compile_env: 0
method: PackageInfo
isForeignClassExtension: categoryName
	^ categoryName isEmpty not and: [ categoryName first = $* and: [(self isYourClassExtension: categoryName) not]]
%
category: 'testing'
set compile_env: 0
method: PackageInfo
isOverrideCategory: aString
	^ aString endsWith: '-override'
%
category: 'testing'
set compile_env: 0
method: PackageInfo
isOverrideMethod: aMethodReference
	^ self isOverrideCategory: aMethodReference category
%
category: 'testing'
set compile_env: 0
method: PackageInfo
isOverrideOfYourMethod: aMethodReference
	^ (self isYourClassExtension: aMethodReference category) not and:
		[(self changeRecordForOverriddenMethod: aMethodReference) notNil]
%
category: 'testing'
set compile_env: 0
method: PackageInfo
isYourClassExtension: categoryName
	^ categoryName notNil and: [self category: categoryName matches: self methodCategoryPrefix]
%
category: 'naming'
set compile_env: 0
method: PackageInfo
methodCategoryPrefix
	^ methodCategoryPrefix ifNil: [methodCategoryPrefix _ '*', self packageName asLowercase]
%
category: 'listing'
set compile_env: 0
method: PackageInfo
methods
	^ (self extensionMethods, self coreMethods)
		select: [:method | method isValid and: [(#(DoIt DoItIn:) includes: method methodSymbol) not]]
%
category: 'testing'
set compile_env: 0
method: PackageInfo
methodsInCategory: aString ofClass: aClass 
	^ ((aClass selectorsIn: aString) ifNil: [#()])
			collect: [:sel | self referenceForMethod: sel ofClass: aClass]
%
category: 'preamble/postscript'
set compile_env: 0
method: PackageInfo
name

^ self packageName
%
category: '*monticello'
set compile_env: 0
method: PackageInfo
outsideClasses
	^MCPlatformSupport allBehaviors difference: self classesAndMetaClasses
%
category: 'listing'
set compile_env: 0
method: PackageInfo
overriddenMethods
	^ self allOverridenMethods select: [:ea | self isOverrideOfYourMethod: ea]
%
category: 'listing'
set compile_env: 0
method: PackageInfo
overriddenMethodsInClass: aClass
	^ (self overrideCategoriesForClass: aClass) gather: [:ea | self methodsInCategory: ea ofClass: aClass]
%
category: 'testing'
set compile_env: 0
method: PackageInfo
overrideCategoriesForClass: aClass
	^ aClass categoryNames select: [:cat | self isOverrideCategory: cat]
%
category: 'listing'
set compile_env: 0
method: PackageInfo
overrideMethods
	^ self extensionMethods select: [:ea | self isOverrideMethod: ea]
%
category: 'naming'
set compile_env: 0
method: PackageInfo
packageName
	^ packageName ifNil: [packageName _ self categoryName]
%
category: 'naming'
set compile_env: 0
method: PackageInfo
packageName: aString
	packageName _ aString
%
category: 'preamble/postscript'
set compile_env: 0
method: PackageInfo
postscript

^ postscript ifNil: [postscript _ StringHolder new contents: '"below, add code to be run after the loading of this package"'].
%
category: 'preamble/postscript'
set compile_env: 0
method: PackageInfo
postscript: aString

postscript _ StringHolder new contents: aString
%
category: 'preamble/postscript'
set compile_env: 0
method: PackageInfo
postscriptOfRemoval

^ postscriptOfRemoval ifNil: [postscriptOfRemoval _ StringHolder new contents: '"below, add code to clean up after the unloading of this package"']
%
category: 'preamble/postscript'
set compile_env: 0
method: PackageInfo
postscriptOfRemoval: aString

postscriptOfRemoval _ StringHolder new contents: aString
%
category: 'preamble/postscript'
set compile_env: 0
method: PackageInfo
preamble

	^ preamble ifNil: [preamble _ StringHolder new contents: '"below, add code to be run before the loading of this package"'].
%
category: 'preamble/postscript'
set compile_env: 0
method: PackageInfo
preamble: aString

preamble _ StringHolder new contents: aString
%
category: 'preamble/postscript'
set compile_env: 0
method: PackageInfo
preambleOfRemoval

^ preambleOfRemoval ifNil: [preambleOfRemoval _ StringHolder new contents: '"below, add code to prepare for the unloading of this package"']
%
category: 'preamble/postscript'
set compile_env: 0
method: PackageInfo
preambleOfRemoval: aString

preambleOfRemoval _ StringHolder new contents: aString
%
category: 'testing'
set compile_env: 0
method: PackageInfo
referenceForMethod: aSymbol ofClass: aClass
	^ MethodReference new setStandardClass: aClass methodSymbol: aSymbol
%
category: 'registering'
set compile_env: 0
method: PackageInfo
register
	PackageOrganizer default registerPackage: self
%
category: 'modifying'
set compile_env: 0
method: PackageInfo
removeMethod: aMethodReference
%
category: 'listing'
set compile_env: 0
method: PackageInfo
selectors
	^ self methods collect: [:ea | ea methodSymbol]
%
category: 'listing'
set compile_env: 0
method: PackageInfo
systemCategories
	^ SystemOrganizer new categories select: [:cat | self includesSystemCategory: cat]
%
category: 'naming'
set compile_env: 0
method: PackageInfo
systemCategoryPrefix
	^ self packageName
%
category: '*grease-gemstone-core'
set compile_env: 0
method: PackageInfo
versionString
	| ancestor ancestors |
	ancestors := (MCPackage named: self name) workingCopy ancestry ancestors.
	ancestors isEmpty ifTrue: [ ^'unknown' ].
	ancestor := ancestors first.
	^ancestor name , '.mcz      ' , ancestor date printString, ' ', ancestor time printString
%
doit
PackageInfo category: 'PackageInfo-Base'
%
