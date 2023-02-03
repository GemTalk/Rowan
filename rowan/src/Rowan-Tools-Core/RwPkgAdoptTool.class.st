Class {
	#name : 'RwPkgAdoptTool',
	#superclass : 'RwPackageTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'smalltalk api' }
RwPkgAdoptTool >> adoptClass: theClass classExtension: classExtension instanceSelectors: instanceSelectors classSelectors: classSelectors intoPackageNamed: packageName [
	"adopt the methods for class named <className> and it's methods into the package named <packageName>, 
		if it is not a <classExtension>, adopt the class into the package as well."

	"Ignore packaged instance and class methods"

	| loadedPackage loadedProject packageSymDictName theSymbolDictionary registry theBehavior className |
	loadedPackage := Rowan image loadedPackageNamed: packageName.
	loadedProject := loadedPackage loadedProject.
	className := theClass name asString.

	packageSymDictName := (loadedProject symbolDictNameForPackageNamed: packageName)
		asSymbol.
	theSymbolDictionary := Rowan image symbolDictNamed: packageSymDictName.

	registry := theSymbolDictionary rowanSymbolDictionaryRegistry.

	classExtension
		ifFalse: [ 
			[ 
			registry
				addClassAssociation: (theSymbolDictionary associationAt: theClass name)
				forClass: theClass
				toPackageNamed: packageName ]
				on: RwExistingAssociationWithSameKeyNotification
				do: [ :ex | ex resume ] ].

	theBehavior := theClass.
	instanceSelectors
		do: [ :methodSelector | 
			| theCompiledMethod |
			theCompiledMethod := (theBehavior
				compiledMethodAt: methodSelector
				otherwise: nil)
				ifNil: [ 
					(RwAdoptMissingMethodErrorNotification
						method: methodSelector
						isMeta: false
						inClassNamed: className
						isClassExtension: classExtension
						intoPackageNamed: packageName) signal.
					"skip adoption of this method"
					nil ].
			theCompiledMethod
				ifNotNil: [ 
					registry
						adoptCompiledMethod: theCompiledMethod
						classExtension: classExtension
						for: theBehavior
						protocol: (theBehavior categoryOfSelector: methodSelector)
						toPackageNamed: packageName ] ].

	theBehavior := theClass class.
	classSelectors
		do: [ :methodSelector | 
			| theCompiledMethod |
			theCompiledMethod := (theBehavior
				compiledMethodAt: methodSelector
				otherwise: nil)
				ifNil: [ 
					(RwAdoptMissingMethodErrorNotification
						method: methodSelector
						isMeta: true
						inClassNamed: className
						isClassExtension: classExtension
						intoPackageNamed: packageName) signal.
					"skip adoption of this method"
					nil ].
			theCompiledMethod
				ifNotNil: [ 
					registry
						adoptCompiledMethod: theCompiledMethod
						classExtension: classExtension
						for: theBehavior
						protocol: (theBehavior categoryOfSelector: methodSelector)
						toPackageNamed: packageName ] ]
]

{ #category : 'smalltalk api' }
RwPkgAdoptTool >> adoptClass: theClass intoPackageNamed: packageName [
	"adopt the class named <className> and it's methods into the package named <packageName>"

	"Ignore packaged instance and class methods"

	self
		adoptClass: theClass
		classExtension: false
		instanceSelectors: theClass selectors
		classSelectors: theClass class selectors
		intoPackageNamed: packageName
]

{ #category : 'smalltalk api' }
RwPkgAdoptTool >> adoptClassExtension: theClass instanceSelectors: instanceSelectors classSelectors: classSelectors intoPackageNamed: packageName [
	"adopt extension methods for the class named <className> into the package named <packageName>"

	^ self
		adoptClass: theClass
		classExtension: true
		instanceSelectors: instanceSelectors
		classSelectors: classSelectors
		intoPackageNamed: packageName
]

{ #category : 'smalltalk api' }
RwPkgAdoptTool >> adoptClassExtensionNamed: className  instanceSelectors: instanceSelectors classSelectors: classSelectors intoPackageNamed: packageName [

	"adopt extension methods for the class named <className> into the package named <packageName>"

	self 
		adoptClassNamed: className 
		classExtension: true 
		instanceSelectors: instanceSelectors 
		classSelectors: classSelectors 
		intoPackageNamed: packageName

]

{ #category : 'smalltalk api' }
RwPkgAdoptTool >> adoptClassNamed: className classExtension: classExtension instanceSelectors: instanceSelectors classSelectors: classSelectors intoPackageNamed: packageName [
	"adopt the methods for class named <className> and it's methods into the package named <packageName>, 
		if it is not a <classExtension>, adopt the class into the package as well."

	"Ignore packaged instance and class methods"

	| loadedPackage loadedProject packageSymDictName theClass theSymbolDictionary |
	loadedPackage := Rowan image loadedPackageNamed: packageName.
	loadedProject := loadedPackage loadedProject.

	packageSymDictName := (loadedProject symbolDictNameForPackageNamed: packageName)
		asSymbol.
	theSymbolDictionary := Rowan image symbolDictNamed: packageSymDictName.

	theClass := theSymbolDictionary
		at: className asSymbol
		ifAbsent: [ 
			(RwAdoptMissingClassErrorNotification
				classNamed: className
				isClassExtension: classExtension
				intoPackageNamed: packageName) signal.
			"if exception resumed then we'll skip the adopt operation for this class"
			^ self ].

	^ self adoptClass: theClass classExtension: classExtension instanceSelectors: instanceSelectors classSelectors: classSelectors intoPackageNamed: packageName
]

{ #category : 'smalltalk api' }
RwPkgAdoptTool >> adoptClassNamed: className  instanceSelectors: instanceSelectors classSelectors: classSelectors intoPackageNamed: packageName [

	"adopt the class named <className> and it's methods into the package named <packageName>"

	"Ignore packaged instance and class methods"
	
	self 
		adoptClassNamed: className 
		classExtension: false 
		instanceSelectors: instanceSelectors 
		classSelectors: classSelectors 
		intoPackageNamed: packageName

]

{ #category : 'smalltalk api' }
RwPkgAdoptTool >> adoptClassNamed: className  intoPackageNamed: packageName [

	"adopt the class named <className> and it's methods into the package named <packageName>"

	"Ignore packaged instance and class methods"

	| theClass |

	theClass := Rowan globalNamed: className.
	self 
		adoptClassNamed: className 
		instanceSelectors: theClass selectors 
		classSelectors: theClass class selectors 
		intoPackageNamed: packageName

]

{ #category : 'smalltalk api' }
RwPkgAdoptTool >> adoptMethod: methodSelector inClassNamed: className  isMeta: isMeta intoPackageNamed: packageName [

	"adopt the method <methodSelector> in class named <className> and it's methods into the package named <packageName>"

	| theClass theBehavior protocolString  |
	theClass := Rowan globalNamed: className.
	theBehavior := isMeta
		ifTrue: [ theClass class ]
		ifFalse: [ theClass ].
	protocolString := (theBehavior categoryOfSelector: methodSelector asSymbol) asString. 
	self 
		adoptMethod: methodSelector 
		protocol: protocolString 
		inClassNamed: className  
		isMeta: isMeta 
		intoPackageNamed: packageName

]

{ #category : 'smalltalk api' }
RwPkgAdoptTool >> adoptMethod: methodSelector protocol: protocolString inClassNamed: className  isMeta: isMeta intoPackageNamed: packageName [

	"adopt the method <methodSelector> in class named <className> and it's methods into the package named <packageName>.
		move the method into protocol <protocolString> "

	| loadedPackage loadedProject packageSymDictName theClass theSymbolDictionary registry 
		theBehavior theCompiledMethod |
	loadedPackage := Rowan image loadedPackageNamed: packageName.
	loadedProject := loadedPackage loadedProject.

	packageSymDictName := loadedProject symbolDictNameForPackageNamed: packageName.

	theClass := Rowan globalNamed: className.
	theSymbolDictionary := Rowan image symbolDictNamed: packageSymDictName.

	registry := theSymbolDictionary rowanSymbolDictionaryRegistry.

	theBehavior := isMeta
		ifTrue: [ theClass class ]
		ifFalse: [ theClass ].

	theCompiledMethod := theBehavior compiledMethodAt: methodSelector.

	theCompiledMethod rowanProjectName = Rowan unpackagedName
		ifFalse: [ self error: 'The method ', className printString, '>>', methodSelector asString, ' is already packaged ... no need to adopt' ].

	theClass  rowanPackageName ~= packageName
		ifTrue: [  
			registry
				adoptCompiledMethod: theCompiledMethod 
				classExtension: true
				for: theBehavior 
				protocol: protocolString
				toPackageNamed: packageName ]
		ifFalse: [ 
			registry
				adoptCompiledMethod: theCompiledMethod 
				classExtension: false
				for: theBehavior 
				protocol: protocolString
				toPackageNamed: packageName ].
]

{ #category : 'smalltalk api' }
RwPkgAdoptTool >> adoptSymbolDictionary: symbolDictionary intoPackageNamed: packageName [

	"create loaded classes in the loaded package <packageName> for the unpackaged classes in <symbolDictionary>"

	symbolDictionary keysAndValuesDo: [:className :object |
		object isBehavior
			ifTrue: [ 
				object rowanPackageName = Rowan unpackagedName
					ifTrue: [ self adoptClassNamed: className asString intoPackageNamed: packageName ] ] ]
.

]

{ #category : 'smalltalk api' }
RwPkgAdoptTool >> adoptSymbolDictionaryNamed: symDictName intoPackageNamed: packageName [

	"create loaded classes in the loaded package <packageName> for the unpackaged classes in the symbol dictionary named <symDictName>"

	^ self adoptSymbolDictionary: (Rowan image symbolDictNamed: symDictName) intoPackageNamed: packageName
]

{ #category : 'smalltalk api' }
RwPkgAdoptTool >> adoptSymbolList: symbolList excluding: excludedSymbolDictionaries intoPackagesNamed: packageNameMap [

	"create loaded classes for the unpackaged classes in each of the symbol dictionaries in symbolList, except for those listed in
		<excludedSymbolDictionaries>. The loaded classes should be created in the package associated with the name of the symbol 
		dictionary in <packageNameMap>"

	symbolList do: [:symbolDict |
		(excludedSymbolDictionaries includes: symbolDict)
			ifFalse: [ self adoptSymbolDictionary: symbolDict intoPackageNamed: (packageNameMap at: symbolDict name asString) ] ]

]

{ #category : 'smalltalk api' }
RwPkgAdoptTool >> adoptSymbolList: symbolList excludingSymbolDictsNamed: excludedSymbolDictionaryNames intoPackagesNamed: packageNameMap [

	"create loaded classes for the unpackaged classes in each of the symbol dictionaries in symbolList, except for those named in
		<excludedSymbolDictionaryNames>. The loaded classes should be created in the package associated with the name of the symbol 
		dictionary in <packageNameMap>"

	symbolList do: [:symbolDict |
		| symDictName |
		symDictName := symbolDict name asString.
		(excludedSymbolDictionaryNames includes: symDictName)
			ifFalse: [ self adoptSymbolDictionary: symbolDict intoPackageNamed: (packageNameMap at: symDictName) ] ]

]
