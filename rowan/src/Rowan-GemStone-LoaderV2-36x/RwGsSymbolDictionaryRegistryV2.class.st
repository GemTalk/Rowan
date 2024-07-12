Class {
	#name : 'RwGsSymbolDictionaryRegistryV2',
	#superclass : 'Object',
	#instVars : [
		'symbolDictionary',
		'packageRegistry'
	],
	#category : 'Rowan-GemStone-LoaderV2-36x'
}

{ #category : 'package adopt' }
RwGsSymbolDictionaryRegistryV2 >> adoptCompiledMethod: compiledMethod classExtension: classExtension for: behavior protocol: protocolString toPackageNamed: packageName [
	| methodDictionary selector protocolSymbol loadedMethod loadedPackage loadedClassOrExtension |
	classExtension
		ifTrue: [ 
			"to adopt and extension method, it is much less complicated"
			^ self
				addExtensionCompiledMethod: compiledMethod
				for: behavior
				protocol: protocolString
				toPackageNamed: packageName ].

	methodDictionary := (behavior persistentMethodDictForEnv: 0)
		ifNil: [ Dictionary new ].
	selector := compiledMethod selector.
	compiledMethod
		==
			(methodDictionary
				at: selector
				ifAbsent: [ self error: 'expected an existing compiled method' ])
		ifFalse: [ 
			self
				error:
					'The given compiled method ', compiledMethod printString, ' does not match the existing compiled method in the class: ', behavior printString, '.' ].

	protocolSymbol := protocolString asSymbol.
	(behavior includesCategory: protocolSymbol)
		ifFalse: [ behavior addCategory: protocolSymbol ].
	behavior _moveMethod: selector toCategory: protocolSymbol.

	compiledMethod isFromRowan
		ifTrue: [ 
			"existing LoadedMethod found for compiled method ... ignore already packaged methods"
			^ self ].

	loadedMethod := RwGsLoadedSymbolDictMethod forMethod: compiledMethod.
	compiledMethod _origin: loadedMethod.

	loadedPackage := self
		loadedPackageNamed: packageName
		ifAbsent: [ 
			self
				error: 'Internal error -- attempt to add a method ', compiledMethod printString, ' to a nonexistent package', packageName printString, '.' ].

	loadedClassOrExtension := loadedPackage
		loadedClassOrClassExtensionForClass: behavior
		ifAbsent: [ 
			self
				error:
					'Internal error -- attempt to add a method to a package ', loadedPackage name printString, ' in which its class ', behavior printString, ' is neither defined nor extended.' ].
	loadedClassOrExtension addLoadedMethod: loadedMethod
]

{ #category : 'initialization' }
RwGsSymbolDictionaryRegistryV2 >> initialize [

	| symbolDictObjectSecurityPolicy |
	packageRegistry := KeyValueDictionary new.	"keyed by package name"

	symbolDictObjectSecurityPolicy := symbolDictionary objectSecurityPolicy.
	packageRegistry objectSecurityPolicy: symbolDictObjectSecurityPolicy.
]
