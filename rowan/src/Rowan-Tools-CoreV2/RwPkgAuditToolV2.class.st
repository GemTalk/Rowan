Class {
	#name : 'RwPkgAuditToolV2',
	#superclass : 'RwAbstractTool',
	#category : 'Rowan-Tools-CoreV2'
}

{ #category : 'other' }
RwPkgAuditToolV2 >> auditForPackage: loadedPackage [
	"audit dirty packages"

	| res packageSymbolDictionaryName registrySymbolDictionaryName packageAuditDetail |
	res := RwAuditReport for: loadedPackage.
	packageAuditDetail := {}.
	(Rowan image
		loadedRegistryForPackageNamed: loadedPackage name
		ifAbsent: [ 
			packageAuditDetail
				add:
					(RwAuditPackageDetail
						for: loadedPackage
						reason: #'loadedPackageNotInRegistry'
						message:
							'The loaded package ' , loadedPackage name printString
								, ' is not found in a package registry').
			nil ])
		ifNotNil: [ :loadedPackageRegistry | 
			packageSymbolDictionaryName := loadedPackage packageSymbolDictionaryName.
			registrySymbolDictionaryName := loadedPackageRegistry _symbolDictionary name
				asString.
			registrySymbolDictionaryName = packageSymbolDictionaryName
				ifFalse: [ 
					packageAuditDetail
						add:
							(RwAuditPackageDetail
								for: loadedPackage
								reason: #'loadedPackageInWrongRegistry'
								message:
									'The loaded package ' , loadedPackage name printString
										, ' is registered in the wrong symbol dictionary ('
										, registrySymbolDictionaryName printString
										, '). It is expected to be registered in '
										, packageSymbolDictionaryName printString) ].
			loadedPackage
				loadedClassesDo: [ :aLoadedClass | 
					| classSymbolDictName |
					classSymbolDictName := aLoadedClass classSymbolDictionaryName.
					classSymbolDictName = packageSymbolDictionaryName
						ifFalse: [ 
							packageAuditDetail
								add:
									((RwAuditPackageClassSymbolDictionaryDetail
										for: loadedPackage
										message:
											'The loaded class symbol dictionary name ' , classSymbolDictName printString
												, ' does not match the loaded package symbol dictionary name '
												, packageSymbolDictionaryName printString)
										reason: #'differentSymbolDictionaryForClassAndPackage';
										loadedClass: aLoadedClass;
										classSymbolDictionaryName: classSymbolDictName;
										packageSymbolDictionaryName: packageSymbolDictionaryName;
										yourself) ].
					(self auditLoadedClass: aLoadedClass)
						ifNotEmpty: [ :aColl | res at: aLoadedClass name put: aColl ] ]
				loadedClassExtensionsDo: [ :aLoadedClass | 
					| classEtensionSymbolDictionaryName |
					classEtensionSymbolDictionaryName := loadedPackageRegistry
						_symbolDictionary name asString.
					classEtensionSymbolDictionaryName = packageSymbolDictionaryName
						ifFalse: [ 
							packageAuditDetail
								add:
									((RwAuditPackageClassSymbolDictionaryDetail
										for: loadedPackage
										message:
											'The loaded extension class symbol dictionary name '
												, classEtensionSymbolDictionaryName printString
												, ' does not match the loaded package symbol dictionary name '
												, packageSymbolDictionaryName printString)
										reason: #'differentSymbolDictionaryForClassExtensionAndPackage';
										loadedClass: aLoadedClass;
										classSymbolDictionaryName: classEtensionSymbolDictionaryName;
										packageSymbolDictionaryName: packageSymbolDictionaryName;
										yourself) ].
					(self auditLoadedClassExtension: aLoadedClass)
						ifNotEmpty: [ :aColl | res at: aLoadedClass name put: aColl ] ] ].
	packageAuditDetail
		ifNotEmpty: [ :aColl | res at: loadedPackage name put: aColl ].
	^ res
]

{ #category : 'other' }
RwPkgAuditToolV2 >> auditForPackageNamed: packageName [
	
	^self auditForPackage: (Rowan image loadedPackageNamed: packageName).

]

{ #category : 'other' }
RwPkgAuditToolV2 >> auditLoadedClass: aLoadedClass [
	"look for methods compiled into class without Rowan API"

	^ RwClsAuditTool new auditClass: aLoadedClass
]

{ #category : 'other' }
RwPkgAuditToolV2 >> auditLoadedClassExtension: aLoadedClass [
"look for methods compiled into class without Rowan API"

	^ RwClsExtensionAuditTool new auditClass: aLoadedClass
]
