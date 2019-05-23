other
auditForPackage: loadedPackage
"audit dirty packages"
	|  res|
	res := KeyValueDictionary new.
	GsFile gciLogClient: '  -- Auditing package ', loadedPackage name.
		loadedPackage 
				loadedClassesDo: [:aLoadedClass |  (self auditLoadedClass: aLoadedClass) 
					ifNotEmpty: [:aColl | res at: aLoadedClass name put: aColl]]				
				loadedClassExtensionsDo: [:aLoadedClass | (self auditLoadedClassExtension: aLoadedClass) 
					ifNotEmpty: [:aColl | res at: aLoadedClass name put: aColl] ] .
	^res
