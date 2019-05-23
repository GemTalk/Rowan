tests
_auditBlock

	|  res  loadedProject auditLoadedClassBlock auditLoadedClassExtensionBlock |
	GsFile gciLogServer: '--STARTING ROWAN AUDIT'.
	res := KeyValueDictionary new.
	auditLoadedClassBlock := self _auditLoadedClassBlock.

	auditLoadedClassExtensionBlock := self _auditLoadedClassBlock.
		
	Rowan projectNames do: [:projectName |
		GsFile gciLogServer: '---Auditing project: ', projectName printString.
		loadedProject := Rowan image loadedProjectNamed: projectName.
		loadedProject loadedPackages do: [:loadedPackage |
			loadedPackage
				loadedClasses
					valuesDo: [:aLoadedClass |
						(auditLoadedClassBlock value: aLoadedClass) 
							ifNotEmpty: [:aColl | res at: aLoadedClass name put: aColl ] ].
			loadedPackage
				loadedClassExtensions
					valuesDo: [:aLoadedClass | 
						(auditLoadedClassExtensionBlock value: aLoadedClass) 
							ifNotEmpty: [:aColl | res at: aLoadedClass name put: aColl ] ] ] ].
	UserGlobals at: #ROWAN_AUDIT_issue_365_results put: res.
	GsFile gciLogServer: '--ENDING ROWAN AUDIT'.
^true
