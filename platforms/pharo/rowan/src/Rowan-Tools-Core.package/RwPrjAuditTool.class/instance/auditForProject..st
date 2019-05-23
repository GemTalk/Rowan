other
auditForProject: aLoadedProject
"audit loaded project"

	| res |

	res := KeyValueDictionary new.
	GsFile gciLogClient: '==============Auditing project ', aLoadedProject name.
		aLoadedProject loadedPackages values do: [:e | (Rowan packageTools audit auditForPackage: e) 
				ifNotEmpty: [:aColl | res at: e name put: aColl]].	
	^res