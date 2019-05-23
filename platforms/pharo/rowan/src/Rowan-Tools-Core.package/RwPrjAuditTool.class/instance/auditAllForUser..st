other
auditAllForUser: aUserId
| res |
	res := StringKeyValueDictionary new.
			(Rowan image  _loadedProjectRegistryForUserId: aUserId) keysAndValuesDo: [:prjName :aLoadedProject |
				(self auditForProject: aLoadedProject) ifNotEmpty: [:aColl | res at: prjName put: aColl]
	].
	^res