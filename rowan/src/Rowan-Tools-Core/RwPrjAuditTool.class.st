Class {
	#name : 'RwPrjAuditTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'other' }
RwPrjAuditTool >> auditAll [
	
	^self auditAllForUser: System myUserProfile userId
]

{ #category : 'other' }
RwPrjAuditTool >> auditAllForUser: aUserId [
| res |
	res := StringKeyValueDictionary new.
			(Rowan image  _loadedProjectRegistryForUserId: aUserId) keysAndValuesDo: [:prjName :aLoadedProject |
				(self auditForProject: aLoadedProject) ifNotEmpty: [:aColl | res at: prjName put: aColl]
	].
	^res
]

{ #category : 'other' }
RwPrjAuditTool >> auditForProject: aLoadedProject [
	"audit loaded project"

	| res |
	res := KeyValueDictionary new.
	GsFile gciLogClient: '==============Auditing project ' , aLoadedProject name.
	aLoadedProject loadedPackages values
		do: [ :loadedPackage | 
			(Rowan image loadedPackageNamed: loadedPackage name ifAbsent: [  ])
				ifNil: [ 
					"this is a package that is not visible to the current user, so it isn't 
							necessary to audit it, because it isn't likely that the classes 
							referenced by the package are visible to the current user"
					"https://github.com/GemTalk/Rowan/issues/546"
					 ]
				ifNotNil: [ 
					(Rowan packageTools audit auditForPackage: loadedPackage)
						ifNotEmpty: [ :aColl | res at: loadedPackage name put: aColl ] ] ].
	^ res
]

{ #category : 'other' }
RwPrjAuditTool >> auditForProjectNamed: aProjectName [

	^self auditForProject: (Rowan image loadedProjectNamed: aProjectName)

]

{ #category : 'other' }
RwPrjAuditTool >> auditForProjectsNamed: aCol [
"audit all named projects"
	
	| res |
	res := Array new.
		aCol do: [:prjName | res addAll: (self auditForProjectNamed: prjName )	].
	^res

]

{ #category : 'other' }
RwPrjAuditTool >> auditProjectsNamed: aCol [
"audit all named projects"
	
	| res |
	res := Array new.
		aCol do: [:prjName | res addAll: (self auditForProjectNamed: prjName )	].
	^res

]
