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

	res := Dictionary new.
	self _log: '==============Auditing project ', aLoadedProject name.
		aLoadedProject loadedPackages values do: [:e | (Rowan packageTools audit auditForPackage: e) 
				ifNotEmpty: [:aColl | res at: e name put: aColl]].	
	^res
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
