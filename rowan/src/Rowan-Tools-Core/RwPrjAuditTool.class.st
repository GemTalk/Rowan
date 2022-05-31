Class {
	#name : 'RwPrjAuditTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'other' }
RwPrjAuditTool >> auditAll [
	^ self auditAllOn: nil
]

{ #category : 'other' }
RwPrjAuditTool >> auditAllForUser: aUserId [
	^ self auditAllForUser: aUserId on: nil
]

{ #category : 'other' }
RwPrjAuditTool >> auditAllForUser: aUserId on: logStreamOrNil [
	| res |
	res := RwAuditReport for: self on: logStreamOrNil.
	(Rowan image _loadedProjectRegistryForUserId: aUserId)
		keysAndValuesDo: [ :prjName :aLoadedProject | 
			(self auditForProject: aLoadedProject on: logStreamOrNil)
				ifNotEmpty: [ :aColl | res at: prjName put: aColl ] ].
	^ res
]

{ #category : 'other' }
RwPrjAuditTool >> auditAllOn: logStreamOrNil [
	^ self auditAllForUser: System myUserProfile userId on: logStreamOrNil
]

{ #category : 'other' }
RwPrjAuditTool >> auditForProject: aLoadedProject [
	"audit loaded project"

	^ self auditForProject: aLoadedProject on: nil
]

{ #category : 'other' }
RwPrjAuditTool >> auditForProject: aLoadedProject on: logStreamOrNil [
	"audit loaded project"

	| res logMessage |
	res := RwAuditReport for: aLoadedProject on: logStreamOrNil.
	logMessage := '==============Auditing project ' , aLoadedProject name.
	logStreamOrNil
		ifNil: [ GsFile gciLogClient: logMessage ]
		ifNotNil: [ 
			logStreamOrNil
				nextPutAll: logMessage;
				lf ].
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
					(Rowan packageTools audit auditForPackage: loadedPackage on: logStreamOrNil)
						ifNotEmpty: [ :aColl | res at: loadedPackage name put: aColl ] ] ].
	^ res
]

{ #category : 'other' }
RwPrjAuditTool >> auditForProjectNamed: aProjectName [
	^ self auditForProject: (Rowan image loadedProjectNamed: aProjectName) on: nil
]

{ #category : 'other' }
RwPrjAuditTool >> auditForProjectNamed: aProjectName on: logStreamOrNil [
	^ self
		auditForProject: (Rowan image loadedProjectNamed: aProjectName)
		on: logStreamOrNil
]

{ #category : 'other' }
RwPrjAuditTool >> auditForProjectsNamed: aCol [
	"audit all named projects"

	^ self auditForProjectsNamed: aCol on: nil
]

{ #category : 'other' }
RwPrjAuditTool >> auditForProjectsNamed: aCol on: logStreamOrNil [
	"audit all named projects"

	| res |
	res := Array new.
	aCol
		do: [ :prjName | res addAll: (self auditForProjectNamed: prjName on: logStreamOrNil) ].
	^ res
]
