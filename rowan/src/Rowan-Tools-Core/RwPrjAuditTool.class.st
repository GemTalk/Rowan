Class {
	#name : 'RwPrjAuditTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'other' }
RwPrjAuditTool >> auditProjectsNamed: aCol [

	^aCol collect: [:prjName | (Rowan image loadedProjectNamed: prjName) 
			loadedPackagesDo: [:a| ]
			loadedClassedDo: [:a| ]
			loadedClassExtenstionsDo: [:a| ]
			loadedInstanceMethodsDo: [:a| ]
			loadedClassMethodsDo: [:a| ]
	]

]
