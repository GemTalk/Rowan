Extension { #name : 'RwProjectDefinition' }

{ #category : '*rowan-definitions-deprecated' }
RwProjectDefinition >> commit: message [
	"lperform commit on the repository associated with receiver ..."

	^ Rowan projectTools commit commitProjectDefinition: self message: message
]

{ #category : '*rowan-definitions-deprecated' }
RwProjectDefinition >> export [

	Rowan projectTools write writeDeprecatedProjectDefinition: self
]

{ #category : '*rowan-definitions-deprecated' }
RwProjectDefinition >> exportSpecification [

	^ Rowan tools spec exportProjectDefinition: self
]
