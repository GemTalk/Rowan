Class {
	#name : 'RwAuditClassErrorNotification',
	#superclass : 'RwAdoptAuditClassErrorNotification',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'Handling' }
RwAuditClassErrorNotification >> defaultAction [
	"record audit error"
	^ true
]
