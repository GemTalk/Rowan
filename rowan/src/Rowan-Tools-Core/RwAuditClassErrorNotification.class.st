Class {
	#name : 'RwAuditClassErrorNotification',
	#superclass : 'RwAdoptAuditErrorNotification',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'Handling' }
RwAuditClassErrorNotification >> defaultAction [
	"record audit error"
	^ true
]
