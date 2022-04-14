Class {
	#name : 'RwAuditPackageDetail',
	#superclass : 'RwAuditDetail',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'testing' }
RwAuditPackageDetail >> isPackageDetail [
	"package details do not respond to #behavior ... need to filter details during Rowan upgrade"

	^ true
]
