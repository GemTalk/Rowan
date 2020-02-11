Class {
	#name : 'RwPrjTraceTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'testing' }
RwPrjTraceTool >> isTracing [
	^ (SessionTemps current at: #'ROWAN_TRACE' otherwise: nil) == #'gciLogServer'
]

{ #category : 'tracing' }
RwPrjTraceTool >> startTracing [
	SessionTemps current at: #'ROWAN_TRACE' put: #'gciLogServer'
]

{ #category : 'tracing' }
RwPrjTraceTool >> stopTracing [
	SessionTemps current removeKey: #'ROWAN_TRACE' ifAbsent: []
]

{ #category : 'tracing' }
RwPrjTraceTool >> trace: message [
	self isTracing
		ifTrue: [ GsFile gciLogServer: message ]
]
