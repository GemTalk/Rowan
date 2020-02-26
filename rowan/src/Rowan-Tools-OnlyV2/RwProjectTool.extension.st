Extension { #name : 'RwProjectTool' }

{ #category : '*rowan-tools-onlyv2' }
RwProjectTool class >> browser [

	^RwPrjBrowserToolV2 new
]

{ #category : '*rowan-tools-onlyv2' }
RwProjectTool class >> load [

	^RwPrjLoadToolV2 new
]

{ #category : '*rowan-tools-onlyv2' }
RwProjectTool class >> write [

	^RwPrjWriteToolV2 new
]

