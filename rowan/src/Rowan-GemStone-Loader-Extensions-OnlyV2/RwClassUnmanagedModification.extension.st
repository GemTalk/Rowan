Extension { #name : 'RwClassUnmanagedModification' }

{ #category : '*rowan-gemstone-loader-extensions-onlyv2' }
RwClassUnmanagedModification >> classVersioningPatchClassUsing: aPatchSet [

	^ aPatchSet _classUnmanagedVersioningPatchClass
]
