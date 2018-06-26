Class {
	#name : 'RwExecuteClassInitializeMethodsAfterLoadNotification',
	#superclass : 'RwNotification',
	#type : 'variable',
	#gs_options : [
		'disallowGciStore'
	],
	#category : 'Rowan-Core'
}

{ #category : 'handling' }
RwExecuteClassInitializeMethodsAfterLoadNotification >> defaultAction [

	^ false

]
