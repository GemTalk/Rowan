Class {
	#name : 'RwCompileErrorCompilingMethodsForNewClassVersionNotification',
	#superclass : 'RwNotification',
	#instVars : [
		'compileError'
	],
	#category : 'Rowan-Core'
}

{ #category : 'accessing' }
RwCompileErrorCompilingMethodsForNewClassVersionNotification >> compileError [
	^compileError
]

{ #category : 'accessing' }
RwCompileErrorCompilingMethodsForNewClassVersionNotification >> compileError: object [
	compileError := object
]

{ #category : 'Handling' }
RwCompileErrorCompilingMethodsForNewClassVersionNotification >> defaultAction [
 	"resume exception with true, to indicate that the method will be manually added with corrections after the load is completed"
	"do not handle or resume exception with false, to pass the compile error on up the stack"
	
	^self compileError pass
]
