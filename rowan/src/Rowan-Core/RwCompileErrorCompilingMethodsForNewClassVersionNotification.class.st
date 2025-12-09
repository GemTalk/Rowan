Class {
	#name : 'RwCompileErrorCompilingMethodsForNewClassVersionNotification',
	#superclass : 'RwNotification',
	#instVars : [
		'compileError',
		'projectName',
		'packageName',
		'methodSelector',
		'methodCategory'
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

{ #category : 'accessing' }
RwCompileErrorCompilingMethodsForNewClassVersionNotification >> methodCategory [
	^methodCategory
]

{ #category : 'accessing' }
RwCompileErrorCompilingMethodsForNewClassVersionNotification >> methodCategory: object [
	methodCategory := object
]

{ #category : 'accessing' }
RwCompileErrorCompilingMethodsForNewClassVersionNotification >> methodSelector [
	^methodSelector
]

{ #category : 'accessing' }
RwCompileErrorCompilingMethodsForNewClassVersionNotification >> methodSelector: object [
	methodSelector := object
]

{ #category : 'accessing' }
RwCompileErrorCompilingMethodsForNewClassVersionNotification >> packageName [
	^packageName
]

{ #category : 'accessing' }
RwCompileErrorCompilingMethodsForNewClassVersionNotification >> packageName: object [
	packageName := object
]

{ #category : 'accessing' }
RwCompileErrorCompilingMethodsForNewClassVersionNotification >> projectName [
	^projectName
]

{ #category : 'accessing' }
RwCompileErrorCompilingMethodsForNewClassVersionNotification >> projectName: object [
	projectName := object
]
