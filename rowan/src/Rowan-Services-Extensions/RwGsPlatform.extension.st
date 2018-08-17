Extension { #name : 'RwGsPlatform' }

{ #category : '*rowan-services-extensions' }
RwGsPlatform >> classServiceClass [

	^ RowanClassService

]

{ #category : '*rowan-services-extensions' }
RwGsPlatform >> jadeServerClassNamed: className [

	| jadeClasses |
	jadeClasses := Array with: (UserGlobals at: #JadeServer). 
	jadeClasses add: (UserGlobals at: #JadeServer64bit32). 
	^jadeClasses detect:[:cls | cls name == className] ifNone:[self error: 'Could not look up a JadeServer class: ', className]
]

{ #category : '*rowan-services-extensions' }
RwGsPlatform >> methodServiceClass [

	^ RowanMethodService

]

{ #category : '*rowan-services-extensions' }
RwGsPlatform >> packageServiceClass [

	^ RowanPackageService

]

{ #category : '*rowan-services-extensions' }
RwGsPlatform >> projectServiceClass [

	^ RowanProjectService

]

{ #category : '*rowan-services-extensions' }
RwGsPlatform >> serviceClass [

	^ RowanService

]

{ #category : '*rowan-services-extensions' }
RwGsPlatform >> serviceClasses [
	"Explicitly add each class rather than sending #allSubclasses so
	that users other than SystemUser have visibility. Visibility in Rowan
	is determined at compile time."
	
	| array |
	array := Array with: RowanService. 
	array add: RowanAnsweringService; 
		add: RowanBrowserService;
		add: RowanClassService; 
		add: RowanDebuggerService; 
		add: RowanFrameService;
		add: RowanMethodService;
		add: RowanPackageService; 
		add: RowanProcessService;
		add: RowanProjectService;
		add: RowanQueryService;
		add: RowanVariableService.
	^array
]

{ #category : '*rowan-services-extensions' }
RwGsPlatform >> serviceClassFor: className [

	^self serviceClasses detect:[:cls | cls name asString = className asString] ifNone:[]
]
