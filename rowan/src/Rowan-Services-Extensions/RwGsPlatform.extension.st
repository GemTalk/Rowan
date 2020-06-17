Extension { #name : 'RwGsPlatform' }

{ #category : '*rowan-services-extensions' }
RwGsPlatform >> answeringServiceClass [
  ^ RowanAnsweringService
]

{ #category : '*rowan-services-extensions' }
RwGsPlatform >> browserServiceClass [

	^ RowanBrowserService
]

{ #category : '*rowan-services-extensions' }
RwGsPlatform >> classServiceClass [

	^ RowanClassService
]

{ #category : '*rowan-services-extensions' }
RwGsPlatform >> commandResultClass [

	^ RowanCommandResult
]

{ #category : '*rowan-services-extensions' }
RwGsPlatform >> definedProjectServiceClass [
	^ RowanDefinedProjectService
]

{ #category : '*rowan-services-extensions' }
RwGsPlatform >> jadeServerClassNamed: className [

	| jadeClasses |
	jadeClasses := Array with: (UserGlobals at: #JadeServer). 
	jadeClasses add: (UserGlobals at: #JadeServer64bit32). 
	jadeClasses add: (UserGlobals at: #JadeServer64bit35). 
	^jadeClasses detect:[:cls | cls name == className] ifNone:[self error: 'Could not look up a JadeServer class: ', className]
]

{ #category : '*rowan-services-extensions' }
RwGsPlatform >> loadedProjectServiceClass [
	^ RowanLoadedProjectService
]

{ #category : '*rowan-services-extensions' }
RwGsPlatform >> loggingServiceClass [

	^ RowanLoggingService
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
RwGsPlatform >> resolvedProjectServiceClass [
	^ RowanResolvedProjectService
]

{ #category : '*rowan-services-extensions' }
RwGsPlatform >> serviceClass [

	^ RowanService

]

{ #category : '*rowan-services-extensions' }
RwGsPlatform >> serviceClasses [
	"Explicitly add each class rather than sending #allSubclasses so
	that users other than SystemUser have visibility. Visibility in Rowan
	is determined at compile time. See STONReader>>lookupClass:"

	| array |
	array := Array with: RowanService.
	array
		add: RowanAnsweringService;
		add: RowanAutoCommitService;
		add: RowanBrowserService;
		add: RowanClassService;
		add: RowanDebuggerService;
		add: RowanDefinedProjectService;
		add: RowanDictionaryService;
		add: RowanFrameService;
		add: RowanInspectorService;
		add: RowanLoadedProjectService;
		add: RowanLoggingService;
		add: RowanMethodService;
		add: RowanPackageService;
		add: RowanProcessService;
		add: RowanProjectService;
		add: RowanQueryService;
		add: RowanResolvedProjectService;
		add: RowanSpecificationService;
		add: RowanTestService;
		add: RowanVariableService.
	^ array
]

{ #category : '*rowan-services-extensions' }
RwGsPlatform >> serviceClassFor: className [

	^self serviceClasses detect:[:cls | cls name asString = className asString] ifNone:[]
]

{ #category : '*rowan-services-extensions' }
RwGsPlatform >> specificationServiceClass [
  ^ RowanSpecificationService
]
