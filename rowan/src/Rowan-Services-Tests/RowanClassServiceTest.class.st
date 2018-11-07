Class {
	#name : 'RowanClassServiceTest',
	#superclass : 'RowanServicesTest',
	#category : 'Rowan-Services-Tests'
}

{ #category : 'patch' }
RowanClassServiceTest >> createClassNamed: className [

]

{ #category : 'support' }
RowanClassServiceTest >> setUp [

	super setUp.
	self createDefaultClass. 
	self loadDefaultProject.
]

{ #category : 'support' }
RowanClassServiceTest >> tearDown [

	super tearDown.
	self unloadDefaultProject
]

{ #category : 'tests' }
RowanClassServiceTest >> test_addedProjectNotOnDisk [

	| proj pkg cls projectService |
	proj := RwProjectDefinition newForGitBasedProjectNamed: 'Azer'.
	proj repositoryRootPath: '$ROWAN_PROJECTS_HOME/Azer'.
	Rowan projectTools create createProjectFor: proj.
	pkg := RwPackageDefinition newNamed: 'Baijan'.
	proj addPackage: pkg.

	cls := RwClassDefinition
		newForClassNamed: #Baijan
		super: 'Object'
		instvars: #(name address orderHistory)
		classinstvars: #()
		classvars: #()
		category: nil
		comment: 'a CustomerRecord holds the sales record for a customer.'
		pools: #()
		type: 'normal'.
	pkg addClassDefinition: cls.
	Rowan projectTools load loadProjectDefinition: proj.

	(RowanClassService forClassNamed: 'Baijan') update. "<-- walkback occurrred here"
	projectService := RowanProjectService newNamed: 'Azer'. 
	[self deny: projectService existsOnDisk.
	self assert: projectService isSkew]
		ensure: [RowanBrowserService new unloadProjectsNamed: (Array with: 'Azer')]
]

{ #category : 'tests' }
RowanClassServiceTest >> test_isTestCase [

	| classService |
	self jadeiteIssueTested: #issue253 withTitle: 'popup method menu in method list is slow for JadeServer methods'. 
	classService := RowanClassService forClassNamed: self class.
	self assert: classService isTestCase.
	classService isTestCase: false. 
	self deny: classService isTestCase. 
	classService setIsTestCaseCommand. 
	self assert: classService isTestCase.

	classService := RowanClassService forClassNamed: 'JadeServer'.
	self deny: classService isTestCase.
]

{ #category : 'tests' }
RowanClassServiceTest >> testAddCategory [
	| classService behavior |
	
	behavior := Rowan globalNamed: self defaultClassName. 
	self deny: (behavior categoryNames includes: 'fnoodle'). 
	classService := RowanClassService forClassNamed: self defaultClassName meta: false. 
	classService addCategory: 'fnoodle'.
	self assert: (behavior categoryNames includes: #fnoodle).
]

{ #category : 'tests' }
RowanClassServiceTest >> testBehavior [
	"return class or meta class" 
	| classService |
	classService := RowanClassService forClassNamed: 'Array' meta: false.
	self assert: classService behavior equals: Array.
	classService := RowanClassService forClassNamed: 'OrderedCollection' meta: true.
	self assert: classService behavior equals: OrderedCollection class
]

{ #category : 'tests' }
RowanClassServiceTest >> testClassComment [
	| classService className behavior packageService |
	behavior := Rowan globalNamed: self defaultClassName. 
	self assert: behavior comment equals: String new. 
	classService := RowanClassService forClassNamed: self defaultClassName meta: false. 
	classService classComment: 'This is a test'. 
	self assert: behavior comment equals: 'This is a test'
]

{ #category : 'tests' }
RowanClassServiceTest >> testClassFromName [
	"always return thisClass" 
	| classService |
	classService := RowanClassService forClassNamed: 'Array' meta: false.
	self assert: classService classFromName equals: Array.
	classService := RowanClassService forClassNamed: 'OrderedCollection' meta: true.
	self assert: classService classFromName equals: OrderedCollection
]

{ #category : 'tests' }
RowanClassServiceTest >> testClassHierarchy [
	"return class hierarchy in format client can use.
	#nil -> #(Object class service)
	Object class service -> #(RowanService class service) 
	etc"
  
	| classService hierarchy objectClassService rowanServiceService |
	classService := RowanClassService forClassNamed: 'RowanClassService'.
	hierarchy := classService classHierarchy hierarchyServices. 
	self assert: (hierarchy isKindOf: Dictionary). 
	self assert: ((hierarchy at: #nil) isKindOf: Array).
	self assert: (hierarchy at: #nil) size equals: 1. 
	objectClassService := (hierarchy at: #nil) first.
	self assert: (objectClassService isKindOf: RowanClassService).
	self assert: objectClassService name equals: 'Object'.
	rowanServiceService := (hierarchy at: objectClassService) first. 
	self assert: rowanServiceService name equals: 'RowanService'.
	self assert: (hierarchy at: rowanServiceService) first name equals: 'RowanClassService'.
]

{ #category : 'tests' }
RowanClassServiceTest >> testClassHierarchyClassSide [
	"same as instance side"
  
	| classService hierarchy objectClassService rowanServiceService |
	classService := RowanClassService forClassNamed: 'RowanClassService' meta: true.
	hierarchy := classService classHierarchy hierarchyServices. 
	self assert: (hierarchy isKindOf: Dictionary). 
	self assert: ((hierarchy at: #nil) isKindOf: Array).
	self assert: (hierarchy at: #nil) size equals: 1. 
	objectClassService := (hierarchy at: #nil) first.
	self assert: (objectClassService isKindOf: RowanClassService).
	self assert: objectClassService name equals: 'Object'.
	rowanServiceService := (hierarchy at: objectClassService) first. 
	self assert: rowanServiceService name equals: 'RowanService'.
	self assert: (hierarchy at: rowanServiceService) first name equals: 'RowanClassService'.
]

{ #category : 'tests' }
RowanClassServiceTest >> testEquality [

	| classService1 classService2 |
	classService1 := RowanClassService forClassNamed: 'RowanClassService'. 
	classService2 := RowanClassService forClassNamed: 'RowanClassService'. 
	self assert: classService1 equals: classService2.
	self deny: classService1 == classService2.
	self deny: classService1 equals: RowanClassService. 
	self deny: classService1 equals: #foo.
	self deny: classService1 equals: (RowanPackageService forPackageNamed: 'Rowan-Services-Tests').
	self deny: classService1 equals: (RowanProjectService newNamed: 'Rowan').
	self deny: classService1 equals: (RowanMethodService forSelector: #testEquality class: RowanClassServiceTest meta: false organizer: ClassOrganizer new)
]
