Class {
	#name : 'RowanBrowserServiceTest',
	#superclass : 'RowanServicesTest',
	#category : 'Rowan-Services-Tests'
}

{ #category : 'tests' }
RowanBrowserServiceTest >> test_compileClassWithClassSelected [
  "because the selectedClass is the name of the 
	class to be compiled, it compiles without a confirmation"

  | browserService |
  self
    jadeiteIssueTested: #'issue516'
    withTitle: 'Deleting classes and project reload (3.0.77)'.
  self createServicesTestPackage.
  self loadServicesTestProject.
  browserService := RowanBrowserService new.
  browserService selectedClass: 'Fubar'.
  browserService
    compileClass:
      'Object rwSubclass: ''Fubar''
	instVarNames: #( abc def )
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
		category: ''' , self servicesTestPackageName
        ,
          '''
		options: #()'.
  browserService recompileMethodsAfterClassCompilation.
  [ 
  self
    assert:
      (Rowan image symbolList resolveSymbol: #'Fubar') value allInstVarNames
    equals: #(#'abc' #'def').
  browserService selectedClass: 'Fubar'.
  browserService
    compileClass:
      'Object rwSubclass: ''Fubar''
	instVarNames: #( abc )
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
		category: ''' , self servicesTestPackageName
        ,
          '''
		options: #()'.
  browserService recompileMethodsAfterClassCompilation.
  self
    assert:
      (Rowan image symbolList resolveSymbol: #'Fubar') value allInstVarNames
    equals: #(#'abc') ]
    ensure: [ 
      Rowan projectTools browser removeClassNamed: 'Fubar'.
      self unloadServicesTestProject ]
]

{ #category : 'tests' }
RowanBrowserServiceTest >> test_flipAutoCommit [
	| autoCommit service | 
	self jadeiteIssueTested: #issue396 withTitle: 'Ability to turn on autocommit would be nice'.
	autoCommit := RowanService autoCommit. 
	service := RowanAutoCommitService new. 
	[service flipAutoCommit. 
	self assert: RowanService autoCommit equals: autoCommit not. 
	self assert: (SessionTemps current at: #'Jadeite_AutoCommit') equals: autoCommit not.
	service flipAutoCommit. 
	self assert: RowanService autoCommit equals: autoCommit. 
	self assert: (SessionTemps current at: #'Jadeite_AutoCommit') equals: autoCommit] ensure: [
		RowanService setAutoCommit: autoCommit.
		self assert: RowanService autoCommit equals: autoCommit.]
]

{ #category : 'tests' }
RowanBrowserServiceTest >> test_windowsRegistry [
  | browserService object |
  self
    jadeiteIssueTested: #'issue385'
    withTitle: 'Inspector should keep associated root object alive'.
  browserService := RowanBrowserService new.
  object := Object new.
  browserService saveRootObject: object asOop windowHandle: 123456.
  self assert: (browserService openWindows at: 123456) last equals: object.
  browserService releaseWindowHandle: 123456.
  self
    assert: (browserService openWindows at: 123456 ifAbsent: [ 'gone' ])
    equals: 'gone'
]
