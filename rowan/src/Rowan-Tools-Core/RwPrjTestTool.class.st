Class {
	#name : 'RwPrjTestTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'private' }
RwPrjTestTool >> _addTestsForProjectNamed: projectName toTestSuite: suite [

	| loadedProject |
	loadedProject := Rowan image loadedProjectNamed: projectName.
	loadedProject loadedPackages
		do: [ :loadedPackage | 
			loadedPackage loadedClasses values
				do: [ :rowanLoadedClass | 
					| cl |
					cl := rowanLoadedClass handle.
					(cl inheritsFrom: TestCase)
						ifTrue: [ 
							cl isAbstract
								ifFalse: [ suite addTests: cl suite tests ] ] ] ].
	^ suite

]

{ #category : 'smalltalk api' }
RwPrjTestTool >> testSuiteForProjectNamed: projectName [

	| suite |
	suite := TestSuite named: projectName , ' tests'.
	self _addTestsForProjectNamed: projectName toTestSuite: suite.
	^ suite

]

{ #category : 'smalltalk api' }
RwPrjTestTool >> testSuiteForProjectsNamed: projectNames [

	| suite strm |
	strm := WriteStream on: String new.
	projectNames do: [:projectName | strm nextPutAll: projectName; space ].
	strm nextPutAll:  'tests'.
	suite := TestSuite named: strm contents.
	projectNames do: [:projectName | self _addTestsForProjectNamed: projectName toTestSuite: suite ].
	^ suite

]
