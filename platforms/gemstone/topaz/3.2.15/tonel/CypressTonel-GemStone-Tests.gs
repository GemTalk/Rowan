! Package: CypressTonel-GemStone-Tests


! Remove existing behavior from package CypressTonel-GemStone-Tests
!!!! This can be cleaned up when some package functionality is moved to the base system.

doit
| packageName |
packageName := 'CypressTonel-GemStone-Tests'.
System myUserProfile symbolList do: [:symDict |
	symDict do: [:possibleClass |
			| toRemove |
		possibleClass isBehavior ifTrue: [
			{possibleClass. possibleClass class} do: [:aClass |
				aClass category = packageName
					ifTrue: [
							"*anythingbutpackagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										each isEmpty not and: [
											(each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]])
											or: [each first ~= $*]]]
					]
					ifFalse: [
							"*packagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										each isEmpty not and: [
											each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]]]]
					].
				toRemove do: [:each | aClass removeCategory: each].
			]
		]
	]
].
true.
%


! Class Declarations

! Class Extensions

! Class Extension for TonelCypressReaderTest

! ------------------- Instance methods for TonelCypressReaderTest

category: '*cypresstonel-gemstone-tests'
method: TonelCypressReaderTest
createReaderFor: snapshot fileName: packageName
  | dir |
  dir := self newFileDirectorySnapshot: snapshot fileName: packageName.
  ^ TonelCypressReader on: dir fileName: packageName
%

category: '*cypresstonel-gemstone-tests'
method: TonelCypressReaderTest
directory
	directory ifNil:
		[directory := (GsFile _expandEnvVariable: 'PWD' isClient: false)].
	^ directory
%

category: '*cypresstonel-gemstone-tests'
method: TonelCypressReaderTest
newFileDirectorySnapshot: aSnapshot fileName: packageName
  "This method assumes writer works. If it is broken (the tests should show it), this tests 
	 will break too."

  | dir |
  dir := self directory.
  (TonelCypressWriter on: dir)
    writeSnapshot: aSnapshot
    inPackageNamed: packageName.
  ^ dir
%

category: '*cypresstonel-gemstone-tests'
method: TonelCypressReaderTest
tearDown
	self directory ifNotNil: [:dir | CypressFileUtilities current deleteAll: dir ].
	super tearDown.
%

! Class initializers 

doit
true.
%



! End of Package: CypressTonel-GemStone-Tests


