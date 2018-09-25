! Package: Tonel-GemStone-Tests


! Remove existing behavior from package Tonel-GemStone-Tests
!!!! This can be cleaned up when some package functionality is moved to the base system.

doit
| packageName |
packageName := 'Tonel-GemStone-Tests'.
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

! Class Extension for TonelAbstractWriterTest

! ------------------- Instance methods for TonelAbstractWriterTest

category: '*tonel-gemstone-tests'
method: TonelAbstractWriterTest
contentsOfFileNamed: fileName inDirectory: dir
  self fileUtils
    readStreamFor: fileName
    in: dir
    do: [ :stream | ^ stream contents ]
%

category: '*tonel-gemstone-tests'
method: TonelAbstractWriterTest
directory
  directory
    ifNil: [ 
      | fileUtils |
      fileUtils := self fileUtils.
      directory := fileUtils
        directoryFromPath: 'mctest'
        relativeTo: fileUtils default.
      fileUtils ensureDirectoryExists: directory ].
  ^ directory
%

category: '*tonel-gemstone-tests'
method: TonelAbstractWriterTest
directoryNamed: directoryName existsIn: dir
  | fileUtils filePath |
  fileUtils := self fileUtils.
  filePath := fileUtils directoryFromPath: directoryName relativeTo: dir.
  ^ fileUtils directoryExists: filePath
%

category: '*tonel-gemstone-tests'
method: TonelAbstractWriterTest
directoryNamed: directoryName in: dir
  | fileUtils filePath |
  fileUtils := self fileUtils.
  ^ fileUtils directoryFromPath: directoryName relativeTo: dir
%

category: '*tonel-gemstone-tests'
method: TonelAbstractWriterTest
fileNamesIn: dir
  | fileUtils filePath |
  fileUtils := self fileUtils.
  ^ fileUtils entryNamesFrom: dir
%

category: '*tonel-gemstone-tests'
method: TonelAbstractWriterTest
tearDown
  directory
    ifNotNil: [ :dir | 
      | fileUtils |
      fileUtils := self fileUtils.
      (fileUtils directoryExists: dir)
        ifTrue: [ fileUtils deleteAll: dir ] ].
  super tearDown
%

! Class Extension for TonelReaderTest

! ------------------- Instance methods for TonelReaderTest

category: '*tonel-gemstone-tests'
method: TonelReaderTest
directory
  directory
    ifNil: [ 
      | fileUtils |
      fileUtils := self fileUtils.
      directory := fileUtils
        directoryFromPath: 'mctest'
        relativeTo: fileUtils default.
      fileUtils ensureDirectoryExists: directory ].
  ^ directory
%

category: '*tonel-gemstone-tests'
method: TonelReaderTest
tearDown
  directory
    ifNotNil: [ :dir | 
      | fileUtils |
      fileUtils := self fileUtils.
      (fileUtils directoryExists: dir)
        ifTrue: [ fileUtils deleteAll: dir ] ].
  super tearDown
%

! Class initializers 

doit
true.
%



! End of Package: Tonel-GemStone-Tests


