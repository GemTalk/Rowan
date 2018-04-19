Extension { #name : 'TonelCypressReaderTest' }

{ #category : '*cypresstonel-gemstone-tests' }
TonelCypressReaderTest >> createReaderFor: snapshot fileName: packageName [
  | dir |
  dir := self newFileDirectorySnapshot: snapshot fileName: packageName.
  ^ TonelCypressReader on: dir fileName: packageName

]

{ #category : '*cypresstonel-gemstone-tests' }
TonelCypressReaderTest >> directory [
	directory ifNil:
		[directory := (GsFile _expandEnvVariable: 'PWD' isClient: false)].
	^ directory

]

{ #category : '*cypresstonel-gemstone-tests' }
TonelCypressReaderTest >> newFileDirectorySnapshot: aSnapshot fileName: packageName [
  "This method assumes writer works. If it is broken (the tests should show it), this tests 
	 will break too."

  | dir |
  dir := self directory.
  (TonelCypressWriter on: dir)
    writeSnapshot: aSnapshot
    inPackageNamed: packageName.
  ^ dir

]

{ #category : '*cypresstonel-gemstone-tests' }
TonelCypressReaderTest >> tearDown [
	self directory ifNotNil: [:dir | CypressFileUtilities current deleteAll: dir ].
	super tearDown.

]
