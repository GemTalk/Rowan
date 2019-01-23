Class {
	#name : 'RwProjectReferenceDefinitionTest',
	#superclass : 'RwAbstractComponentDefinitionTest',
	#category : 'Rowan-Tests'
}

{ #category : 'tests' }
RwProjectReferenceDefinitionTest >> testSpecificationConversion [

	| rowanSpec projectHome specUrlString projectSpec_1 projectSpec_2 projectReferenceDefinition memory 
		x y |	
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	projectHome := rowanSpec repositoryRootPath , '/test/testRepositories/'.

"identify spec to be used for reading project"
	specUrlString :=  'file:' , projectHome, '/Issue122/filetree/rowan/specs/Issue122.ston'.

	projectSpec_1 := specUrlString asRwUrl asSpecification.

	projectReferenceDefinition := projectSpec_1 asProjectReferenceDefinition.

	projectSpec_2 := projectReferenceDefinition asSpecification.

	memory := FileSystem currentMemoryFileSystem.
	(memory / 'projectSpec_1') ensureDeleteAll.
	(memory / 'projectSpec_2') ensureDeleteAll.

	(memory / 'projectSpec_1') ensureCreateDirectory.
	(memory / 'projectSpec_2') ensureCreateDirectory.
	projectSpec_1 exportToUrl: 'memory:/projectSpec_1'.
	projectSpec_2 exportToUrl: 'memory:/projectSpec_2'.

	(x := (memory / 'projectSpec_1' / 'Issue122.ston') contents).
	(y := (memory / 'projectSpec_2' / 'Issue122.ston') contents).
	x = y 
		ifFalse: [
			1 to: (x size min: y size) do: [:i |
				| a b |
				(a := x at: i) = (b := y at: i) ifFalse: [ self halt ] ]].
	self assert: x = y.
]
