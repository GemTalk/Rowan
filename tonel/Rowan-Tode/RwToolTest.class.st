Class {
	#name : 'RwToolTest',
	#superclass : 'TestCase',
	#category : 'Rowan-Tode'
}

{ #category : 'private' }
RwToolTest >> basicPackageLoadSpec: packageNames packageNamesToLoad: packageNamesToLoad [
  ^ RwPackageLoadSpecification new
    specName: 'basic test spec';
    packageNames: packageNames;
    packageNamesToLoad: packageNamesToLoad;
    repoSpec: RwDiskRepositorySpecification new;
    yourself
]

{ #category : 'private' }
RwToolTest >> basicPackageSet [
  "this is the package set used to generate the test data in the testRepositories dir"

  ^ RwDiskRepositoryGenerator new generatePackageSet
]
