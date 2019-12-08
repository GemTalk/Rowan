Class {
	#name : 'RwResolvedRepositoryV2',
	#superclass : 'RwAbstractResolvedObjectV2',
	#category : 'Rowan-Components'
}

{ #category : 'repository' }
RwResolvedRepositoryV2 >> branches [
	^ self _projectRepository branches
]

{ #category : 'repository' }
RwResolvedRepositoryV2 >> checkout: aCommittish [
	^ self _projectRepository checkout: aCommittish
]

{ #category : 'repository' }
RwResolvedRepositoryV2 >> createBranch: branchName [
	^ self _projectRepository createBranch: branchName
]

{ #category : 'repository' }
RwResolvedRepositoryV2 >> pull: remoteName branch: branchName [
	^ self _projectRepository pull: remoteName branch: branchName
]

{ #category : 'repository' }
RwResolvedRepositoryV2 >> remote [
	^ self _projectRepository remote
]
