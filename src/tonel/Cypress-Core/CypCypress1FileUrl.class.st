"
No class-specific documentation for CypCypressFileUrl, hierarchy is: 
Object
  CypUrl
    CypFileUrl
      CypCypressFileUrl

"
Class {
	#name : 'CypCypress1FileUrl',
	#superclass : 'CypFileUrl',
	#category : 'Cypress-Core'
}

{ #category : 'private' }
CypCypress1FileUrl class >> schemeName [

	^'cypress'
]

{ #category : 'public' }
CypCypress1FileUrl >> repositoryClass [

	^CypCypress1Repository
]
