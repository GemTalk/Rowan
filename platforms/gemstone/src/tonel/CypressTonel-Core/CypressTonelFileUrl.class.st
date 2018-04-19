"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressTonelFileUrl',
	#superclass : 'CypressAbstractFileUrl',
	#category : 'CypressTonel-Core'
}

{ #category : 'constants' }
CypressTonelFileUrl class >> schemeName [
  ^ 'tonel'

]

{ #category : 'accessing' }
CypressTonelFileUrl >> codeFormat [
  ^ 'Tonel'

]

{ #category : 'testing' }
CypressTonelFileUrl >> isStrict [
  ^ true

]

{ #category : 'accessing' }
CypressTonelFileUrl >> repositoryClass [

	^CypressTonelRepository

]
