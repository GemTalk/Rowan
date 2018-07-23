Extension { #name : 'Date' }

{ #category : '*ston-gemstonecommon' }
Date >> stonOn: stonWriter [
  "Use an ISO style YYYYMMDD representation"

  stonWriter
    writeObject: self
    listSingleton: (self asStringUsingFormat: #(3 2 1 $- 1 1 $: false))

]

{ #category : '*ston-gemstonecommon' }
Date class >> fromSton: stonReader [

	^ self fromStream: stonReader parseListSingleton readStream usingFormat: #(3 2 1 $- 1 1)

]
