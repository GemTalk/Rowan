Extension { #name : 'RwGsLoadedSymbolDictClass' }

{ #category : '*rowan-gemstone-core-32x' }
RwGsLoadedSymbolDictClass >> disownFromLoaded: aPackageSymbolDictionary [

       loadedInstanceMethods
               valuesDo: [ :loadedInstanceMethod | 
                       self removeLoadedInstanceMethod: loadedInstanceMethod.
                       aPackageSymbolDictionary methodRegistry
                               removeKey: loadedInstanceMethod handle ].
       loadedClassMethods
               valuesDo: [ :loadedClassMethod | 
                       self removeLoadedClassMethod: loadedClassMethod.
                       aPackageSymbolDictionary methodRegistry removeKey: loadedClassMethod handle ].

]
