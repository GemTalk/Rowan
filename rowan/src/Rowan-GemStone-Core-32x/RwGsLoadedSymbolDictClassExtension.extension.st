Extension { #name : 'RwGsLoadedSymbolDictClassExtension' }

{ #category : '*rowan-gemstone-core-32x' }
RwGsLoadedSymbolDictClassExtension >> disownFromLoaded: registry [
       loadedInstanceMethods
               valuesDo: [ :loadedInstanceMethod | 
                       self removeLoadedInstanceMethod: loadedInstanceMethod.
                       registry methodRegistry removeKey: loadedInstanceMethod handle ].
       loadedClassMethods
               valuesDo: [ :loadedClassMethod | 
                       self removeLoadedClassMethod: loadedClassMethod.
                       registry methodRegistry removeKey: loadedClassMethod handle ].

       registry unregisterLoadedClassExtension: self forClass: handle

]
