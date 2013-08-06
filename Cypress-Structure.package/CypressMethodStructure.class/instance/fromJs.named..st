"
notice: This work is protected by copyright. All rights reserved.
category: initialization
"
fromJs: jsObject named: methodNameParts
    | ext |
    (ext := methodNameParts at: 2) = '.st'
        ifTrue: [ self extractCypressSource: (jsObject at: 'contents') ]
        ifFalse: [ 
            ext = '.json'
                ifTrue: [ properties := jsObject at: 'contents' ] ]
