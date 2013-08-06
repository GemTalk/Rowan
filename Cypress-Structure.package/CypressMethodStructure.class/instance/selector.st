"
notice: This work is protected by copyright. All rights reserved.
category: accessing
"
selector
    ^ String
        streamContents: [ :stream | 
            self name
                do: [ :chara | 
                    stream
                        nextPut:
                            (chara = $.
                                ifTrue: [ $: ]
                                ifFalse: [ chara ]) ] ]
