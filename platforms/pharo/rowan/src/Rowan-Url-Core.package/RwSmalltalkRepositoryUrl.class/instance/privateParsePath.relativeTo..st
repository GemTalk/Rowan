parsing
privateParsePath: remainder relativeTo: basePath
  | parsedPath |
  parsedPath := super privateParsePath: remainder relativeTo: basePath.
  (parsedPath
    detect: [ :e | e includes: $: ]
    ifNone: [ 
      project := parsedPath copyFrom: 1 to: parsedPath size - 1.
      dir := parsedPath copyFrom: 2 to: parsedPath size ])
    ifNotNil: [ :pivot | 
      project := parsedPath copyFrom: 1 to: (parsedPath indexOf: pivot) - 1.
      project addLast: (pivot copyUpTo: $:).
      committish := pivot copyFrom: (pivot indexOf: $:) + 1 to: pivot size.
      dir := parsedPath
        copyFrom: (parsedPath indexOf: pivot) + 1
        to: parsedPath size ].
  ^ parsedPath