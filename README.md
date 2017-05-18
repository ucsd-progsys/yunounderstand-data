yunounderstand
==============

This is a collection of novice interactions with the OCaml top-level. 

We recruited around 50 students each across two instances of the CSE 130
(Undergraduate Programming Languages) course at UC San Diego (IRB #140608) 
to use an instrumented version of the [ocaml-top] editor, which logged
each of their interactions with the top-level system while they worked
on the first three homework assignments.

[ocaml-top]: https://www.typerex.org/ocaml-top.html


Raw Data
--------

Each JSON file in `data/raw/{sp14/fa15}` contains an object of the
following form on each line. Note that this means the file as a whole is
NOT valid JSON, each line must be parsed individually as a JSON object.

``` JSON
{
    "file": "hw1.ml" | "hw2.ml" | "hw3.ml",
    "time": number,
    "body": string,
    "cursor": number,
    "event": {
        "type": "abort" | "eval" | "stop",
        "region": {
            "start": number,
            "stop": number
        }
    },
    "ocaml": [{
        "in": string,
        "out": string,
        "type": "scope" | "syntax" | "type" | "",
        "min": string
    }]
}
```

The `"file"` field is the name of the file and should be one of
`"hw1.ml"`, `"hw2.ml"`, or `"hw3.ml"`. The `"time"` field is current
UNIX timestamp. The `"body"` field is the contents of the file. The
`"cursor"` field tracks the location of the cursor as an offset into
the body. 

The `"event"` field describes what type of event occurred. The
possibilities for the `"type"` field are:

- `"abort"`: the student aborted the current computation, i.e. sent a
  SIGINT to the top-level.
- `"eval"`: the student sent some definitions to be evaluated. In this
  case the `"event"` field will also contain a `"region"` object with
  `"start"` and `"stop"` offsets into the body, indicating what program
  text was evaluated. 
- `"stop"`: the student restarted the top-level.

In the case of an `"eval"` event, the object will also contain an
`"ocaml"` array, which contains the list of definitions that were sent
to the OCaml top-level. Each item in the list is an object containing:

- `"in"`: a single definition sent to OCaml.
- `"out"`: OCaml's response.
- `"type"`: a classification of the response. `"scope"` indicates an
  unbound variable error, `"syntax"` a syntax error, and `"type"` a type
  error. The empty string implies there was no error.
- `"min"`: in the case of a type error, this field will contain a
  **minimal** program that produces the same error. We were not always
  able to produce such a program, so this field will sometimes be empty.
  
**NOTE:** the offsets into the body (`"cursor"`, `"start"`, and
  `"stop"`) are not always reliable, we have observed cases where they
  do not match up with the actual text that was sent to OCaml.
  

Derived Data
------------

We include two derived datasets.

1. A collection of distinct, ill-typed programs, located in
   `data/derived/{sp14,fa15,comb}/prog`, extracted from the SP14
   (resp. FA15 and combined) dataset. The programs are further grouped
   into `cnstr` and `unify` folders. The `cnstr` folder contains
   programs with errors that are explained pretty well by OCaml
   (e.g. "This constructor takes 3 arguments but only 2 were supplied").
   The `unify` folder contains more general unification errors, which is
   the vast majority.

2. A collection of ill-typed programs paired with their subsequent
   **fixes**, located in `data/derived{sp14,fa15}/pairs.json`. These
   files are again a sequence of JSON objects on each line, with the
   following structure:
   
   ```JSON
   {
       "index": number,
       "hw": "hw1" | "hw2" | "hw3",
       "problem": string,
       "bad": string,
       "fix": string
   }
   ```

