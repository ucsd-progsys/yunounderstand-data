A Collection of Novice Interactions with the OCaml Top-Level System [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.806813.svg)](https://doi.org/10.5281/zenodo.806813)
==============

We recruited around 50 students each across two instances of the CSE 130
(Undergraduate Programming Languages) course at UC San Diego (IRB #140608) 
to use an instrumented version of the [ocaml-top] editor, which logged
each of their interactions with the top-level system while they worked
on the first three homework assignments.

[ocaml-top]: https://www.typerex.org/ocaml-top.html

We have released this data as a [CC0] work, you are free to use it
for your own research or however else you please. We just ask that if
your use leads to a publication, please cite the dataset as follows.

[CC0]: https://creativecommons.org/publicdomain/zero/1.0/

``` bibtex
@misc{yunounderstand,
  author       = {Eric L Seidel and Ranjit Jhala},
  title        = {A Collection of Novice Interactions with the {OCaml} {Top-Level} System},
  month        = jun,
  year         = 2017,
  doi          = {10.5281/zenodo.806814},
  url          = {https://doi.org/10.5281/zenodo.806814}
}
```


Raw Data
--------

Each JSON file in `data/raw/{sp14,fa15}/` contains an object of the
following form on each line. Note that this means the file as a whole is
NOT valid JSON, each line must be parsed individually as a JSON object.

```
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
`"hw1.ml"`, `"hw2.ml"`, or `"hw3.ml"`. The `"time"` field is the UNIX
timestamp of the event. The `"body"` field is the contents of the file
at that point. The `"cursor"` field tracks the location of the cursor as
an offset into the body.

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
- `"out"`: OCaml's response. We only captured the error responses, so
  this field will often be empty.
- `"type"`: a classification of the response. `"scope"` indicates an
  unbound variable error, `"syntax"` a syntax error, and `"type"` a type
  error. The empty string implies there was no error.
- `"min"`: a self-contained program with the **minimal** set of definitions. 
  This is extremely useful because the students were interacting with the OCaml
  **interpreter** rather than the compiler, and would send individual (groups of)
  definitions to the interpreter rather than compiling the entire file. Thus,
  we cannot expect the contents of the `"in"` field to constitute a complete,
  **closed** program.

  We were not always able to produce such a minimal program, so this field 
  will sometimes be empty.
  
**NOTE:** the offsets into the body (`"cursor"`, `"start"`, and `"stop"`)
are not always reliable, we have observed cases where they do not match up 
with the actual text that was sent to OCaml.
  

Derived Data
------------

We include two derived datasets.

1. A collection of distinct, minimal (i.e. derived from the `"min"`
   field above), ill-typed programs, located in
   `data/derived/{sp14,fa15,comb}/prog`, extracted from the SP14
   (resp. FA15 and combined) dataset. The programs are further grouped
   into `cnstr` and `unify` folders. The `cnstr` folder contains
   programs with errors that are explained pretty well by OCaml
   (e.g. "This constructor takes 3 arguments but only 2 were supplied").
   The `unify` folder contains more general unification errors, which is
   the vast majority.
   
   For convenience, each `.ml` file is paired with a `.ml.out` file
   containing the OCaml compiler's error message, and a `.orig.ml` file
   containing the unminimized program (i.e. the `"body"` field above).
   
   These programs are produced by the python3 script `scripts/extract_programs.py`.
   We provide Makefile targets `progs{-fa15,-sp14,-comb}` for convenience.

2. A collection of ill-typed programs paired with their subsequent
   **fixes**, located in `data/derived{sp14,fa15}/pairs.json`. These
   files are again a sequence of JSON objects on each line, with the
   following structure:
   
   ```
   {
       "index": number,
       "hw": "hw1" | "hw2" | "hw3",
       "problem": string,
       "bad": string,
       "fix": string
   }
   ```
   
   The `"hw"` and `"problem"` fields specify which homework and problem
   the student was working on. The `"bad"` field contains the ill-typed
   program, and the `"fix"` field contains the student's fix. We define
   a "fix" to an ill-typed program as the first subsequent program the 
   student submitted to the interpreter that 
   (1) we can determine to be solving the same homework problem, and 
   (2) has the correct type. 
   Both `"bad"` and `"fix"` contain the minimal programs from the `"min"` field above.
   
   We determine which homework problem a program is solving by looking
   at the names of the defined functions (this works because the
   programs are already minimized). We determine whether a fix is valid
   by checking that it has the expected type (we, of course, know the
   expected types of all homework programs).

   These programs are produced by the python3 script `scripts/extract_pairs.py`.
   We provide Makefile targets `pairs{-fa15,-sp14}` for convenience.
   **NOTE:** `extract_pairs.py` expects `ocaml` to be on your `PATH` as it will
   check that the "fixes" typecheck against the expected type for each homework
   problem.
