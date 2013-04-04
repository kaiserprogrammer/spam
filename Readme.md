# Spam

Peter Seibel's implementation of spam and ham classification with a bayesian filter.
Altered to fit my style of programming.

## Usage

```lisp
(let ((*spam-db* (make-instance 'memory-db)))
  (train "what the hell" :ham)
  (classify "normal hell text"))
```
