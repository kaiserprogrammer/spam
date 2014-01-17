# Spam

Peter Seibel's implementation of spam and ham classification with a bayesian filter.
Altered to fit my style of programming.

## Usage

```lisp
(let ((db (make-instance 'spam:memory-db)))
           (spam:train "what  the hell" :ham db)
           (spam:classify "normal text hell" db))
```
