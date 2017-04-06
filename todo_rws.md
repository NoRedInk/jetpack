## moving to RWS

* remove progress bar
* remove free?
* change task to be RWSC

data Step = Compiled FilePath | Resolved Dependency | Concated FilePath
data Progress = [Step]

data Log = Info Text | Warning Text | Error Text
newtype Environement = (CliArguments, Config)

type Task = RWSC Environement Log Progress (ExceptT [Error] IO a)

### why not RWST?

because we run stuff in parallel. RWSC is backed by TVar.

### why not free?

* because we have state and env.
* because we don't reuse the dsl
* logging interpreter doesn't log interesting things.

### advantages of this

* reporting progress from all places
* logging from interesting places
* throwing errors from everywhere
