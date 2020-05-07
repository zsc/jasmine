# jasmine
A distributed make in [JoCaml](http://jocaml.inria.fr/)

Concretely, maps building to multiple remote machines, and doing P2P copy of relevant files when necessary.
```
    type cmd = 
      | TFetch of slave * filename* filename (* Fetching files from a slave machine *)
      | TDo_Ace of int * slave * filename* filename (* Run the payload task *)
      | TCmd of string (* RPC used for quering machine status etc. *)
```
