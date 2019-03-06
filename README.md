# PLT stuff

This repo is for exercises based on Dave's path to basic PLT/type theory stuff.

Here's the proposed path:

```
   From IRC 2019-03-06
<dalaing> for anyone who wants some language exercises...
12:48 Hutton's Razor is a good start: https://www.codewars.com/kata/huttons-razor
12:48 I'd considering writing a parser as well, if you were going to write a pretty printer
12:49 then I'd add more numeric operators, bools / if-then-else / the operators from Eq and Ord - which brings in the need for a type system
12:50 write the inference and checking, along with parsers and pretty printers for the types
12:51 after that I'd work towards STLC, and then probably intuitionistic linear logic
12:51 for STLC I'd probably try to work out the type rules by staring at the forms of things - ie if you have var, lam and app, what should their types looks like?
12:52 although cheat if you get stuck: https://github.com/dalaing/plt-talk/blob/master/version-2/slides/slides.pdf
12:52 for intuitionistic linear logic, the exercise would be: implement it
12:53 which will involve googling for the type rules / semantics
12:53 and not googling for how to implement it if you want a challenge
12:57 I wouldn't write any of it as a sum of rules like I did for my talk, just write an eval function, a parse function, etc...
13:01 the linear logic bit is where things get a bit interesting, there is plenty to think about there
13:08 <ajmcmiddlin> Andrew Thanks dalaing!
13:10 
<dalaing> David Laing before STLC, maybe add pairs to the language - something that constructs pairs (the introduction form) and the projections fst and snd (the eliminators)
13:11 it also adds a new type, that takes two other types as arguments
13:11 so it is a good warm up
13:11 leaving out the parser and pretty printer is also totally an option
```
