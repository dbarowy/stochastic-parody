# Stochastic Parody

# NOT DONE YET

##Programming language developed by Matt Laws and Leah Williams

### Stochastic Parody is a programming language that allows for simple creation of songs / parodies build using F#. 

We provide a language Presentation on youtube here: https://youtu.be/QknRtqc2KJs

To learn more about the language see our docs located at docs/specification.pdf

There are 2 ways to evaluate a program written in this language:

* from the solution level (the code/ directory) call dotnet run --project Stochastic-Parody <input.song>
* from the project level (the code/Stochastic-Parody/ directory) call dotnet run <input.song>
* both of these offer an optional second argument that  if set to v or verbose will display the interworkings of the translation


We also provide a test suite for the project located at code/Stochastic-Parody-Tests/ that can be run by calling dotnet test

Example programs are provided in code/examples/

Instructions on how the dictionary was preprocessed is found in code/preproc