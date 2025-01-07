# Compiler 2024F Final Report
Team members:
+ 110590002 王熯紘 
+ 110590004 林奕廷
+ 110590017 陳姿安

## Technical Choices

We implemented this mini-Python compiler using a direct, straightforward approach: compiling each statement and expression from our typed AST (Abstract Syntax Tree) directly to the corresponding x86-64 assembly code without any optimization passes. Despite its simplicity, this solution passes all the provided tests.

### Motivation Behind the Direct Compilation

1. **Deadline Constraints**  
   We began this project very early, at a stage where we had not yet covered the optimization techniques introduced in Chapter 10. Our priority was to ensure we had a fully functional compiler before multiple deadlines started to overlap. As a result, we focused on correctness first, deferring advanced optimizations to a later stage if time permitted.

2. **Simplicity Over Complexity**  
   By compiling each construct (for example, a binary operation, a function call, or a list creation) as a small, self-contained sequence of assembly instructions, we greatly simplified the compiler design. This “function-per-expression” style also made the code easier to write and debug in a short timeframe.

### Conclusion

In summary, our mini-Python compiler demonstrates how a direct translation from a typed AST to x86-64 assembly can be both straightforward and effective in terms of functionality. However, the lack of optimization means that the generated code may not perform efficiently in complex scenarios. If we had more time, we would introduce multiple intermediate representations to facilitate advanced optimizations such as instruction selection, register allocation, and better memory management.
