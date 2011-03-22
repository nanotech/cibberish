cibberish
=========

_Random C Type Generator_

A sample of its output should provide sufficient example of what this program does:

    int main(int argc, char const* argv[])
    {
        { volatile unsigned char(*const l)(long int); }
        { double dk; }
        { volatile char(*volatile ewx)(); }
        { volatile signed char knm_; }
        { extern volatile double(*volatile zpzev)(); }
        { const unsigned char(*ct)(unsigned short int, signed char); }
        { unsigned int(*volatile lbhhnr_m)(); }
        { volatile const double mtekblpxo; }
        { const unsigned char**const volatile*const*const**zafdlykp_bat; }
        { extern signed char gtoxqxnnogaxqae; }
        { short int jhk_caqeeqlwdici; }
        return 0;
    }

And here's one of the longer outputs (with hard line breaks for your convenience):

    extern const signed long int(*const(*volatile x)(const signed int*const,
    const unsigned char(*(*volatile**volatile(*)(const signed
    int**const(***)(unsigned char(**)(unsigned long int), const unsigned int),
    const char(*volatile*)(volatile signed long int, volatile const
    double)))())()))();

Why?
----

Well, why not? Actually, it was inspired by [cdecl][cdecl], although
producing much less helpful results. Maybe you could print the output
onto t-shirts?

[cdecl]: http://cdecl.org/

Further Improvements
--------------------

Here are a few ideas that could be implemented, listed in ascending
estimated difficulty:

* Arrays. Should be a straightforward addition to CType.
* Inline structs and enums, similar to arrays. (unions too?)
* Initializers (and statements).
* Plain (non-pointer) function types. There is already support for
  printing these, but there would need to be a separate case in the
  CType generator for the root level so it wouldn't end up generating
  return types or arguments with direct functions in them.
* typedefs. Would require some sort of order dependency.
* Full imperative program generation. This would be able to create a
  valid C program, complete with function calls, conditionals,
  arithmetic, and pointer referencing and dereferencing. Bonus points
  for using malloc and free with correct memory management.
* Make something useful out of this and use it for [Genetic
  Programming][gp].

[gp]: http://en.wikipedia.org/wiki/Genetic_programming
