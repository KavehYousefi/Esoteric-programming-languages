/***********************************************************************
 * 
 * This program implements an interpreter for the esoteric programming
 * language "Deadfish", invented by Jonathan Todd Skinner and presented
 * on December 10th, 2006, the kenspeckle proprium of which wones in its
 * integer-valued accumulator as four instructions' cynosure, queried
 * from the standard input in a perpetual iterance cycle, while
 * adhibiting to faulty situations no other ultimity than a simple
 * newline output respondency.
 * 
 * 
 * Concept
 * =======
 * The Deadfish programming language ostends a rudimentation of both
 * syntaxis and operative circumference, its foundry that of four
 * instructions to manipulate and output an integer-valued accumulator,
 * queried and handled inwith a perpetual iteration's scope, while any
 * non-instruction token's participation instigates a newline output in
 * lieu of an error's inroad.
 * 
 * == DEADFISH: AN AGNOMINATION TO COMMUNICATE JOUISSANCE ==
 * The language's stevening as "Deadfish", succeeding the originally to
 * the author's deliberation delivered "fishheads" designation, shall
 * limn in diction the programmatic "entertainment" issuing from the
 * restricted and kenspeckle warklume's usance.
 * 
 * == THE MEMORY: A SINGLE REGISTER ==
 * The entirety of program memory accommodated for the developer does
 * not ascend aboon a single register, the "accumulator"; this unit
 * reserved for a signed integer's inclusion, the lateralities
 * appertaining to the type being theoretically bourneless in their
 * expansion, while the incipial state prescribes the zero (0) object.
 * 
 * Two conspicable exemptions from the liberal mode of its castaldy, the
 * accumulator, if having acquired the value of -1 or 256, will return
 * to the pristine state of zero (0) in the next cycle's prevenance.
 * 
 * == INSTRUCTIONS ARE REPRESENTED BY SINGLE SYMBOLS ==
 * Deadfish's quadruple instruction roster apportions merely to Latin
 * letters --- irregardless of their presentation as minuscules or
 * through a majuscular exposition --- an agency, these tallying the
 * "d", "i", "o", and "s" members; any symbol not admitted an
 * epiphenomenal bailiwick reverberates in a single newline output's
 * issuance.
 * 
 * == PROGRAMS OPERATE IN A PERPETUAL CYCLE ==
 * A component of interactivity wones in the program's execution by its
 * perpetual request and procession cycle.
 * 
 * At the program's inchoation, the user is queried for a line of input,
 * preceded by the prompt message ">> ":
 * 
 *   >> 
 * 
 * An immediate consequence to the submission from the runtime system's
 * laterality, every character from this response is processed, either
 * adhibiting a causatum to a recognized instruction, or responding with
 * a newline display for a superfluous entity.
 * 
 * An ultimity from the desinent symbol's traversal, the cycle iterum
 * commences with the prompt message, input request, and evaluation.
 * 
 * == A PURLICUE OF DEADFISH'S EXECUTION MODEL ==
 * A summary shall attend to a more stringent and compendious
 * elucidation's dation anent the execution principle of a Deadfish
 * program:
 * 
 *   let accumulator <- 0
 *   
 *   repeat infinitely do
 *     print ">> "
 *     
 *     let userInput <- query line from standard input
 *     
 *     for character c in userInput do
 *       if c = "i" then
 *         accumulator <- accumulator + 1
 *       else if c = "d" then
 *         accumulator <- accumulator - 1
 *       else if c = "s" then
 *         accumulator <- accumulator * accumulator
 *       else if c = "o" then
 *         print accumulator
 *       else
 *         print newline
 *       end if
 *       
 *       { Normalize the accumulator on two special cases. }
 *       if (accumulator = -1) or (accumulator = 256) then
 *         accumulator <- 0
 *       end if
 *     end for
 *   end repeat
 * 
 * 
 * Architecture
 * ============
 * The edification that relates of Deadfish's architectural designment
 * betokens the lealty to eath comprehension, proffering a single
 * register, or "accumulator", the capacity of which yet does not wist
 * of an extremum at either of the signed dispansions, its original
 * state the value zero (0).
 * 
 * A peculiar haecceity's woning entertains the attendance to a twissel
 * of states that the accumulator may assume; scilicet, if either the
 * value -1 or 256 is stored in the salvatory, its state automatically
 * relapses to its inchoation, the neutral zero (0) object.
 * 
 * 
 * Data Types
 * ==========
 * Deadfish's type system produces an affedavit to its homespun tenets
 * the same embue the architecture: Merely signed integers, albeit
 * disencumbered from any bournes' imposition along both axes of
 * polarity, participate in a program's expression.
 * 
 * 
 * Syntax
 * ======
 * From a vista upon its donet, a Deadfish program's tolerance enhalses
 * any symbol, the veridical causata's adhibition, maugre this proprium,
 * retaining their effective scope only in the context of the quadruple
 * instructions, each represented by an aefault Latin letter without
 * imperatives concerning the distinguishment betwixt majuscular and
 * minuscular presentation.
 * 
 * 
 * Instructions
 * ============
 * Deadfish's operative competences enumerate a quadruple contingency,
 * the circumference of which applies itself to basic arithmetic
 * modulations of the accumulator, with an singular output facility's
 * nuncupation to the storage's display.
 * 
 * The reserved identifiers for this cause may be both employed in their
 * minuscular as well as majuscular guise.
 * 
 * Any character disqualified for a causatum's affiliation instigates a
 * single newline entity's printing on the standard output.
 * 
 * == OVERVIEW ==
 * An apercu shall be administered to the encheson of the operations'
 * explication:
 * 
 *   ------------------------------------------------------------------
 *   Command | Effect
 *   --------+---------------------------------------------------------
 *   i       | Increments the accumulator by one (1). If the new value
 *   --------| equals -1 or 256, its state is reset to zero (0).
 *   I       | 
 *   ..................................................................
 *   d       | Decrements the accumulator by one (1). If the new value
 *   --------| equals -1 or 256, its state is reset to zero (0).
 *   D       | 
 *   ..................................................................
 *   s       | Squares the accumulator by multiplying its value by
 *   --------| itself. If the new value equals -1 or 256, its state is
 *   S       | reset to zero (0).
 *   ..................................................................
 *   o       | Prints the accumulator state in its verbatim numeric
 *   --------| form to the standard output.
 *   O       | 
 *   ------------------------------------------------------------------
 * 
 * 
 * Implementation
 * ==============
 * This interpreter has been implemented in the programming language
 * Objective-C, its haecceity a dedication to simplicity as the
 * paravaunt virtue.
 * 
 * == COMPILATION AND EXECUTION ==
 * The interpreter's development and deployment proceeded in the context
 * of the program "Cygwin" on a "Microsoft Windows 10" machine, the
 * warklume of its compilation and execution the "gcc" tool.
 * 
 * Pursuing the telos of this Objective-C program's compilation, the
 * following input, imputing a current sojourn in the commorancy of the
 * "Deadfish_001.m" source file's directory, ought to be entered in
 * Cygwin:
 * 
 *   gcc -I "c:/GNUstep/GNUstep/System/Library/Headers"   \
 *       -L "c:/GNUstep/GNUstep/System/Library/Libraries" \
 *       -o Deadfish_001 Deadfish_001.m                   \
 *       -lobjc                                           \
 *       -lgnustep-base                                   \
 *       -fconstant-string-class=NSConstantString
 * 
 * The successful patration's consequence shall subsequently amount to
 * the thus generated executable file's invocation, yet in the selfsame
 * directory:
 * 
 *   ./Deadfish_001.exe
 * 
 * The same approach as elucidated above has been attempted to a
 * successful patration utilizing the program "GNUstep".
 * 
 * --------------------------------------------------------------------
 * 
 * Author: Kaveh Yousefi
 * Date:   2024-06-14
 * 
 * Sources:
 *   [esolang2024Deadfish]
 *   The Esolang contributors, "Deadfish", June 6th, 2024
 *   URL: "https://esolangs.org/wiki/Deadfish"
 * 
 **********************************************************************/



////////////////////////////////////////////////////////////////////////
// -- Import of libraries.                                         -- //
////////////////////////////////////////////////////////////////////////

#include <stdlib.h>

#import  <Foundation/Foundation.h>



////////////////////////////////////////////////////////////////////////
// -- Declaration of function prototypes.                          -- //
////////////////////////////////////////////////////////////////////////

int main (int argc, char* argv[]);



////////////////////////////////////////////////////////////////////////
// -- Implementation of operations.                                -- //
////////////////////////////////////////////////////////////////////////

/**
 * Launches the Deadfish interpreter, perpetually querying the standard
 * input for an instruction and executing the same.
 */
int main (int argc, char* argv[])
{
  NSAutoreleasePool *pool;
  NSInteger          accumulator;
  char               input;
  
  pool        = [[NSAutoreleasePool alloc] init];
  accumulator = 0;
  
  while (YES)
  {
    printf (">> ");
    input = getchar ();
    
    /* Normalize the accumulator on two special cases to zero (0).    */
    if ((accumulator == -1) || (accumulator == 256))
    {
      accumulator = 0;
    }
    
    switch (input)
    {
      case 'i':
      case 'I':
        accumulator = accumulator + 1;
        break;
      
      case 'd':
      case 'D':
        accumulator = accumulator - 1;
        break;
      
      case 'o':
      case 'O':
        printf ("%d\n", accumulator);
        break;
      
      case 's':
      case 'S':
        accumulator = accumulator * accumulator;
        break;
      
      default:
        printf ("\n");
        break;
    }
  }
  
  [pool drain];
  
  return EXIT_SUCCESS;
}
