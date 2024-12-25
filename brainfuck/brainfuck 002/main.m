/***********************************************************************
 * 
 * This program implements an interpreter for the esoteric programming
 * language "brainfuck", invented by Urban Mueller, the kenspeckle
 * proprium of which appertains to the mickleness in its capacitation,
 * in some variants elevated to an equipollence with the
 * Turing-completeness criteria.
 * 
 * 
 * Concept
 * =======
 * All brainfuck programs operate on a tape of byte-valued cells, its
 * active entity designated by a mobile cell pointer. The instructions
 * are designated by a symbol each, an octuple membership whose
 * potentials entalent it with Turing-completeness.
 * 
 * == THE MEMORY: A TAPE OF BYTES ==
 * The brainfuck programming language is founded upon the manipulation
 * of a tape compact of a bilaterally infinite dispansion of unsigned
 * byte-valued cell, the currently active member among which, endowed
 * with the capacity for indagations and modifications, is selected by
 * a mobile cell pointer.
 * 
 * == OPERATIONS: AN OCTUPLE OF SINGLE-CHARACTER BEHESTS ==
 * All operations, amplecting a tally of eight, are expressed by an
 * identifier whose agnomination includes an aefauld symbol. These
 * facilities comprehend parvipotent arithmetics, memory management,
 * input, output, as well as a jump-based control flow.
 * 
 * == BRAINFUCK AND TURING-COMPLETENESS ==
 * brainfuck's kenspeckle parturition is peccant of bewraying its status
 * as a competent warklume and a polymechany. A programming language
 * imbued with the capacity for an equiparation to this specimen as a
 * consectary chevises its own Turing-completeness' attest, a proprium
 * desumed from brainfuck's personal potence.
 * 
 * 
 * Architecture
 * ============
 * brainfuck's architectural diorism amplects a tape enumerating an
 * infinite tally of cells along both of its axes, each such unit an
 * aefauld unsigned byte's salvatory. A designated and mobile marker,
 * the "cell pointer" applies itself to the current cell's selection,
 * the sole member entalented with a respondency to indagations and
 * manipulations.
 * 
 * == THE TAPE IS A BILATERALLY INFINITE LINE OF CELLS ==
 * A program's data castaldy is realized in a linear arrangement of
 * cells, a tape of infinite dispansion along both of its lateralities.
 * 
 * == EACH CELL COMPREHENDS AN UNSIGNED BYTE SCALAR ==
 * Every cell's amplectation is laid around a single unsigned byte
 * datum, commorant, proceeding from this consectary, in the closed
 * integral range spanning [0, 255].
 * 
 * Initially acquiring the minimum value of zero (0), stepwise
 * incrementations and decrementations may be imposed upon these units.
 * Any of its two bournes' transgressions conditions a wrapping around
 * towards the athwart extremum; that is, if the value descends alow the
 * bottom march of zero (0), the state assumes the maximum of 255;
 * obversely, an excess beyond the upper bourne of 255 relapses to the
 * minimum of zero (0).
 * 
 * == THE CELL POINTER SELECTS THE ACTIVE CELL ==
 * At any instant in the program only one cell may answer to
 * perquisition and modification requests, its status as such particular
 * member being a dependency upon the cell pointer's referral.
 * 
 * Empighted at the execution's inchoation at the first cell, its
 * capacitation for alternating the referred unit is realized in the
 * pointer's amenability to gradual relocations along any of the two
 * tape axes.
 * 
 * 
 * Data Types
 * ==========
 * A bifurcation governs brainfuck's type system and cleaves the same
 * into the paravaunt unsigned byte species and the parhedral character
 * complement, the former commits to the memory and its arithmetics,
 * while the latter's utility is restricted to the communication along
 * the input and output conduits.
 * 
 * == UNSIGNED INTEGERS: THE PREMIER DATA ITEMS ==
 * Its role in the program memory already intimates the unsigned byte
 * species' excellent impact, its gamut covering the extent of the
 * integer interval [0, 255].
 * 
 * == ASCII CHARACTERS: TOKENS OF COMMUNICATION ==
 * The paravail object of deliberation, the character type, appropriates
 * its sole deployment in the form of the ASCII repertoire, operating
 * on the input and output communication channels.
 * 
 * 
 * Instructions
 * ============
 * The operative aspects of brainfuck proceed from an octuple
 * cardinality, its perimeter amplecting basic arithmetics, the memory's
 * castaldy, character-based input and output communications, and a
 * jump-based control flow conduit.
 * 
 * == OVERVIEW ==
 * A cursory but compendious apercu's dation shall be a basic mete of
 * gnarity anent the language's instructions:
 * 
 *   ------------------------------------------------------------------
 *   Command | Effect
 *   --------+---------------------------------------------------------
 *   +       | Increments the current cell by one (1). If the new state
 *           | transcends the upper extremum of 255, the value returns
 *           | to the minimum of zero (0).
 *   ..................................................................
 *   -       | Decrements the current cell by one (1). If the new state
 *           | transcends the lower extremum of zero (0), the value
 *           | wraps around to the maximum of 255.
 *   ..................................................................
 *   >       | Translates the cell pointer one step to the right.
 *   ..................................................................
 *   <       | Translates the cell pointer one step to the left.
 *   ..................................................................
 *   .       | Prints the character whose ASCII code equals the current
 *           | cell value to the standard output.
 *   ..................................................................
 *   ,       | Queries the standard input for a character and stores
 *           | its ASCII code in the current cell.
 *   ..................................................................
 *   [       | If the current cell value equals zero (0), moves the
 *           | instruction pointer (IP) forward to the position
 *           | immediately succeeding the matching "]" instruction;
 *           | otherwise proceeds as usual.
 *   ..................................................................
 *   ]       | If the current cell value does not equal zero (0), moves
 *           | the instruction pointer (IP) back to the position
 *           | immediately succeeding the matching "[" instruction;
 *           | otherwise proceeds as usual.
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
 * "main.m" source file's directory, ought to be entered in Cygwin:
 * 
 *   gcc -I "c:/GNUstep/GNUstep/System/Library/Headers"   \
 *       -L "c:/GNUstep/GNUstep/System/Library/Libraries" \
 *       -o brainfuckInterpreter main.m                   \
 *       -lobjc                                           \
 *       -lgnustep-base                                   \
 *       -fconstant-string-class=NSConstantString
 * 
 * The successful patration's consequence shall subsequently amount to
 * the thus generated executable file's invocation, yet in the selfsame
 * directory:
 * 
 *   ./brainfuckInterpreter.exe
 * 
 * The same approach as elucidated above has been attempted to a
 * successful patration utilizing the program "GNUstep".
 * 
 * 
 * Lacunae in the Specification
 * ============================
 * Maugre maturity being a tenable imputation in regards to the
 * language's senescence, an account of unresolved inquisitions retain
 * their woning in brainfuck's specification, a subset therefrom shall
 * receive the following sections' perquisitions.
 * 
 * == WHAT CONCRETE MEMORY SPECIFICATIONS EXERCISE THEIR PURVIEW? ==
 * Urban Mueller's original brainfuck implementation expressed itself in
 * a particular program memory accommodation, enumerating 30,000 cells
 * of unsigned byte capacity each. An adherence's dever is, however,
 * neither declaimed nor dismissed.
 * 
 * Several different interpretations are allotted to the quesited
 * property of the memory, frequently imposing inconcinnities and
 * incongruencies betwixt implementations, without an explicit
 * disqualification's infliction, tallying variations upon the cell
 * count towards a bilateral infinity, as well as the admission of
 * signed octet or even integer gamuts for the cells.
 * 
 * It has been adjudged to resort to a prescription of an infinite
 * extent of cells, its apertures empighted on both lateralities, where
 * every cell acquires the castaldy over its personal unsigned byte
 * value.
 * 
 * --------------------------------------------------------------------
 * 
 * Author: Kaveh Yousefi
 * Date:   2024-06-21
 * 
 * Sources:
 *   [esolang2024brainfuck]
 *   The Esolang contributors, "brainfuck", July 2nd, 2024
 *   URL: "https://esolangs.org/wiki/Brainfuck"
 *   
 *   [stackoverflow2012q11221902]
 *   The Stack Overflow contributors,
 *     "Objective C NSStack and NSQueue?", June 27th, 2012
 *   URL: "https://stackoverflow.com/questions/11221902/
 *         objective-c-nsstack-and-nsqueue"
 *   Notes:
 *     - Describes the utilization of "NSMutableArray" as a stack.
 *     - Offers the "NSMutableArray" methods
 *         addObject
 *         lastObject
 *         removeLastObject
 *       for this purpose.
 *     - Claims efficiency in these operations' implementations.
 *   
 *   [stackoverflow2012q11352486]
 *   The Stack Overflow contributors,
 *     "NSDictionary With Integer Values", July 5th, 2012
 *   URL: "https://stackoverflow.com/questions/11352486/
 *         nsdictionary-with-integer-values"
 *   Notes:
 *     - Describes the usance of integer numbers (of the "int" type) as
 *       keys and values in an NSDictionary or NSMutableDictionary.
 *     - Mentions that NSDictionary and NSMutableDictionary expect the
 *       keys and values to be of the "NSObject" type.
 *     - As a corollary, in order to insert numeric primitives, the same
 *       ought to be wrapped into an "NSNumber" objects ere their
 *       transfer into the dictionary.
 *     - Demonstrates the insertion of such keys and values in a
 *       dictionary, as well as their retrieval and conversion back into
 *       "int" primitives.
 *   
 *   [tutorialspoint2024objcdataencaps]
 *   Tutorials Point, "Objective-C Data Encapsulation", 2024
 *   URL: "https://www.tutorialspoint.com/objective_c/
 *         objective_c_data_encapsulation.htm"
 *   Notes:
 *     - Demonstrates the definition and implementation of interfaces.
 *     - Demonstrates the definition of fields in classes.
 * 
 **********************************************************************/



////////////////////////////////////////////////////////////////////////
// -- Import of libraries.                                         -- //
////////////////////////////////////////////////////////////////////////

#include <stdlib.h>

#import  <Foundation/Foundation.h>



////////////////////////////////////////////////////////////////////////
// -- Declaration of types.                                        -- //
////////////////////////////////////////////////////////////////////////

typedef char OCTET;



////////////////////////////////////////////////////////////////////////
// -- Definition of "Memory" interface.                            -- //
////////////////////////////////////////////////////////////////////////

/**
 * Models the brainfuck program memory as a theoretically infinite
 * dispansion of unsigned byte-valued cells, amenable to a mobile cell
 * pointer which at any instant designates the currently active unit.
 */
@interface Memory : NSObject
  {
    NSMutableDictionary *cells;
    NSNumber            *cellPointer;
  }
  
  
  /**
   * Creates and returns a "Memory" instance whose cells all assume the
   * initial state of zero (0).
   */
  - (id) initEmptyMemory;
  
  /**
   * Ascertains an entry's existence for the cell designated by the
   * cell pointer by storing a zero-valued unit upon its absence.
   * 
   * Please heed that this operation is intended as an internal warklume
   * rather than for public utilizations.
   */
  - (void) ensureCurrentCellExists;
  
  /**
   * Returns the byte value stored in the cell under the cell pointer.
   */
  - (OCTET) getCurrentCellValue;
  
  /**
   * Stores the "newValue" in the cell under the pointer, contingently
   * preceded by wrapping its value around in order to accommodate the
   * imposed byte range for cells.
   */
  - (void) setCurrentCellValue: (int) newValue;
  
  /**
   * Increments the cell under the pointer by an amount of one (1),
   * contingently wrapping its state around in order to ascertain lealty
   * to the homologated byte-valued range.
   */
  - (void) incrementCurrentCell;
  
  /**
   * Decrements the cell under the pointer by an amount of one (1),
   * contingently wrapping its state around in order to ascertain lealty
   * to the homologated byte-valued range.
   */
  - (void) decrementCurrentCell;
  
  /**
   * Translates the cell pointer one step to the left.
   */
  - (void) moveCellPointerLeft;
  
  /**
   * Translates the cell pointer one step to the right.
   */
  - (void) moveCellPointerRight;
@end



////////////////////////////////////////////////////////////////////////
// -- Implementation of "Memory" class.                            -- //
////////////////////////////////////////////////////////////////////////

@implementation Memory
  - (id)    initEmptyMemory
  {
    cells       = [[NSMutableDictionary alloc] init];
    cellPointer = [NSNumber numberWithInt: 0];
    
    return self;
  }
  
  
  - (void)  ensureCurrentCellExists
  {
    if ([cells objectForKey: cellPointer] == nil)
    {
      [cells setObject: [NSNumber numberWithInt: 0]
             forKey:    cellPointer];
    }
  }
  
  - (OCTET) getCurrentCellValue
  {
    NSNumber *boxedCellValue;
    OCTET     cellValue;
    
    [self ensureCurrentCellExists];
    
    boxedCellValue = [cells objectForKey: cellPointer];
    cellValue      = [boxedCellValue intValue];
    
    return cellValue;
  }
  
  - (void)  setCurrentCellValue: (int) newValue
  {
    [cells setObject: [NSNumber numberWithInt: (newValue % 256)]
           forKey:    cellPointer];
  }
  
  - (void)  incrementCurrentCell
  {
    [self setCurrentCellValue: [self getCurrentCellValue] + 1];
  }
  
  - (void)  decrementCurrentCell
  {
    [self setCurrentCellValue: [self getCurrentCellValue] - 1];
  }
  
  - (void)  moveCellPointerLeft
  {
    cellPointer = [NSNumber numberWithInt: [cellPointer intValue] - 1];
  }
  
  - (void)  moveCellPointerRight
  {
    cellPointer = [NSNumber numberWithInt: [cellPointer intValue] + 1];
  }
@end



////////////////////////////////////////////////////////////////////////
// -- Definition of "JumpTable" interface.                         -- //
////////////////////////////////////////////////////////////////////////

/**
 * The "JumpTable" class furnishes a bilateral association betwixt two
 * jump points in a brainfuck program, mediated by adminiculum of their
 * zero-based positions inside the source code.
 */
@interface JumpTable : NSObject
  {
    NSMutableDictionary *connections;
  }
  
  
  /**
   * Creates and returns a new "JumpTable" instance whose jump points,
   * in a bilateral fashion, are extracted from the piece of brainfuck
   * source "code".
   */
  - (id)   initFromBrainfuckCode: (NSString *) code;
  
  
  /**
   * Associates the "startPoint" with the "endPoint" in a bilateral way,
   * both representative of a jump instruction's location inside of the
   * respective brainfuck program.
   */
  - (void) connectStartPoint:     (int) startPoint
                                  withEndPoint: (int) endPoint;
  
  /**
   * Returns the location of the jump point opposite to the "jumpSource"
   * in the underlying brainfuck program.
   */
  - (int)  jumpDestinationFor:    (int) jumpSource;
@end



////////////////////////////////////////////////////////////////////////
// -- Implementation of "JumpTable" interface.                     -- //
////////////////////////////////////////////////////////////////////////

@implementation JumpTable
  - (id) initFromBrainfuckCode: (NSString *) code
  {
    connections = [[NSMutableDictionary alloc] init];
    
    NSUInteger      codeLength;
    unichar        *buffer;
    int             characterIndex;
    NSMutableArray *forwardJumpPoints;
    
    codeLength        = [code length];
    buffer            = malloc (sizeof (unichar) * (codeLength + 1));
    forwardJumpPoints = [[NSMutableArray alloc] init];
    
    [code getCharacters: buffer range: NSMakeRange (0, codeLength)];
    
    for (characterIndex = 0;
         characterIndex < codeLength;
         characterIndex++)
    {
      unichar token;
      
      token = buffer[characterIndex];
      
      if (token == '[')
      {
        [forwardJumpPoints
          addObject: [NSNumber numberWithInt: characterIndex]];
      }
      else if (token == ']')
      {
        NSNumber *startPoint;
        
        startPoint = [forwardJumpPoints lastObject];
        [forwardJumpPoints removeLastObject];
        
        [self connectStartPoint: [startPoint intValue]
              withEndPoint:      characterIndex];
      }
      else
      {
      }
    }
    
    free (buffer);
    
    return self;
  }
  
  
  - (void) connectStartPoint: (int) startPoint
                              withEndPoint: (int) endPoint
  {
    NSNumber *wrappedStartPoint;
    NSNumber *wrappedEndPoint;
    
    wrappedStartPoint = [NSNumber numberWithInt: startPoint];
    wrappedEndPoint   = [NSNumber numberWithInt: endPoint];
    
    [connections setObject: wrappedStartPoint forKey: wrappedEndPoint];
    [connections setObject: wrappedEndPoint forKey: wrappedStartPoint];
  }
  
  - (int) jumpDestinationFor: (int) jumpSource
  {
    NSNumber *oppositePoint;
    
    oppositePoint = [connections objectForKey:
                                 [NSNumber numberWithInt: jumpSource]];
    
    if (oppositePoint != nil)
    {
      return [oppositePoint intValue];
    }
    else
    {
      [NSException
        raise:  @"Invalid jump source"
        format: @"No jump target associated with position %d",
                jumpSource];
    }
  }
@end



////////////////////////////////////////////////////////////////////////
// -- Declaration of function prototypes.                          -- //
////////////////////////////////////////////////////////////////////////

int       main                 (int argc, const char* argv[]);
void      interpretBrainfuck   (NSString *bfCode);
NSString* loadBrainfuckProgram (NSString *programPath);



////////////////////////////////////////////////////////////////////////
// -- Implementation of interpreter.                               -- //
////////////////////////////////////////////////////////////////////////

/**
 * Interprets the piece of brainfuck source code.
 */
void interpretBrainfuck (NSString *bfCode)
{
  NSUInteger  codeLength;
  unichar    *codeBuffer;
  unichar     currentToken;
  int         ip;
  JumpTable  *jumpTable;
  Memory     *memory;
  
  codeLength = [bfCode length];
  codeBuffer = malloc (sizeof (unichar) * (codeLength + 1));
  ip         = 0;
  
  [bfCode getCharacters: codeBuffer
          range:         NSMakeRange (0, codeLength)];
  
  jumpTable  = [[JumpTable alloc] initFromBrainfuckCode: bfCode];
  memory     = [[Memory    alloc] initEmptyMemory];
  
  while (ip < codeLength)
  {
    currentToken = codeBuffer[ip];
    
    switch (currentToken)
    {
      case '+' :
        [memory incrementCurrentCell];
        break;
      
      case '-' :
        [memory decrementCurrentCell];
        break;
      
      case '>' :
        [memory moveCellPointerRight];
        break;
      
      case '<' :
        [memory moveCellPointerLeft];
        break;
      
      case '.' :
        printf ("%c", [memory getCurrentCellValue]);
        break;
      
      case ',' :
        printf ("\n>> ");
        [memory setCurrentCellValue: getchar()];
        break;
      
      case '[' :
        if ([memory getCurrentCellValue] == 0)
        {
          ip = [jumpTable jumpDestinationFor: ip];
        }
        break;
      
      case ']' :
        if ([memory getCurrentCellValue] != 0)
        {
          ip = [jumpTable jumpDestinationFor: ip];
        }
        break;
      
      default :
        break;
    }
    
    ip++;
  }
  
  free (codeBuffer);
}



////////////////////////////////////////////////////////////////////////
// -- Implementation of file handling operations.                  -- //
////////////////////////////////////////////////////////////////////////

/**
 * Loads the contents of the file located under the program path and
 * returns an "NSString" representation thereof.
 */
NSString* loadBrainfuckProgram (NSString *programPath)
{
  NSFileManager *fileLoader;
  NSString      *fileContent;
  NSError       *loadingError;
  
  fileLoader = [NSFileManager defaultManager];
  
  if ([fileLoader fileExistsAtPath: programPath] == YES)
  {
    fileContent = [NSString
                    stringWithContentsOfFile: programPath
                    encoding:                 NSUTF8StringEncoding
                    error:                    &loadingError];
    
    if (loadingError != nil)
    {
      NSLog (@"Error while reading \"%@\": %@",
             programPath, loadingError);
      fileContent = nil;
    }
  }
  else
  {
    NSLog (@"File not found.");
    fileContent = nil;
  }
  
  return fileContent;
}



////////////////////////////////////////////////////////////////////////
// -- Implementation of entry operation.                           -- //
////////////////////////////////////////////////////////////////////////

int main (int argc, const char* argv[])
{
  NSAutoreleasePool *pool;
  
  pool = [[NSAutoreleasePool alloc] init];
  
  if (argc > 1)
  {
    NSString *programPath;
    NSString *brainfuckProgram;
    
    programPath      = [NSString
                         stringWithCString: argv[1]
                         encoding:          NSASCIIStringEncoding];
    brainfuckProgram = loadBrainfuckProgram (programPath);
    
    if (brainfuckProgram != nil)
    {
      interpretBrainfuck (brainfuckProgram);
    }
    else
    {
      printf ("Could not load the file %s.", programPath);
    }
  }
  else
  {
    printf ("Please invoke this interpreter specifying a brainfuck "
            "program's path.");
  }
  
  [pool drain];
  
  return EXIT_SUCCESS;
}
