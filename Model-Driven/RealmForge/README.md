# Assignment 2 - File Guide

## Files Created for Your Assignment

### Main Submission Files

1. **assignment2_complete.oz** ⭐
   - This is your MAIN submission file
   - Contains ALL the code needed for the assignment in one file
   - Includes List functions, MDC implementation, Expression Tree converter, and theory answers in comments
   - Submit this if you need to submit a single .oz file

2. **assignment2_documentation.md** 📝
   - Complete documentation with all theory answers
   - Nicely formatted for PDF conversion if needed
   - Contains detailed explanations and answers for Task 3

### Modular Files (if you prefer separate files)

3. **List.oz**
   - Helper list functions from Assignment 1
   - Can be included with `\insert 'List.oz'`

4. **assignment2.oz**
   - Core implementation without List functions
   - Use with List.oz via `\insert`

5. **task3_theory.md**
   - Theory answers for Task 3 in markdown format

### Testing Files

6. **test_assignment2.oz**
   - Comprehensive test suite
   - Run this to verify all functions work correctly
   - Shows expected outputs for all tasks

## How to Use

### Option 1: Single File Submission (Recommended)
```oz
% In Mozart/Oz, just load:
\insert 'assignment2_complete.oz'

% Then test with:
{System.show {Interpret {Tokenize {Lex "1 2 + 3 *"}}}}
```

### Option 2: Modular Approach
```oz
% Load both files:
\insert 'List.oz'
\insert 'assignment2.oz'

% Then use the functions
```

### Running Tests
```oz
% To run all tests:
\insert 'test_assignment2.oz'
```

## What to Submit on Blackboard

According to the assignment requirements:

1. **Required**: `assignment2_complete.oz` (working Oz code)
2. **Optional**: `assignment2_documentation.md` or PDF with theory answers
3. **Do NOT zip the files** (as per instructions)

## Quick Test Examples

```oz
% Task 1: MDC
{Interpret {Tokenize {Lex "1 2 + 3 *"}}}  % Returns [6]

% Task 2: Expression Tree
{ExpressionTree {Tokenize {Lex "2 3 + 5 /"}}}  % Returns divide(number(5) plus(number(2) number(3)))
```

## Important Notes

- All required functions are implemented ✅
- High-level descriptions included ✅
- Theory questions answered ✅
- Code is tested and working ✅

Good luck with your submission!