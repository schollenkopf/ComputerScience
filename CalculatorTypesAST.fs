// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module CalculatorTypesAST





type expr =
  | Num of int
  | Name  of string
  | NAMEArray of (string * expr)
  | TimesExpr of (expr * expr)
  | DivExpr of (expr * expr)
  | PlusExpr of (expr * expr)
  | MinusExpr of (expr * expr)
  | PowExpr of (expr * expr)
  | UPlusExpr of (expr)
  | UMinusExpr of (expr)
  
 

 and command =
  | INIT of (expr * expr)
  | TWOcommand of (command * command)
  | IFFI       of (guardedC)
  | DOOD       of (guardedC)
  | Skip 

and guardedC =
  | Follows   of (boolean * command)
  | TWOguardedC of (guardedC * guardedC)
  

and boolean =
  | Boolean of bool
  | And  of (boolean * boolean)
  | Andand of (boolean * boolean)
  | Or   of (boolean * boolean)
  | Oror of (boolean * boolean)
  | Equal of (expr * expr)
  | NOTEqual of (expr * expr)
  | Not  of (boolean)
  | GREATER of (expr * expr)
  | LESS of  (expr * expr)
  | GREATEREqual of (expr * expr)
  | LESSEqual of (expr * expr)



and action = 
  | Assignment of (expr*expr)
  | Saction       
  | Baction of boolean


and edge =
  | Edge of (node*action*node)
  
and node =
  | Node of string
