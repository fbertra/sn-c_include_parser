package cl.fbd.c_include_file_parser

import scala.util.parsing.combinator._

trait CType 
trait BaseCType extends CType
case class PrimitiveCType (name: String, isUnsigned: Boolean = false, modifier: Int = 0 /* 0: no modifier 1: short, 2: long */) extends BaseCType
case class StructCType (name: String) extends BaseCType
case class TypeDefinedCType (name: String) extends BaseCType
case class PointerCType (typePointedTo: CType) extends CType

case class Pointer ()

case class CParameter (name: String, ctype: CType)

case class CParameterList (parameters: List [CParameter])


trait CDefinition

case class CFunction (name: String, returnType: CType, parametersList: CParameterList) extends CDefinition

case class CTypedef (name: String, ctype: CType) extends CDefinition

/*
 * 
 */

object Main extends RegexParsers {
  def main (args: Array[String]) {
    /*
    if (args.length < 1) {
      println ("use: <file.h>")
      System.exit(1)
    }
    
    val nameOfIncludeFile = args(0)
    
    if (! nameOfIncludeFile.endsWith(".h")) {
      println ("Name of C include file should end with an .h extension")
      System.exit(1)
    }
    
    println ("Processing C include file: " + nameOfIncludeFile)
    * 
    */
    
    /*
      void f (int* a, int b, double d) ;
      void ff (int* a, int** b, double*** d) ;
      void* g (int a) ;
      void h () ;
      struct astruct j (int a) ;
      struct astruct* k (int a) ;
      
      typedef int _int_0 ; 
      typedef short int _int_3 ; 
      typedef long int _int_4 ; 
      typedef unsigned int _int_1 ; 
      typedef unsigned short int _int_2 ; 
      typedef unsigned long int _int_3 ;
       
      typedef char _char_0 ; 
      typedef signed char _char_1 ; 
      typedef unsigned char _char_2 ;
      
      struct _IO_marker {
        struct _IO_marker *_next;
        struct _IO_FILE *_sbuf;
       int _pos;
      };
       
     */
    
    val sz =
      """
        extern FILE *fopen (const char *__restrict __filename,
           const char *__restrict modes) ;
      """

    /*
    parse(typedef, sz) match {
      case Success(matched,_) => println(matched.toString ())
      case Failure(msg,_) => println("FAILURE: " + msg)
      case Error(msg,_) => println("ERROR: " + msg)
    }
    * 
    */

    parse(function, sz) match {
      case Success(matched,_) => println(matched.toString ())
      case Failure(msg,_) => println("FAILURE: " + msg)
      case Error(msg,_) => println("ERROR: " + msg)
    }
    
    /*
    parse(definitions, sz) match {
      case Success(matched,_) => println(matched.foreach (f => println (f.toString ())))
      case Failure(msg,_) => println("FAILURE: " + msg)
      case Error(msg,_) => println("ERROR: " + msg)
    }
    * 
    */
    
    ()
  }
  
  /*
   * parser methods
   */
  
  def definitions: Parser[List[CDefinition]] = definition*
  
  def definition: Parser[CDefinition] = typedef | function
  
  // function
  
  def function: Parser[CFunction] = "extern" ~> ctype ~ word ~ parameterList ~ functionAttr.? <~ ";" ^^ { 
    case cType ~ nameFunction ~ parameters ~ attr => CFunction (nameFunction, cType, parameters) 
  }      
  
  def parameterList: Parser[CParameterList] = "(" ~> repsep (parameter, ",") <~ ")" ^^ { 
    case list => CParameterList (list)
  }   
  
  def parameter: Parser[CParameter]  = constParameter | baseParameter 
  
  def constParameter: Parser[CParameter]  = "const" ~> baseParameter
  
  def baseParameter: Parser[CParameter]  = ctype ~ word ^^ {  
    case nmaeCtype ~ name => CParameter (name, nmaeCtype)
  }
  
  def functionAttr: Parser[String] =  "__attribute__" ~ "((" ~> repsep (word, ",") <~ "))" ^^ { 
    case list => "ignore"
  }   
  
  // typedef
  
  def typedef: Parser[CTypedef] = "typedef" ~> ctype ~ word <~ ";"  ^^ { 
    case cType ~ name => CTypedef (name, cType) 
  }      
  
  // types
  
  def ctype: Parser[CType] = basectype ~ repsep (pointer, "") ^^ { 
    case basetype ~ list => {
      def inner (l: List[Pointer]) : CType = { 
        if (l.isEmpty) 
          basetype 
        else 
          PointerCType (inner (l.tail))
      }
     
      inner (list)
    }
  }
  
  def basectype: Parser[BaseCType] = structCType | primitiveCType | typeDefinedCType
  
  def structCType: Parser[StructCType] = "struct" <~ word ^^ {
    case name => StructCType (name)
  }
  
  def primitiveCType = unsignedPrimitiveCType | modifiedPrimitiveCType | basePrimitiveCType 
  
  def unsignedPrimitiveCType: Parser[PrimitiveCType] = ("unsigned" | "signed" ) ~ maybeModifiedPrimitiveCType ^^ {
    case sign ~ modifiedprimitivectype => {
      val _isUnsigned = "unsigned" == sign
      
      modifiedprimitivectype.copy (isUnsigned = _isUnsigned)
    }
  }
  
  def maybeModifiedPrimitiveCType: Parser[PrimitiveCType] = modifiedPrimitiveCType | basePrimitiveCType 
  
  def modifiedPrimitiveCType: Parser[PrimitiveCType] = ("short" | "long" ) ~ basePrimitiveCType ^^ {
    case modifier ~ baseprimitivectype => {
      val _modifier = if ("short" == modifier) 1 else 2
      
      baseprimitivectype.copy (modifier = _modifier)
    }
  }
  
  def basePrimitiveCType: Parser[PrimitiveCType] = ("char" | "void" | "int" | "float" | "double") ^^ {
    case name => PrimitiveCType (name)
  }
  
  def typeDefinedCType: Parser[TypeDefinedCType] = word ^^ {
    case name => TypeDefinedCType (name)
  }
  
  def pointer: Parser[Pointer] = ("*__restrict" | "*") ^^ { case _ => Pointer () }
  
  // TO-DO
  def word: Parser[String]   = """([a-z]|[A-Z]|[_])([a-z]|[A-Z]|[0-9]|[_])+""".r ^^ { _.toString } // 
}