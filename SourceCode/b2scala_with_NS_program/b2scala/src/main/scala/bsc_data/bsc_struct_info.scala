/***********************************************************************

 Copyright (c) 2024 Jean-Marie Jacquet and the CoordiNam Lab members
 (University of Namur, Belgium)
 
 Permission is hereby granted, free of charge, to any person obtaining
 a  copy of  this  software and  associated  documentation files  (the
 "Software"), to  deal in the Software  without restriction, including
 without limitation the  rights to use, copy,  modify, merge, publish,
 distribute, sublicense,  and/or sell copies  of the Software,  and to
 permit persons to whom the Software is furnished to do so, subject to
 the following conditions:
 
 The  above  copyright notice  and  this  permission notice  shall  be
 included in all copies or substantial portions of the Software.
 
 THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
 EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
 MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
 NONINFRINGEMENT. IN NO  EVENT SHALL THE AUTHORS  OR COPYRIGHT HOLDERS
 BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY, WHETHER  IN AN
 ACTION OF  CONTRACT, TORT OR  OTHERWISE, ARISING  FROM, OUT OF  OR IN
 CONNECTION WITH  THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
 SOFTWARE.
 
************************************************************************/


package bscala.bsc_data

abstract class SI_Term {
   def bsc_toString = toString
}

case class BSC_Token(name:String) extends SI_Term {
   override def bsc_toString: String = { name }
}

object Token {
  
  /** Create a token as an instance of the case class of BSC_Token.
   * 
   *  @param value the name of the token.
   *  @return the case class token. 
  */
  def apply(value:String) = new BSC_Token(value)
       
}


