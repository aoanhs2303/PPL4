	/**
*	@author Dr.Nguyen Hua Phung
*	@version 1.0
*	28/6/2006
*	This class is used to generate code at a intermediate level
*
*/
package mc.codegen

import java.io.BufferedWriter
import java.io.FileWriter
import java.text.DecimalFormat
import java.util.Iterator

import mc.utils._
import mc.checker._


class Emitter(filename:String) {
  val buff = new StringBuffer()
	
	
	val jvm = new JasminCode()

  def getJVMType(inType:Type):String = inType match {
    case IntType => "I"
    case FloatType => "F"
    case StringType => "Ljava/lang/String;"
    case BoolType => "Z"
    case VoidType => "V"
    case ArrayType(_,t) => "["+getJVMType(t)
    case ArrayPointerType(t) => "["+getJVMType(t)
    case FunctionType(il,o) => "("+il.foldLeft("")(_+getJVMType(_))+")"+getJVMType(o)
    case ClassType(t) => "L"+t+";"
  }
  def getFullType(inType:Type):String = inType match {
    case IntType => "int"
    case FloatType => "float"
    case BoolType => "boolean"
    case StringType => "java/lang/String"
    case VoidType => "void"
  }

	def emitPUSHICONST(i:Int,frame:Frame):String  =  
	 {
		frame.push();
		if (i >= -1 && i <= 5) jvm.emitICONST(i)
		else if (i >= -128 && i <= 127) jvm.emitBIPUSH(i)
		else if (i >= -32768 && i <= 32767) jvm.emitSIPUSH(i)
		else jvm.emitLDC("" + i) 	
	}

	def emitPUSHICONST(in:String,frame:Frame):String = 
    in match {
      case "true" => emitPUSHICONST(1,frame)
      case "false" => emitPUSHICONST(0,frame)
      case _ => emitPUSHICONST(in.toInt,frame)
    }


	def emitPUSHFCONST(in:String,frame:Frame):String = 
	 {
		val f = in.toFloat;	
		frame.push();
		val myFormatter = new DecimalFormat("###0.0###");
		val rst = myFormatter.format(f);
		if (rst.equals("0.0") || rst.equals("1.0") ||rst.equals("2.0")) 
			jvm.emitFCONST(rst)
	   else
			jvm.emitLDC(in);
	}
	/**
	*	generate code to push a constant onto the operand stack.
	*	@param in the lexeme of the constant
	*	@param typ the type of the constant
	*/
	def emitPUSHCONST(in:String, typ:Type, frame:Frame) = 
	typ match {
      case IntType => emitPUSHICONST(in,frame)
      case FloatType => emitPUSHFCONST(in, frame)
      case BoolType => emitPUSHICONST(in, frame)
      case StringType => {
        frame.push();
        jvm.emitLDC( in);
      }
      case _ => throw IllegalOperandException(in)
    }

        ////////////////////////////////////////////////////////////////

        
    def emitALOAD(in:Type,frame:Frame) =  
	  {
    	//..., arrayref, index -> ..., value
    	frame.pop();
      in match {
        case IntType => jvm.emitIALOAD()

        case (ArrayPointerType(_)|ClassType(_)|StringType) => jvm.emitAALOAD()
        case FloatType => jvm.emitFALOAD()
        case _ => throw IllegalOperandException(in.toString);
      }
		
	}
    
        
    def emitASTORE(in:Type,frame:Frame) = 
	{
    	//..., arrayref, index, value -> ...
    	frame.pop();
    	frame.pop();
    	frame.pop();
      in match {
        case IntType => jvm.emitIASTORE()
        case (ArrayPointerType(_)|ClassType(_)|StringType) => jvm.emitAASTORE()
       	case BoolType => jvm.emitBASTORE()
       	case FloatType => jvm.emitFASTORE()
        case _ => throw  IllegalOperandException(in.toString)
      }
		
	}
        
	/** 	generate the var directive for a local variable.
	*	@param in the index of the local variable.
	*	@param varName the name of the local variable.
	*	@param inType the type of the local variable.
	*	@param fromLabel the starting label of the scope where the variable is active.
	*	@param toLabel the ending label  of the scope where the variable is active.
	*/
	def emitVAR(in:Int,varName:String, inType:Type, fromLabel: Int, toLabel: Int,frame:Frame) = jvm.emitVAR(in, varName, getJVMType(inType), fromLabel, toLabel);
	

	/**
	*	generate code to put the value of a variable onto the operand stack.
	*	@param name the name entry of the variable.

	*/
	def emitREADVAR(name:String,inType:Type,index:Int,frame:Frame) = 
	{
		//... -> ..., value
		
			frame.push();
      inType match {
        case (IntType|BoolType) => jvm.emitILOAD(index)
        case (ArrayPointerType(_)|ArrayType(_,_)|ClassType(_)|StringType) => jvm.emitALOAD(index)
        case FloatType => jvm.emitFLOAD(index)
        case _ => throw IllegalOperandException(name)
      }
			

	}
  /* generate the second instruction for array cell access
   * 
   */
	def emitREADVAR2(name:String,typ:Type,frame:Frame) = 
  {
    //... -> ..., value
    
      //frame.push();
      typ match {
        case _ => throw IllegalOperandException(name)
      }
      

  }	

	/**
	*	generate code to pop a value on top of the operand stack and store it to a block-scoped variable.
	*	@param name the symbol entry of the variable.
	*/
	def emitWRITEVAR(name:String,inType:Type,index:Int,frame:Frame) = 
	{
		//..., value -> ...
		frame.pop();    
    
    inType match {
      case (IntType | BoolType) => jvm.emitISTORE(index)
      case FloatType => jvm.emitFSTORE(index)

      case (ArrayPointerType(_)|ClassType(_)|StringType) => jvm.emitASTORE(index)
      
      case _ => throw IllegalOperandException(name)
    }
         
	}	
    /* generate the second instruction for array cell access
   * 
   */
  def emitWRITEVAR2(name:String,typ:Type,frame:Frame) = 
  {
    //... -> ..., value
    
      //frame.push();
      typ match {          

        case _ => throw IllegalOperandException(name)
      }
      

  } 
	/** 	generate the field (static) directive for a class mutable or immutable attribute.
	*	@param lexeme the name of the attribute.
	*	@param in the type of the attribute.
	*	@param isFinal true in case of constant; false otherwise
	*/
	def emitATTRIBUTE(lexeme:String,  in:Type, isFinal:Boolean, value:String) = 
      jvm.emitSTATICFIELD(lexeme,getJVMType(in),false)
		
    /*
    (kind,isFinal) match {
    case (Static,true) => ".field static final " + lexeme + " " + getJVMType(in) + (if (value == "null") "" else  " = " + value + "\n")
    case (Static,false) => ".field static " + lexeme + " " + getJVMType(in) + "\n";
    case (Instance,true) =>".field final " + lexeme + " " + getJVMType(in) + " = " + value + "\n";
    case _ => ".field " + lexeme + " " + getJVMType(in) + "\n";
  }*/
    def emitGETSTATIC( lexeme:String,  in:Type,frame:Frame) = {
      frame.push()
      jvm.emitGETSTATIC(lexeme, getJVMType(in))
    }
        
    def emitPUTSTATIC( lexeme:String, in: Type,frame:Frame) = {
      frame.pop()
      jvm.emitPUTSTATIC(lexeme, getJVMType(in))
    }

    def emitGETFIELD( lexeme:String,  in:Type,frame:Frame) =   jvm.emitGETFIELD(lexeme, getJVMType(in))
    
        
    def emitPUTFIELD( lexeme:String, in: Type,frame:Frame) = {
      frame.pop()
      frame.pop()
      jvm.emitPUTFIELD(lexeme, getJVMType(in))
    }
	/**	generate code to invoke a static method
	*	@param lexeme the qualified name of the method(i.e., class-name/method-name)
	*	@param in the type descriptor of the method.
	*/
	def emitINVOKESTATIC(lexeme:String,in:Type ,frame:Frame) =
	{	
    val typ = in.asInstanceOf[FunctionType]
    typ.input.map(x=>frame.pop)
		if (typ.output != VoidType)
			frame.push();		
		jvm.emitINVOKESTATIC(lexeme,getJVMType(in));
	}
	/**  generate code to invoke a special method
  * @param lexeme the qualified name of the method(i.e., class-name/method-name)
  * @param in the type descriptor of the method.
  */
  def emitINVOKESPECIAL(lexeme:String,in:Type ,frame:Frame) =
  { 
    val typ = in.asInstanceOf[FunctionType]
    typ.input.map(x=>frame.pop)
    frame.pop
    if (typ.output != VoidType)
      frame.push();   
    jvm.emitINVOKESPECIAL(lexeme,getJVMType(in));
  } 
  
  /**  generate code to invoke a default special method
  * 
  */
  def emitINVOKESPECIAL(frame:Frame) = {
    frame.pop
    jvm.emitINVOKESPECIAL()
  }
  /**  generate code to invoke a virtual method
  * @param lexeme the qualified name of the method(i.e., class-name/method-name)
  * @param in the type descriptor of the method.
  */
  def emitINVOKEVIRTUAL(lexeme:String,in:Type ,frame:Frame) =
  { 
    val typ = in.asInstanceOf[FunctionType]
    typ.input.map(x=>frame.pop)
    frame.pop
    if (typ.output != VoidType)
      frame.push();   
    jvm.emitINVOKEVIRTUAL(lexeme,getJVMType(in));
  } 
        /**
	*	generate ineg, fneg.
	*	@param in the type of the operands.
	*/
  	def emitNEGOP( in:Type,frame:Frame) = 
	{
  		//..., value -> ..., result
       	if (in == IntType)
          	 jvm.emitINEG()
      	else
           	 jvm.emitFNEG()
  	}
        
  	def emitNOT(in:Type,frame:Frame ) =
	{
  		val label1 = frame.getNewLabel();
    	val label2 = frame.getNewLabel();
      val result = new StringBuffer();
      result.append(emitIFTRUE(label1,frame));
      result.append(emitPUSHCONST("true", in,frame));
      result.append(emitGOTO(label2,frame));
      result.append(emitLABEL(label1,frame));
      result.append(emitPUSHCONST("false", in,frame));
      result.append(emitLABEL(label2,frame));
      result.toString();
  	}
        
	/**
	*	generate iadd, isub, fadd or fsub.
	*	@param lexeme the lexeme of the operator.
	*	@param in the type of the operands.
	*/	
	  def emitADDOP(lexeme:String, in:Type,frame:Frame) = 
	{
		//..., value1, value2 -> ..., result
		frame.pop();
		if (lexeme.equals("+")) {
			if (in == IntType)
				 jvm.emitIADD();
			else 
				 jvm.emitFADD()
		} else 
			if (in == IntType)
				 jvm.emitISUB();
			else 
				 jvm.emitFSUB();
	}
	/**
	*	generate imul, idiv, fmul or fdiv.
	*	@param lexeme the lexeme of the operator.
	*	@param in the type of the operands.
	*/	
	
	def emitMULOP(lexeme:String, in:Type,frame:Frame) =
	{
		//TODO \:integer division; %:integer remainder
		//..., value1, value2 -> ..., result
		frame.pop();
		if (lexeme.equals("*")) {
			if (in == IntType)
				 jvm.emitIMUL();
			else 
				 jvm.emitFMUL();
		}
		else if (in == IntType)
          jvm.emitIDIV();
    else
			 jvm.emitFDIV();
	}

	
	def emitDIV(frame:Frame) = 
	{
		frame.pop();
		jvm.emitIDIV();
	}

	
	def emitMOD(frame:Frame) =
	{
		frame.pop();
		jvm.emitIREM();
	}
	/**
	*	generate iand.
	*/	

	def emitANDOP(frame:Frame) =
	{
		frame.pop();
		jvm.emitIAND();
	}	
	/**
	*	generate ior.
	*/	
	def emitOROP(frame:Frame) = 
	{
		frame.pop();
		jvm.emitIOR();
	}
        
	def emitREOP(op:String,	in:Type,frame:Frame) = {
		//..., value1, value2 -> ..., result
		val result = new StringBuffer()
		val labelF = frame.getNewLabel()
		val labelO = frame.getNewLabel()
		frame.pop()
		frame.pop()
		val emitif = op match {
			case ">"  => if(in == IntType) jvm.emitIFICMPLE(labelF) else jvm.emitIFLE(labelF)
			case ">=" => if(in == IntType) jvm.emitIFICMPLT(labelF) else jvm.emitIFLT(labelF)
			case "<"  => if(in == IntType) jvm.emitIFICMPGE(labelF) else jvm.emitIFGE(labelF)
			case "<=" => if(in == IntType) jvm.emitIFICMPGT(labelF) else jvm.emitIFGT(labelF)
			case "!=" => jvm.emitIFICMPEQ(labelF)
			case "==" => jvm.emitIFICMPNE(labelF)
		}
		if(in == FloatType) result.append(jvm.emitFCMPL())
		result.append(emitif)
		result.append(emitPUSHCONST("1", IntType,frame))
		frame.pop()
		result.append(emitGOTO(labelO,frame))
		result.append(emitLABEL(labelF,frame))
		result.append(emitPUSHCONST("0", IntType,frame))
		result.append(emitLABEL(labelO,frame))
		result.toString()
	}
def emitRELOP( op:String,  in:Type,trueLabel:Int,falseLabel:Int,frame:Frame) =
    {
      //..., value1, value2 -> ..., result
        val result = new StringBuffer();
        //val (isFalse,label) = if (trueLabel == CodeGenVisitor.FallThrough) (true,falseLabel) else (false,trueLabel)
        frame.pop();
        frame.pop();
        op match {
          case ">" => {
                        result.append(jvm.emitIFICMPLE(falseLabel))
                        result.append(jvm.emitGOTO(trueLabel))
                      }

          case ">=" => result.append(jvm.emitIFICMPLT(falseLabel))

          case "<" =>  result.append(jvm.emitIFICMPGE(falseLabel))

          case "<=" => result.append(jvm.emitIFICMPGT(falseLabel))

          case "!=" =>  result.append(jvm.emitIFICMPEQ(falseLabel))
          case "==" =>  result.append(jvm.emitIFICMPNE(falseLabel))
                          
                       
        }
        result.append(jvm.emitGOTO(trueLabel))
        result.toString();
  }
	/** 	generate the method directive for a function.
	*	@param lexeme the qualified name of the method(i.e., class-name/method-name).
	*	@param in the type descriptor of the method.
	*	@param isStatic <code>true</code> if the method is static; <code>false</code> otherwise.
	*/
	def emitMETHOD( lexeme:String, in: Type, isStatic:Boolean,frame:Frame) =  jvm.emitMETHOD(lexeme,getJVMType(in),isStatic)
	/** 	generate the end directive for a function.
	*/
	def emitENDMETHOD(frame:Frame)  = {
		var buffer = new StringBuffer();
		buffer.append(jvm.emitLIMITSTACK(frame.getMaxOpStackSize()));
		buffer.append(jvm.emitLIMITLOCAL(frame.getMaxIndex()));
		buffer.append(jvm.emitENDMETHOD());
		buffer.toString();
	}


  def getConst(ast:Literal)= ast match {
    case IntLiteral(i) => (i.toString,IntType)

  
  }




	/** 	generate code to initialize a local array variable.<p>
	*	@param index the index of the local variable.
	*	@param in the type of the local array variable.
	*/	
  
	def emitINITARRAY(idx: Int,arr: Type, frame: Frame) = {
		val dimen = arr.asInstanceOf[ArrayType].dimen
		val eleType = arr.asInstanceOf[ArrayType].eleType

		val buffer = new StringBuffer()
		buffer.append(emitPUSHICONST(dimen.value, frame))
		buffer.append(jvm.emitNEWARRAY(getFullType(eleType)))
		buffer.append(jvm.emitASTORE(idx))

		frame.pop()

		buffer.toString
	}



  /*public String emitINITARRAY(int index,Type in) throws CompilationException  {
		StringBuffer buffer = new StringBuffer();
		ArrayType at = (ArrayType) in;
		ProductType rt = (ProductType) at.getIType();
		int element = 0;
		int dimension = 0;
		while (!(rt.getE1Type() == null && rt.getE2Type() == null)) {
			element = ((RangeType)rt.getE1Type()).getUpper();
			dimension++;
			buffer.append(emitPUSHICONST(element));
			rt = (ProductType) rt.getE2Type();
		}
		if (dimension == 1) {
			buffer.append(emitNEWARRAY(at.getEType()));
			frame.pop();
			buffer.append(jvm.emitASTORE(index));
		}
		else {
			for (int i = 0; i < dimension; i++)
				frame.pop();
			buffer.append(jvm.emitMULTIANEWARRAY(at.getJVMType(), dimension));
			buffer.append(jvm.emitASTORE(index));
		}
		return buffer.toString();
	}*/
	/** 	generate code to initialize local array variables.
	*	@param in the list of symbol entries corresponding to local array variable.
	*/
	
	


/*	public String emitLISTARRAY(List<SymEntry> in) throws CompilationException {
		StringBuffer result = new StringBuffer();
		for (Iterator<SymEntry> it = in.iterator();it.hasNext();) {
			SymEntry sym = it.next();
			ArrayType at =(ArrayType)sym.getType();
			result.append(emitINITARRAY((Integer)sym.getObject(),at));
		}
		in.clear();
		return result.toString();
	}*/
	/**
	*	generate code to jump to label if the value on top of operand stack is true.<p>
	*	ifgt label
	*	@param label the label where the execution continues if the value on top of stack is true.
	*/
	def emitIFTRUE(label:Int,frame:Frame)  = 
	{
		frame.pop();
		jvm.emitIFGT(label);
	}
	/**
	*	generate code to jump to label if the value on top of operand stack is false.<p>
	*	ifle label
	*	@param label the label where the execution continues if the value on top of stack is false.
	*/
	def emitIFFALSE(label:Int,frame:Frame) = 
	{
		frame.pop();
		jvm.emitIFLE(label);
	}
        
  	def emitIFICMPGT(label:Int,frame:Frame) = 
	{
		frame.pop();
		jvm.emitIFICMPGT(label);
	}
        
  	def emitIFICMPLT(label:Int,frame:Frame) = 
	{
		frame.pop();
		jvm.emitIFICMPLT(label);
	}
        
	/** 	generate code to duplicate the value on the top of the operand stack.<p>
	*	Stack:<p>
	*	Before: ...,value1<p>
	*	After:  ...,value1,value1<p>
	*/
	def emitDUP(frame:Frame) =
	{
		frame.push();
		jvm.emitDUP();
	}

	def emitDUPX2(frame:Frame) =
	{
		frame.push();
		jvm.emitDUPX2();
	}


	/**	generate code to pop the value on the top of the operand stack.
	*/
	def emitPOP(frame:Frame) = 
	{
		frame.pop();
		jvm.emitPOP();
	}
	/** 	generate code to exchange an integer on top of stack to a floating-point number.
	*/
	def emitI2F(frame:Frame) = jvm.emitI2F()
  
	/**	generate code to return.
	*	<ul>
	*	<li>ireturn if the type is IntegerType or BooleanType
	*	<li>freturn if the type is RealType
	*	<li>return if the type is null
	*	</ul>
	*	@param in the type of the returned expression.
	*/

	def emitRETURN(in:Type,frame:Frame) = 
	{
		in match {
      case IntType | BoolType => frame.pop();jvm.emitIRETURN()
      case FloatType => frame.pop();jvm.emitFRETURN()

      case VoidType => jvm.emitRETURN()
      case ArrayPointerType(_)|ArrayType(_,_)|StringType => frame.pop();jvm.emitARETURN()
    }
	}
	/** generate code that represents a label	
	 *	@param label the label
	 *	@return code Label<label>:
	 */
	def emitLABEL(label:Int,frame:Frame) = jvm.emitLABEL(label)
  
	/** generate code to jump to a label	
	 *	@param label the label
	 *	@return code goto Label<label>
	 */
	def emitGOTO(label:Int,frame:Frame) =  jvm.emitGOTO(label)

	/**	generate some starting directives for a class.<p>
	*	.source MPC.CLASSNAME.java<p>
	*	.class public MPC.CLASSNAME<p>
	*	.super java/lang/Object<p>
	*/	
	def emitPROLOG(name:String,parent:String) = {
		val result = new StringBuffer();
		result.append(jvm.emitSOURCE(name + ".java"));
		result.append(jvm.emitCLASS("public " + name));
		result.append(jvm.emitSUPER(if (parent == "") "java/lang/Object" else parent));
		result.toString();
	}
  
  def emitLIMITSTACK(num:Int) = jvm.emitLIMITSTACK(num)
  
  def emitLIMITLOCAL(num:Int) = jvm.emitLIMITLOCAL(num)
  
  def emitEPILOG() = {
    val file = new FileWriter(filename)
    file.write(buff.toString())
    file.close()
  }
	/** print out the code to screen
	*	@param in the code to be printed out
	*/
	def printout(in:String) = buff.append(in);
	
        
 	def clearBuff() = buff.setLength(0);
   	
}
		
