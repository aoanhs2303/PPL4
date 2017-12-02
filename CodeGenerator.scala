/**
 *	@author Nguyen Hua Phung
 *	@version 1.0
 *	23/10/2015
 * 	This file provides a simple version of code generator
 *

 */

package mc.codegen





import mc.checker._
import mc.utils._
import java.io.{PrintWriter, File}


object CodeGenerator extends Utils {
  val libName = "io"
  def init() = List( Symbol("getInt",FunctionType(List(),IntType),CName(libName)),
                     Symbol("putInt",FunctionType(List(IntType),VoidType),CName(libName)),
                     Symbol("putIntLn",FunctionType(List(IntType),VoidType),CName(libName)),
                     Symbol("getFloat",FunctionType(List(),FloatType),CName(libName)),
                     Symbol("putFloat",FunctionType(List(FloatType),VoidType),CName(libName)),
                     Symbol("putFloatLn",FunctionType(List(FloatType),VoidType),CName(libName)),
                     Symbol("putBool",FunctionType(List(BoolType),VoidType),CName(libName)),
                     Symbol("putBoolLn",FunctionType(List(BoolType),VoidType),CName(libName)),
                     Symbol("putString",FunctionType(List(StringType),VoidType),CName(libName)),
                     Symbol("putStringLn",FunctionType(List(StringType),VoidType),CName(libName)),
                     Symbol("putLn",FunctionType(List(),VoidType),CName(libName))
                    )
    
  
	def gen(ast:AST,dir:File) = {
    val gl = init()     
		val gc = new CodeGenVisitor(ast,gl,dir)    
		gc.visit(ast, null);   
	}

}




case class ClassType(cname:String) extends Type

//case class SubContext(emit:Emitter,decl:List[Decl]) 

case class Flag(frame:Frame,val sym:List[Symbol],flag:Int)

case class SubBody(frame:Frame,sym:List[Symbol])  // sinh ma cho cac phat bien

class Access(val frame:Frame,val sym:List[Symbol],val isLeft:Boolean,val isFirst:Boolean) // sinh ma cho cac bieu thuc

trait Val
  case class Index(value:Int) extends Val
  case class CName(value:String) extends Val



class CodeGenVisitor(astTree:AST,env:List[Symbol],dir:File) extends BaseVisitor with Utils {
	
  val className = "MCClass"
  val path = dir.getPath()
  val emit = new Emitter(path+"/"+className+".j")

  override def visitProgram(ast:Program,c:Any) = {

      emit.printout(emit.emitPROLOG(className, "java.lang.Object"))     // PROLOG ghi ra ten class
      ast.decl.filter(_.isInstanceOf[VarDecl]).map(visit(_,null)) 
      
      val symlst = ast.decl.foldLeft(env)((lst, x) => {
        val s = x match {
          case VarDecl(n,vt) => Symbol(n.name,vt, CName(className))
          case FuncDecl(n,p,rt,_) => Symbol(n.name, FunctionType(p.map(_.varType), rt), CName(className))
        }
        s::lst
      })  

      ast.decl.filter(_.isInstanceOf[FuncDecl]).map(visit(_,SubBody(null,symlst)))
      // generate default constructor 
      genMETHOD(
            FuncDecl(Id("<init>"),List(),null,Block(List(),List())),c,new Frame("<init>",VoidType))
      emit.emitEPILOG()  
  }
  
    /** generate code for default constructor 

   *  @param ast the function declaration whose code will be generated by this method
   *  @param frame the frame where the initialization happen 
   *  @param o the referencing environment
   */
   
  def genMETHOD(ast:FuncDecl,o:Any,frame:Frame) = {
    
    val isInit = ast.returnType == null // neu la init function thi cho ra null
    val isMain = ast.name.name == "main" && ast.param.length == 0 && ast.returnType == VoidType // neu la ham main thi param deo co va return type la void
    val returnType = if (isInit) VoidType else ast.returnType // neu la ham khoi tao thi VoidType con deo thi tra ve kieu cua ham do
    val methodName = if (isInit) "<init>" else ast.name.name // name cua init la <init> con deo thi ten cua no
    val intype = if (isMain) List(ArrayPointerType(StringType)) else List() // neu ham main thi param la args string[] && con lai thi la List() rong
    val mtype =  FunctionType(intype,returnType) // funtioctype co tham so dau vao va kieu tra ve
    
    emit.printout(emit.emitMETHOD(methodName, mtype, !isInit, frame)) // emit ra file j nhung cai tren

    frame.enterScope(true); // true neu la Label dau tien ; frame la noi luu tru dai khai vay
    
    val glenv = o.asInstanceOf[List[Symbol]] // ep kieu duoc nhan ve List[Symbol]

    // Generate code for parameter declarations
    val nenv = if (isInit) {
      emit.printout(emit.emitVAR(frame.getNewIndex,"this",ClassType(className),frame.getStartLabel,frame.getEndLabel,frame))
      glenv
    }
    else if (isMain) {
      emit.printout(emit.emitVAR(frame.getNewIndex,"args",ArrayPointerType(StringType),frame.getStartLabel,frame.getEndLabel,frame))
      glenv
    }
    else {
      ast.param.foldLeft(glenv)((lst,x) => visit(x,frame).asInstanceOf[Symbol]::lst)
    }

    val body = ast.body.asInstanceOf[Block]
    
    val nnenv = body.decl.foldLeft(nenv)((lst,x) => visit(x,frame).asInstanceOf[Symbol]::lst)
   

    emit.printout(emit.emitLABEL(frame.getStartLabel(),frame))
    
    //Generate code for statements
    if (isInit) {
      emit.printout(emit.emitREADVAR("this",ClassType(className),0,frame))
      emit.printout(emit.emitINVOKESPECIAL(frame))
    }
    
    body.stmt.map(visit(_,SubBody(frame,nnenv)))
    
    emit.printout(emit.emitLABEL(frame.getEndLabel(),frame))
    if (returnType == VoidType) emit.printout(emit.emitRETURN(VoidType,frame));
    emit.printout(emit.emitENDMETHOD(frame));
    frame.exitScope();
     
  }
  
  override def visitFuncDecl(ast:FuncDecl,o:Any) = {
    val subctxt = o.asInstanceOf[SubBody]
    val frame = new Frame(ast.name.name,ast.returnType)
    genMETHOD(ast,subctxt.sym,frame)
    //SubBody(null,Symbol(ast.name.name,FunctionType(List(),ast.returnType),CName(className))::subctxt.sym)
  }
 
  override def visitVarDecl(ast:VarDecl,o:Any) = {
    
    val frame = o.asInstanceOf[Frame]
    val name = ast.variable.name
    val mtype = ast.varType
    
    if(frame == null) {
      emit.printout(emit.emitATTRIBUTE(name,mtype,false,null))
    }
    else {
      val index = frame.getNewIndex
      emit.printout(emit.emitVAR(index,name,mtype,frame.getStartLabel,frame.getEndLabel,frame))
      Symbol(name,mtype,Index(index))
    }

  }


  override def visitBinaryOp(ast:BinaryOp, o: Any) = {
    val sub = o.asInstanceOf[Access]
    val frame = sub.frame
    val env = sub.sym
    val buffer = new StringBuffer()

    val isFirst = sub.isFirst
    val isLeft  = sub.isLeft

    ast.op match {
      case "=" => {

        val r = visit(ast.right, new Access(frame, env, false, false)).asInstanceOf[(String, Type)]
        buffer.append(r._1)
        if (!isFirst) 
          buffer.append(emit.emitDUP(frame))

        val l = visit(ast.left, new Access(frame, env, true, false)).asInstanceOf[(String, Type)]

        buffer.append(l._1)
        if(isFirst) emit.printout(buffer.toString) else (buffer.toString,l._2)

      }

      case "%" => {
        val l = visit(ast.left, new Access(frame, env, false, false)).asInstanceOf[(String, Type)]
        val r = visit(ast.right, new Access(frame, env, false, false)).asInstanceOf[(String,Type)]
        buffer.append(l._1)
        buffer.append(r._1)
        buffer.append(emit.emitMOD(frame))

        if(isFirst) emit.printout(buffer.toString) else (buffer.toString, l._2)

      }

      case "&&" => {
        val l = visit(ast.left, new Access(frame, env, false, false)).asInstanceOf[(String, Type)]
        val r = visit(ast.right, new Access(frame,env, false, false)).asInstanceOf[(String, Type)]
        buffer.append(l._1)
        buffer.append(r._1)
        buffer.append(emit.emitANDOP(frame))

        if(isFirst) emit.printout(buffer.toString) else (buffer.toString, l._2)
      }

      case "||" => {
        val l = visit(ast.left, new Access(frame, env, false, false)).asInstanceOf[(String, Type)]
        val r = visit(ast.right, new Access(frame, env, false, false)).asInstanceOf[(String, Type)]
         buffer.append(l._1)
         buffer.append(r._1)
         buffer.append(emit.emitOROP(frame))

         if(isFirst) emit.printout(buffer.toString) else (buffer.toString, l._2)
      }

      case _ => {
        val l = visit(ast.left, new Access(frame, env, false, false)).asInstanceOf[(String, Type)]
        val r = visit(ast.right, new Access(frame, env, false, false)).asInstanceOf[(String, Type)]
        val optype = if(l._2 == FloatType || r._2 == FloatType) FloatType else IntType
        buffer.append(l._1)
        if(l._2 != optype) buffer.append(emit.emitI2F(frame))
        buffer.append(r._1)
        if(r._2 != optype) buffer.append(emit.emitI2F(frame))

        val rtype = ast.op match {
          case ("+" | "-") => buffer.append(emit.emitADDOP(ast.op, optype, frame)); optype
          case ("*" | "/") => buffer.append(emit.emitMULOP(ast.op, optype, frame)); optype
          case _ => buffer.append(emit.emitREOP(ast.op, optype, frame)); BoolType  
        }

        if(isFirst) emit.printout(buffer.toString) else (buffer.toString, rtype)
      }

    }

    // val l = visit(ast.left, new Access(frame, null, false, false)).asInstanceOf[(String, Type)]
    // val r = visit(ast.right, new Access(frame, null, false, false)).asInstanceOf[(String, Type)]
    // val optype = if(l._2 == FloatType || r._2 == FloatType) FloatType else IntType
    // buffer.append(l._1)
    // if(l._2 != optype) buffer.append(emit.emitI2F(frame))
    //   buffer.append(l._1)
    // if(r._2 != optype) buffer.append(emit.emitI2F(frame))
    //   buffer.append(emit.emitADDOP(ast.op, optype, frame))

    // (buffer.toString, optype)  
  }

  override def visitUnaryOp(ast:UnaryOp, o: Any) = {
    val sub = o.asInstanceOf[Access]
    val frame = sub.frame
    val env = sub.sym

    val isFirst = sub.isFirst

    val buffer = new StringBuffer()

    val b = visit(ast.body, new Access(frame, env, false, false)).asInstanceOf[(String, Type)]

    buffer.append(b._1)
    ast.op match {
      case "-" => buffer.append(emit.emitNEGOP(b._2, frame))
      case "!" => buffer.append(emit.emitNOT(b._2, frame))
    }

    if(isFirst) emit.printout(buffer.toString) else (buffer.toString, b._2)
  }

  // override def visitBlock(ast:Block,o:Any) = {
  //   val sub = o.asInstanceOf[Access]
  //   val frame = sub.frame
  //   val sym = sub.sym
  //   val isFirst = sub.isFirst

  //   if(!isFirst) frame.enterScope(false)
  //   emit.printout(emit.emitLABEL(frame.getStartLabel(),frame))

  //   ast.decl.map(visit(_,null))

  //   ast.stmt.map(visit(_,null))
  //   emit.printout(emit.emitLABEL(frame.getEndLabel(),frame))
  //   if(!isFirst) frame.exitScope()
  // }

  
  override def visitCallExpr(ast:CallExpr,o:Any) = {
    val sub = o.asInstanceOf[SubBody]
    val frame = sub.frame
    val env = sub.sym
    val sym = lookup(ast.method.name,env,(x:Symbol)=>x.name).get
    val cname = sym.value.asInstanceOf[CName].value
    val ctype = sym.typ

    val in = ast.params.foldLeft(("",List[Type]()))((y,x) => {
      val (str1,typ1) = visit(x,new Access(frame,env,false,false)).asInstanceOf[(String,Type)]
      (y._1 + str1,y._2 :+ typ1)
    })
    emit.printout(in._1)  
    emit.printout(emit.emitINVOKESTATIC(cname+"/"+ast.method.name,ctype,frame))   
  }

  override def visitIntLiteral(ast:IntLiteral,o:Any) = {
    val ctxt = o.asInstanceOf[Access]
    val frame = ctxt.frame
    (emit.emitPUSHCONST(ast.value.toString,IntType,frame),IntType)
  }

  override def visitFloatLiteral(ast:FloatLiteral, o:Any) = {
    val ctxt = o.asInstanceOf[Access]
    val frame = ctxt.frame
    (emit.emitPUSHCONST(ast.value.toString,FloatType,frame),FloatType)
  }

  override def visitBooleanLiteral(ast:BooleanLiteral,o:Any) = {
    val sub = o.asInstanceOf[Access]          
    val frame = sub.frame  
    (emit.emitPUSHCONST(ast.value.toString, BoolType,frame),BoolType)
  }       

  
}