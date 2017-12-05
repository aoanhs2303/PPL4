package q1.astgen
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.ParserRuleContext
import java.io.{PrintWriter,File}
import org.antlr.v4.runtime.ANTLRFileStream
import scala.collection.JavaConverters._
import org.antlr.v4.runtime.tree._

import q1.utils._
import q1.parser._
import q1.parser.Q1Parser._

class ASTGeneration extends Q1BaseVisitor[Any] {
	override def visitProg(ctx:ProgContext) = 
		Program(ctx.stmt.asScala.toList.map(visit(_)).asInstanceOf[List[Stmt]])		
	override def visitStmt(ctx:StmtContext) = 
		Stmt(Id(ctx.ID.getText),visit(ctx.exp).asInstanceOf[Exp])
	override def visitExp(ctx:ExpContext) =
		ctx.term.asScala.tail.zip(ctx.ADDOP.asScala.toList).foldLeft(visit(ctx.term(0)).asInstanceOf[Exp])((lhs,rhs) => rhs._2.getText match {
			case "+" => Add(lhs,visit(rhs._1).asInstanceOf[Exp])
			case "-" => Minus(lhs,visit(rhs._1).asInstanceOf[Exp])
		})
	override def visitTerm(ctx:TermContext) =
		if(ctx.getChildCount == 1)
			visit(ctx.fact)
		else
			ctx.MULOP.getText match {
				case "*" => Mul(visit(ctx.term).asInstanceOf[Exp],visit(ctx.fact).asInstanceOf[Exp])
				case "/" => Div(visit(ctx.term).asInstanceOf[Exp],visit(ctx.fact).asInstanceOf[Exp])
			}
	override def visitFact(ctx:FactContext) = 
		if(ctx.ID != null)
			Id(ctx.ID.getText)
		else if(ctx.INTLIT != null)
			Intlit(ctx.INTLIT.getText)
		else if(ctx.FLOATLIT != null)
			Floatlit(ctx.FLOATLIT.getText)
		else visit(ctx.exp)


}
