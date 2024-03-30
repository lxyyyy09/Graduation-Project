// no-trailing-spaces: off
package com.rustsmith.ast

import com.rustsmith.CustomRandom
import com.rustsmith.recondition.ReconditionedArrayAccess
import com.rustsmith.recondition.ReconditionedDivision
import com.rustsmith.recondition.ReconditionedMod
import java.math.BigInteger
import kotlin.math.pow
import kotlin.random.Random
import kotlin.reflect.KClass

annotation class ExpressionGenNode(val compatibleType: KClass<out Type>)

annotation class SwarmNode

sealed interface Expression : ASTNode {
    val symbolTable: SymbolTable
}

sealed interface LiteralExpression : Expression

sealed interface LHSAssignmentNode : Expression {
    fun rootNode(): Variable?
}

@ExpressionGenNode(VoidType::class)
data class VoidLiteral(override val symbolTable: SymbolTable) : Expression {
    override fun toRust(): String {
        return "()"
    }
}

@ExpressionGenNode(LiteralType::class)
data class CLIArgumentAccessExpression(val index: Int, val type: Type, override val symbolTable: SymbolTable) :
    LiteralExpression {
    override fun toRust(): String {
//        return "cli_args[reconditioned_cliargs_index_access!(cli_args,$index)].clone().parse::<${type.toRust()}>().unwrap()"
        val base = 2.0
        val charPool: List<Char> = ('a'..'z') + ('A'..'Z') + ('0'..'9')
        val randomString = (1..CustomRandom.nextInt(100))
            .map { charPool[CustomRandom.nextInt(0, charPool.size)] }
            .joinToString("")
        val tmp = when (type.toRust()) {
            "i8" -> (-base.pow(7).toInt()..(base.pow(7) - 1).toInt()).random().toInt()
            "i16" -> (-base.pow(15).toInt()..(base.pow(15) - 1).toInt()).random()
            "i32" -> (-base.pow(31).toInt()..(base.pow(31) - 1).toInt()).random()
            "i64" -> (-base.pow(63).toInt()..(base.pow(63) - 1).toInt()).random()
            "i128" -> (-base.pow(127).toInt()..(base.pow(127) - 1).toInt()).random()
            "u8" -> (0..(base.pow(8) - 1).toInt()).random()
            "u16" -> (0..(base.pow(16) - 1).toInt()).random()
            "u32" -> (0..(base.pow(32) - 1).toInt()).random()
            "u64" -> (0..(base.pow(64) - 1).toInt()).random()
            "u128" -> (0..(base.pow(128) - 1).toInt()).random()
            "usize" -> (0..(base.pow(32) - 1).toInt()).random()
            "f32" -> Random.nextFloat() + (-base.pow(10).toInt()..(base.pow(10)).toInt()).random() * 1.0
            "f64" -> Random.nextDouble() + (-base.pow(10).toInt()..(base.pow(10)).toInt()).random() * 1.0
            "bool" -> Random.nextBoolean()
            "String" -> "\"$randomString\".to_string()"
            else -> "\"$randomString\".to_string()"
        }
        return "cli_args[reconditioned_cliargs_index_access!(cli_args,$index)].clone().parse::<${type.toRust()}>().unwrap_or($tmp)"
    }
}

@ExpressionGenNode(I8Type::class)
data class Int8Literal(val value: Int, override val symbolTable: SymbolTable) : LiteralExpression {
    override fun toRust(): String {
        return "${value}i8"
    }
}

@ExpressionGenNode(I16Type::class)
data class Int16Literal(val value: Int, override val symbolTable: SymbolTable) : LiteralExpression {
    override fun toRust(): String {
        return "${value}i16"
    }
}

@ExpressionGenNode(I32Type::class)
data class Int32Literal(val value: Int, override val symbolTable: SymbolTable) :
    LiteralExpression {
    override fun toRust(): String {
        return "${value}i32"
    }
}

@ExpressionGenNode(I64Type::class)
data class Int64Literal(val value: Long, override val symbolTable: SymbolTable) :
    LiteralExpression {
    override fun toRust(): String {
        return "${value}i64"
    }
}

@ExpressionGenNode(I128Type::class)
data class Int128Literal(val value: BigInteger, override val symbolTable: SymbolTable) :
    LiteralExpression {
    override fun toRust(): String {
        return "${value}i128"
    }
}

@ExpressionGenNode(U8Type::class)
data class UInt8Literal(val value: UInt, override val symbolTable: SymbolTable) : LiteralExpression {
    override fun toRust(): String {
        return "${value}u8"
    }
}

@ExpressionGenNode(U16Type::class)
data class UInt16Literal(val value: UInt, override val symbolTable: SymbolTable) : LiteralExpression {
    override fun toRust(): String {
        return "${value}u16"
    }
}

@ExpressionGenNode(U32Type::class)
data class UInt32Literal(val value: UInt, override val symbolTable: SymbolTable) :
    LiteralExpression {
    override fun toRust(): String {
        return "${value}u32"
    }
}

@ExpressionGenNode(U64Type::class)
data class UInt64Literal(val value: ULong, override val symbolTable: SymbolTable) :
    LiteralExpression {
    override fun toRust(): String {
        return "${value}u64"
    }
}

@ExpressionGenNode(U128Type::class)
data class UInt128Literal(val value: BigInteger, override val symbolTable: SymbolTable) :
    LiteralExpression {
    override fun toRust(): String {
        return "${value}u128"
    }
}

@ExpressionGenNode(USizeType::class)
data class USizeLiteral(val value: ULong, override val symbolTable: SymbolTable) :
    LiteralExpression {
    override fun toRust(): String {
        return "${value}usize"
    }
}

@ExpressionGenNode(F32Type::class)
data class Float32Literal(val value: Float, override val symbolTable: SymbolTable) : LiteralExpression {
    override fun toRust(): String {
        return "${value}f32"
    }
}

@ExpressionGenNode(F64Type::class)
data class Float64Literal(val value: Double, override val symbolTable: SymbolTable) : LiteralExpression {
    override fun toRust(): String {
        return "${value}f64"
    }
}

@ExpressionGenNode(StringType::class)
data class StringLiteral(val value: String, override val symbolTable: SymbolTable) : LiteralExpression {
    override fun toRust(): String {
        return "String::from(\"$value\")"
    }
    fun onlyText(): String {
        return "$value"
    }
}

@ExpressionGenNode(USizeType::class)
data class StringLengthExpression(
    val stringExpression: Expression,
    override val symbolTable: SymbolTable
) : NonMovingExpressions {
    override fun toRust(): String {
        return "${stringExpression.toRust()}.len()"
    }
}

@ExpressionGenNode(VoidType::class)
data class StringPushStrExpression(
    val stringExpression: Expression,
    val pushStrExpression: Expression,
    override val symbolTable: SymbolTable
) : NonMovingExpressions {
    override fun toRust(): String {
        val text = when (pushStrExpression) {
            is StringLiteral -> pushStrExpression.onlyText()
            else -> "hello world"
        }
        return "${stringExpression.toRust()}.push_str(\"${text}\")"
    }
}

@ExpressionGenNode(BoolType::class)
data class BooleanLiteral(val value: Boolean, override val symbolTable: SymbolTable) : LiteralExpression {
    override fun toRust(): String {
        return value.toString()
    }
}

@ExpressionGenNode(TupleType::class)
data class TupleLiteral(val values: List<Expression>, override val symbolTable: SymbolTable) : LiteralExpression {
    override fun toRust(): String {
        return "(${values.joinToString(",") { it.toRust() }})"
    }
}

sealed interface PartialMoveExpression : Expression

sealed interface ElementAccessExpression : Expression

data class TupleElementAccessExpression(
    val expression: Expression,
    val index: Int,
    override val symbolTable: SymbolTable
) : RecursiveExpression, PartialMoveExpression, LHSAssignmentNode, ElementAccessExpression {
    override fun rootNode(): Variable? {
        return if (expression is LHSAssignmentNode) {
            expression.rootNode()
        } else {
            null
        }
    }
    override fun toRust(): String {
        return "${expression.toRust()}.$index"
    }
}

@SwarmNode
data class StructElementAccessExpression(
    val expression: Expression,
    val elementName: String,
    override val symbolTable: SymbolTable
) : RecursiveExpression, PartialMoveExpression, LHSAssignmentNode, ElementAccessExpression {
    override fun rootNode(): Variable? {
        return if (expression is LHSAssignmentNode) {
            expression.rootNode()
        } else {
            null
        }
    }
    override fun toRust(): String {
        return "${expression.toRust()}.$elementName"
    }
}

data class Variable(
    val value: String,
    override val symbolTable: SymbolTable
) : LHSAssignmentNode, ElementAccessExpression {
    override fun rootNode(): Variable {
        return this
    }
    override fun toRust(): String {
        return value
    }
}

@ExpressionGenNode(NonVoidType::class)
data class ElementAccess(val expression: Expression, override val symbolTable: SymbolTable) : Expression {
    override fun toRust(): String {
        return expression.toRust()
    }
}

sealed interface RecursiveExpression : Expression

sealed interface BinOpExpression : RecursiveExpression {
    val expr1: Expression
    val expr2: Expression
}

@SwarmNode
@ExpressionGenNode(NumberType::class)
data class AddExpression(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : BinOpExpression {
    override fun toRust(): String {
        return "(${expr1.toRust()} + ${expr2.toRust()})"
    }
}

@SwarmNode
@ExpressionGenNode(NumberType::class)
data class SubtractExpression(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : BinOpExpression {
    override fun toRust(): String {
        return "(${expr1.toRust()} - ${expr2.toRust()})"
    }
}

@SwarmNode
@ExpressionGenNode(NumberType::class)
data class DivideExpression(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : BinOpExpression {
    override fun toRust(): String {
        return "(${expr1.toRust()} / ${expr2.toRust()})"
    }
}

@SwarmNode
@ExpressionGenNode(NumberType::class)
data class MultiplyExpression(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : BinOpExpression {
    override fun toRust(): String {
        return "(${expr1.toRust()} * ${expr2.toRust()})"
    }
}

@SwarmNode
@ExpressionGenNode(IntType::class)
data class ModExpression(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : BinOpExpression {
    override fun toRust(): String {
        return "(${expr1.toRust()} % ${expr2.toRust()})"
    }
}

@SwarmNode
@ExpressionGenNode(BitWiseCompatibleType::class)
data class BitwiseAndLogicalAnd(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : BinOpExpression {
    override fun toRust(): String {
        return "(${expr1.toRust()} & ${expr2.toRust()})"
    }
}

@SwarmNode
@ExpressionGenNode(BitWiseCompatibleType::class)
data class BitwiseAndLogicalOr(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : BinOpExpression {
    override fun toRust(): String {
        return "(${expr1.toRust()} | ${expr2.toRust()})"
    }
}

@SwarmNode
@ExpressionGenNode(BitWiseCompatibleType::class)
data class BitwiseAndLogicalXor(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : BinOpExpression {
    override fun toRust(): String {
        return "(${expr1.toRust()} ^ ${expr2.toRust()})"
    }
}

@SwarmNode
@ExpressionGenNode(BoolType::class)
data class EqExpression(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : BinOpExpression {
    override fun toRust(): String {
        return "(${expr1.toRust()} == ${expr2.toRust()})"
    }
}

@SwarmNode
@ExpressionGenNode(BoolType::class)
data class NEqExpression(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : BinOpExpression {
    override fun toRust(): String {
        return "(${expr1.toRust()} != ${expr2.toRust()})"
    }
}

@SwarmNode
@ExpressionGenNode(BoolType::class)
data class GTExpression(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : BinOpExpression {
    override fun toRust(): String {
        return "(${expr1.toRust()} > ${expr2.toRust()})"
    }
}

@SwarmNode
@ExpressionGenNode(BoolType::class)
data class GTEExpression(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : BinOpExpression {
    override fun toRust(): String {
        return "(${expr1.toRust()} >= ${expr2.toRust()})"
    }
}

@SwarmNode
@ExpressionGenNode(BoolType::class)
data class LTExpression(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : BinOpExpression {
    override fun toRust(): String {
        return "(${expr1.toRust()} < ${expr2.toRust()})"
    }
}

@SwarmNode
@ExpressionGenNode(BoolType::class)
data class LTEExpression(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : BinOpExpression {
    override fun toRust(): String {
        return "(${expr1.toRust()} <= ${expr2.toRust()})"
    }
}

@SwarmNode
@ExpressionGenNode(NonVoidType::class)
data class GroupedExpression(
    val expression: Expression,
    override val symbolTable: SymbolTable
) : RecursiveExpression {
    override fun toRust(): String {
        return "(${expression.toRust()})"
    }
}

/* Nodes that affect the change of ownership of variables */
@SwarmNode
@ExpressionGenNode(Type::class)
data class FunctionCallExpression(
    val functionName: String,
    val args: List<Expression>,
    override val symbolTable: SymbolTable
) : RecursiveExpression {
    override fun toRust(): String {
        val arguments = args.map { it.toRust() } + "hasher"
        return "$functionName(${arguments.joinToString(",") { it }})"
    }
}

@SwarmNode
@ExpressionGenNode(Type::class)
data class MethodCallExpression(
    val structExpression: Expression,
    val methodName: String,
    val args: List<Expression>,
    override val symbolTable: SymbolTable
) : RecursiveExpression {
    override fun toRust(): String {
        val arguments = args.map { it.toRust() } + "hasher"
        return "${structExpression.toRust()}.$methodName(${arguments.joinToString(",") { it }})"
    }
}

@ExpressionGenNode(StructType::class)
data class StructInstantiationExpression(
    val structName: String,
    val args: List<Pair<String, Expression>>,
    override val symbolTable: SymbolTable
) : LiteralExpression {
    override fun toRust(): String {
        return "$structName {${args.joinToString(" ") { "${it.first}: ${it.second.toRust()}," }}}"
    }
}
/*
@ExpressionGenNode(TraitType::class)
data class TraitInstantiationExpression(
    val traitName:String,
    val traitMap: MutableList<Pair<StructType,MutableList<FunctionDefinition>>> = mutableListOf(),
    override val symbolTable: SymbolTable
) : LiteralExpression{
    override fun toRust(): String {
//        return "pub trait $traitName\n"
        var str1="pub trait $traitName {\n"
        for((key,value) in traitMap){
            for(func in value){
                val self = if (func.addSelfVariable) "&self," else ""
                str1+="    fn ${func.functionName.toString()}($self ${func.arguments.map { "${it.key}:${it.value.toRust()}" }.joinToString { ", " }})->${func.returnType.toRust()};\n"
            }
            str1+="}"
//            break
        }
        var str2=""
        for((key,value) in traitMap){
            str2+="impl ${traitName.toString()} for ${key.structName.toString()}{"
            for(func in value){
                str2+=func.toRust()
            }
            str2+="}\n"
        }
        return str1+str2
    }
}
*/

sealed interface RecursiveStatementBlockExpression : Expression

@ExpressionGenNode(Type::class)
data class BlockExpression(
    val statement: StatementBlock,
    val type: Type?,
    override val symbolTable: SymbolTable
) : RecursiveExpression, RecursiveStatementBlockExpression {
    override fun toRust(): String {
        return "{\n${statement.toRust()}\n}"
    }
}

@ExpressionGenNode(Type::class)
data class ExtractOptionExpression(
    val internalExpression: Expression,
    val matchedSomeVariable: String,
    val matchedStatement: StatementBlock,
    val noneStatement: StatementBlock,
    val type: Type?,
    override val symbolTable: SymbolTable
) : RecursiveExpression, RecursiveStatementBlockExpression {
    override fun toRust(): String {
        return "match (${internalExpression.toRust()}) {\nNone => {\n${noneStatement.toRust()}},\n Some($matchedSomeVariable) => {\n${matchedStatement.toRust()}\n}\n}\n"
    }
}

@ExpressionGenNode(Type::class)
data class IfElseExpression(
    val predicate: Expression,
    val ifBlock: StatementBlock,
    val elseBlock: StatementBlock,
    val type: Type?,
    override val symbolTable: SymbolTable
) : RecursiveExpression, RecursiveStatementBlockExpression {
    override fun toRust(): String {
        return "if (${predicate.toRust()}) {\n ${ifBlock.toRust()} \n} else {\n ${elseBlock.toRust()} \n}"
    }
}

@ExpressionGenNode(VoidType::class)
data class IfExpression(
    val predicate: Expression,
    val ifBlock: StatementBlock,
    override val symbolTable: SymbolTable
) : RecursiveExpression, RecursiveStatementBlockExpression {
    override fun toRust(): String {
        return "if (${predicate.toRust()}) {\n ${ifBlock.toRust()} \n}"
    }
}

// TODO: Change this to be for an arbitrary types to instruct breaks with types too
@SwarmNode
@ExpressionGenNode(VoidType::class)
data class LoopExpression(
    val body: StatementBlock,
    override val symbolTable: SymbolTable
) : RecursiveExpression, RecursiveStatementBlockExpression {
    override fun toRust(): String {
        return "loop {\n ${body.toRust()} \n}"
    }
}

sealed interface ReferencingExpressions : Expression

@SwarmNode
@ExpressionGenNode(ReferenceType::class)
data class ReferenceExpression(
    val expression: Expression,
    override val symbolTable: SymbolTable
) : ReferencingExpressions {
    override fun toRust(): String {
        return "&(${expression.toRust()})"
    }
}

@SwarmNode
@ExpressionGenNode(MutableReferenceType::class)
data class MutableReferenceExpression(
    val expression: Expression,
    override val symbolTable: SymbolTable
) : ReferencingExpressions {
    override fun toRust(): String {
        return "&mut (${expression.toRust()})"
    }
}

@SwarmNode
@ExpressionGenNode(NonVoidType::class)
data class DereferenceExpression(
    val expression: Expression,
    override val symbolTable: SymbolTable
) : RecursiveExpression, LHSAssignmentNode {
    override fun rootNode(): Variable? {
        return if (expression is LHSAssignmentNode) {
            expression.rootNode()
        } else {
            null
        }
    }
    override fun toRust(): String {
        return "(*${expression.toRust()})"
    }
}

@SwarmNode
@ExpressionGenNode(VectorType::class)
data class VectorLiteral(
    val expressions: List<Expression>,
    override val symbolTable: SymbolTable
) : LiteralExpression {
    override fun toRust(): String {
        return "vec![${expressions.joinToString(",") { it.toRust() }}]"
    }
}

@ExpressionGenNode(OptionType::class)
data class SomeLiteral(
    val internalExpression: Expression,
    override val symbolTable: SymbolTable
) : LiteralExpression {
    override fun toRust(): String {
        return "Some::<${internalExpression.toType().toRust()}>(${internalExpression.toRust()})"
    }
}

@ExpressionGenNode(OptionType::class)
data class NoneLiteral(
    val type: Type,
    override val symbolTable: SymbolTable
) : LiteralExpression {
    override fun toRust(): String {
        return "None::<${type.toRust()}>"
    }
}

@SwarmNode
@ExpressionGenNode(StaticSizedArrayType::class)
data class StaticSizedArrayLiteral(
    val expressions: List<Expression>,
    override val symbolTable: SymbolTable
) : LiteralExpression {
    override fun toRust(): String {
        return "[${expressions.joinToString(",") { it.toRust() }}]"
    }
}

@SwarmNode
@ExpressionGenNode(StaticSizedArrayType::class)
data class StaticSizedArrayDefaultLiteral(
    val arraySize: UInt,
    val expression: Expression,
    override val symbolTable: SymbolTable
) : LiteralExpression {
    override fun toRust(): String {
        return "[${expression.toRust()}; $arraySize]"
    }
}

sealed interface NonMovingExpressions : Expression

@ExpressionGenNode(NonVoidType::class)
data class VectorAccess(
    val vectorExpression: Expression,
    val indexExpression: Expression,
    override val symbolTable: SymbolTable
) : RecursiveExpression, NonMovingExpressions, ElementAccessExpression {
    override fun toRust(): String {
//        reconditioned_access
        return "${vectorExpression.toRust()}[reconditioned_access!(${vectorExpression.toRust()},${indexExpression.toRust()})]"

//        return "${vectorExpression.toRust()}[${indexExpression.toRust()}]"
    }
}

// @SwarmNode
@ExpressionGenNode(USizeType::class)
data class VectorLengthExpression(
    val vectorExpression: Expression,
    override val symbolTable: SymbolTable
) : NonMovingExpressions {
    override fun toRust(): String {
        return "${vectorExpression.toRust()}.len()"
    }
}

// @SwarmNode
@ExpressionGenNode(VoidType::class)
data class VectorPushExpression(
    val vectorExpression: Expression,
    val pushExpression: Expression,
    override val symbolTable: SymbolTable
) : NonMovingExpressions {
    override fun toRust(): String {
        return "${vectorExpression.toRust()}.push(${pushExpression.toRust()})"
    }
}

@SwarmNode
@ExpressionGenNode(BoxType::class)
data class NewBoxExpression(
    val internalExpression: Expression,
    override val symbolTable: SymbolTable
) : Expression {
    override fun toRust(): String {
        return "Box::new(${internalExpression.toRust()})"
    }
}

@SwarmNode
@ExpressionGenNode(RcType::class)
data class NewRcExpression(
    val internalExpression: Expression,
    override val symbolTable: SymbolTable
) : Expression {
    override fun toRust(): String {
        return "Rc::new(${internalExpression.toRust()})"
    }
}

@ExpressionGenNode(HashMapType::class)
data class NewHashMapExpression(
    val keyExpression: Expression,
    val valueExpression: Expression,
    override val symbolTable: SymbolTable
) : LiteralExpression {
    override fun toRust(): String {
        val kType = keyExpression.toType().toRust()
        val vType = valueExpression.toType().toRust()
        return "HashMap::<$kType,$vType>::new()"
    }
}

@ExpressionGenNode(USizeType::class)
data class HashMapLengthExpression(
    val hashMapExpression: Expression,
    override val symbolTable: SymbolTable
) : NonMovingExpressions {
    override fun toRust(): String {
        return "${hashMapExpression.toRust()}.len()"
    }
}
// @ExpressionGenNode(MutableReferenceType::class)
// data class HashMapInsertExpression(
//    val hashMapExpression: Expression,
//    val keyExpression: Expression,
//    val valueExpression: Expression,
//    override val symbolTable: SymbolTable
// ) : NonMovingExpressions {
//    override fun toRust(): String {
//        var keyStr:String="${keyExpression.toRust()}"
//        var valueStr:String="${valueExpression.toRust()}"
//        return "${hashMapExpression.toRust()}.entry("+keyStr+")"+".or_insert("+valueStr+")"
//    }
// }

@SwarmNode
@ExpressionGenNode(TypeAliasType::class)
data class TypeAliasExpression(
    val internalExpression: Expression,
    override val symbolTable: SymbolTable
) : Expression {
    override fun toRust(): String {
        return internalExpression.toRust()
    }
}

@SwarmNode
@ExpressionGenNode(I32Type::class)
data class BoxDereferenceExpression(
    val internalExpression: Expression,
    override val symbolTable: SymbolTable
) : LHSAssignmentNode, RecursiveExpression {
    override fun rootNode(): Variable? {
        return if (internalExpression is LHSAssignmentNode) {
            internalExpression.rootNode()
        } else {
            null
        }
    }
    override fun toRust(): String {
        return "(*${internalExpression.toRust()})"
    }
}

sealed interface ReconditionedExpression : Expression

data class WrappingAdd(val addExpression: AddExpression, override val symbolTable: SymbolTable) :
    ReconditionedExpression {
    override fun toRust(): String {
        return "${addExpression.expr1.toRust()}.wrapping_add(${addExpression.expr2.toRust()})"
    }
}

data class WrappingMul(
    val multiplyExpression: MultiplyExpression,
    override val symbolTable: SymbolTable
) :
    ReconditionedExpression {
    override fun toRust(): String {
        return "${multiplyExpression.expr1.toRust()}.wrapping_mul(${multiplyExpression.expr2.toRust()})"
    }
}

data class WrappingSubtract(
    val subtractExpression: SubtractExpression,
    override val symbolTable: SymbolTable
) :
    ReconditionedExpression {
    override fun toRust(): String {
        return "${subtractExpression.expr1.toRust()}.wrapping_sub(${subtractExpression.expr2.toRust()})"
    }
}

data class ReconditionedDivisionExpression(
    val divideExpression: DivideExpression,
    override val symbolTable: SymbolTable
) :
    ReconditionedExpression {
    override fun toRust(): String {
        val zeroExpression = (divideExpression.expr1.toType() as NumberType).zero(symbolTable)
        return "${ReconditionedDivision.macroName}!(${divideExpression.expr1.toRust()}, ${divideExpression.expr2.toRust()}, ${zeroExpression.toRust()})"
    }
}

data class ReconditionedModExpression(
    val modExpression: ModExpression,
    override val symbolTable: SymbolTable
) :
    ReconditionedExpression {
    override fun toRust(): String {
        val zeroExpression = (modExpression.expr1.toType() as NumberType).zero(symbolTable)
        return "${ReconditionedMod.macroName}!(${modExpression.expr1.toRust()}, ${modExpression.expr2.toRust()}, ${zeroExpression.toRust()})"
    }
}

data class ReconditionedIndexAccess(
    val vectorAccessExpression: VectorAccess,
    override val symbolTable: SymbolTable
) :
    ReconditionedExpression {
    override fun toRust(): String {
        return "${ReconditionedArrayAccess.macroName}!(${vectorAccessExpression.vectorExpression.toRust()}, ${vectorAccessExpression.indexExpression.toRust()})"
    }
}

fun Expression.toType(): Type {
    return when (this) {
        is Int8Literal -> I8Type
        is Int16Literal -> I16Type
        is Int32Literal -> I32Type
        is Int64Literal -> I64Type
        is Int128Literal -> I128Type
        is UInt8Literal -> U8Type
        is UInt16Literal -> U16Type
        is UInt32Literal -> U32Type
        is UInt64Literal -> U64Type
        is UInt128Literal -> U128Type
        is USizeLiteral -> USizeType
        is Float32Literal -> F32Type
        is Float64Literal -> F64Type
        is StringLiteral -> StringType
        is StringLengthExpression -> USizeType
        is StringPushStrExpression -> VoidType
        is BooleanLiteral -> BoolType
        is Variable -> symbolTable[this.value]!!.type
        is WrappingAdd -> this.addExpression.toType()
        is WrappingMul -> this.multiplyExpression.toType()
        is WrappingSubtract -> this.subtractExpression.toType()
        is ReconditionedDivisionExpression -> this.divideExpression.toType()
        is GroupedExpression -> this.expression.toType()
        is BlockExpression -> this.type!!
        is ReconditionedModExpression -> this.modExpression.toType()
        is IfElseExpression -> this.type!!
        is FunctionCallExpression -> (symbolTable.functionSymbolTable[this.functionName]!!.type as FunctionType).returnType.clone()
        is TupleLiteral -> TupleType(this.values.map { it.toType() })
        is StructInstantiationExpression -> symbolTable.globalSymbolTable[this.structName]!!.type.clone()
//        is TraitInstantiationExpression -> TraitType(this.traitName) // 返回值类型？
        is TupleElementAccessExpression -> (this.expression.toType() as TupleType).types[this.index]
        is StructElementAccessExpression -> (this.expression.toType() as StructType).types.first { it.first == elementName }.second
//        is HashMapElementAccessExpression -> this.getValueType(this.expression)
        is LoopExpression -> VoidType
        is VoidLiteral -> VoidType
        is IfExpression -> VoidType
        is CLIArgumentAccessExpression -> this.type
        is ReferenceExpression -> ReferenceType(this.expression.toType(), this.symbolTable.depth.value.toUInt())
        is MutableReferenceExpression -> MutableReferenceType(
            this.expression.toType(),
            this.symbolTable.depth.value.toUInt()
        )
        is DereferenceExpression -> (this.expression.toType() as ReferencingTypes).internalType
        is AddExpression -> this.expr1.toType()
        is BitwiseAndLogicalAnd -> this.expr1.toType()
        is BitwiseAndLogicalOr -> this.expr1.toType()
        is BitwiseAndLogicalXor -> this.expr1.toType()
        is DivideExpression -> this.expr1.toType()
        is ModExpression -> this.expr1.toType()
        is MultiplyExpression -> this.expr1.toType()
        is SubtractExpression -> this.expr1.toType()
        is EqExpression -> BoolType
        is NEqExpression -> BoolType
        is GTEExpression -> BoolType
        is GTExpression -> BoolType
        is LTEExpression -> BoolType
        is LTExpression -> BoolType
        is VectorLiteral -> VectorType(this.expressions.first().toType())
        is VectorAccess -> (this.vectorExpression.toType() as VectorType).type
        is ReconditionedIndexAccess -> this.vectorAccessExpression.toType()
        is VectorLengthExpression -> USizeType
        is VectorPushExpression -> VoidType
        is NewBoxExpression -> BoxType(internalExpression.toType())
        is NewRcExpression -> RcType(internalExpression.toType())
        is NewHashMapExpression -> HashMapType(keyExpression.toType(), valueExpression.toType())
        is HashMapLengthExpression -> USizeType
//        is HashMapInsertExpression->MutableReferenceType(valueExpression.toType(),symbolTable.depth.value.toUInt() )
        is BoxDereferenceExpression -> (internalExpression.toType() as BoxType).internalType
        is MethodCallExpression -> symbolTable.globalSymbolTable.structs.find { it.structType.type.structName == (this.structExpression.toType() as StructType).structName }!!.methods.find { it.functionName == methodName }!!.returnType
        is TypeAliasExpression -> internalExpression.toType()
        is StaticSizedArrayDefaultLiteral -> StaticSizedArrayType(expression.toType(), arraySize)
        is StaticSizedArrayLiteral -> StaticSizedArrayType(expressions.first().toType(), expressions.size.toUInt())
        is ElementAccess -> expression.toType()
        is NoneLiteral -> OptionType(type)
        is SomeLiteral -> OptionType(internalExpression.toType())
        is ExtractOptionExpression -> type!!
    }
}

fun Expression.toStatement(addSemicolon: Boolean = true): ExpressionStatement {
    return ExpressionStatement(this, addSemicolon, symbolTable)
}
