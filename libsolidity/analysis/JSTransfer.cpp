/*
	This file is part of solidity.

	solidity is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	solidity is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with solidity.  If not, see <http://www.gnu.org/licenses/>.

    Author: Congming Chen (ustccmchen@gmail.com)
    Date: Jul.7 2018
*/

#include <fstream>
#include <memory>
#include <libsolidity/ast/AST.h>
#include <libsolidity/ast/ExperimentalFeatures.h>
#include <libsolidity/parsing/Scanner.h>
#include <libsolidity/analysis/SemVerHandler.h>
#include <libsolidity/interface/ErrorReporter.h>
#include <libsolidity/interface/Version.h>
#include <libsolidity/analysis/JSTransfer.h>

using namespace std;
using namespace dev;
using namespace dev::solidity;


bool JSTransfer::transfer(ASTNode const& _astRoot)
{
	_astRoot.accept(*this);
	return true;
}

bool JSTransfer::visit(SourceUnit const& _sourceUnit)
{
	m_versionPragmaFound = false;
	m_sourceUnit = &_sourceUnit;
	cout<<"We are now handling source unit, for now, nothing will be done"<<endl;
	return true;
}

void JSTransfer::endVisit(SourceUnit const& _sourceUnit)
{
	if (!m_versionPragmaFound){
		string errorString("Source file does not specify required compiler version!");
		SemVerVersion recommendedVersion{string(VersionString)};
		if (!recommendedVersion.isPrerelease())
			errorString +=
				"Consider adding \"pragma solidity ^" +
				to_string(recommendedVersion.major()) +
				string(".") +
				to_string(recommendedVersion.minor()) +
				string(".") +
				to_string(recommendedVersion.patch()) +
				string(";\"");
		m_errorReporter.warning(_sourceUnit.location(), errorString);
	}
	m_sourceUnit = nullptr;
}

bool JSTransfer::visit(PragmaDirective const& _pragma)
{
	solAssert(!_pragma.tokens().empty(), "");
	solAssert(_pragma.tokens().size() == _pragma.literals().size(), "");
	if (_pragma.tokens()[0] != Token::Identifier)
		m_errorReporter.syntaxError(_pragma.location(), "Invalid pragma \"" + _pragma.literals()[0] + "\"");

	else if (_pragma.literals()[0] == "experimental")
	{
		solAssert(m_sourceUnit, "");
		vector<string> literals(_pragma.literals().begin() + 1, _pragma.literals().end());
		if (literals.size() == 0)
			m_errorReporter.syntaxError(
				_pragma.location(),
				"Experimental feature name is missing."
			);
		else if (literals.size() > 1)
			m_errorReporter.syntaxError(
				_pragma.location(),
				"Stray arguments."
			);
		else{
			string const literal = literals[0];
			if (literal.empty())
				m_errorReporter.syntaxError(_pragma.location(), "Empty experimental feature name is invalid.");
			else if (!ExperimentalFeatureNames.count(literal))
				m_errorReporter.syntaxError(_pragma.location(), "Unsupported experimental feature name.");
			else if (m_sourceUnit->annotation().experimentalFeatures.count(ExperimentalFeatureNames.at(literal)))
				m_errorReporter.syntaxError(_pragma.location(), "Duplicate experimental feature name.");
			else
			{
				auto feature = ExperimentalFeatureNames.at(literal);
				m_sourceUnit->annotation().experimentalFeatures.insert(feature);
				if (!ExperimentalFeatureOnlyAnalysis.count(feature))
					m_errorReporter.warning(_pragma.location(), "Experimental features are turned on. Do not use experimental features on live deployments.");
			}
		}
	}
	else if (_pragma.literals()[0] == "solidity")
	{
		vector<Token::Value> tokens(_pragma.tokens().begin() + 1, _pragma.tokens().end());
		vector<string> literals(_pragma.literals().begin() + 1, _pragma.literals().end());
		SemVerMatchExpressionParser parser(tokens, literals);
		auto matchExpression = parser.parse();
		SemVerVersion currentVersion{string(VersionString)};
		if (!matchExpression.matches(currentVersion))
			m_errorReporter.syntaxError(
				_pragma.location(),
				"Source file requires different compiler version (current compiler is " +
				string(VersionString) + " - note that nightly builds are considered to be "
				"strictly less than the released version"
			);
		m_versionPragmaFound = true;
	}
	else
		m_errorReporter.syntaxError(_pragma.location(), "Unknown pragma \"" + _pragma.literals()[0] + "\"");

	return true;
}

bool JSTransfer::visit(ContractDefinition const& _contract){
	m_currentContract = &_contract;
	m_contract_name = _contract.name();
	indention++;
	
	// for init function
	appendInitSourceLine("init: function(){\n", false);
	appendInitSourceLine("},\n", false);

	return true;
}

void JSTransfer::endVisit(ContractDefinition const&){
	indention--;
	//appendSourceLine("}");
}

bool JSTransfer::visit(ModifierDefinition const& _modifier)
{
	m_placeholderFound = false;
	return true;
}

void JSTransfer::endVisit(ModifierDefinition const& _modifier)
{
	if (!m_placeholderFound)
		m_errorReporter.syntaxError(_modifier.body().location(), "Modifier body does not contain '_'.");
	m_placeholderFound = false;
}

bool JSTransfer::visit(WhileStatement const&)
{
	m_inLoopDepth++;
	return true;
}

void JSTransfer::endVisit(WhileStatement const&)
{
	m_inLoopDepth--;
}

bool JSTransfer::visit(Continue const& _continueStatement)
{
	if (m_inLoopDepth <= 0)
		// we're not in a for/while loop, report syntax error
		m_errorReporter.syntaxError(_continueStatement.location(), "\"continue\" has to be in a \"for\" or \"while\" loop.");
	return true;
}

bool JSTransfer::visit(Break const& _breakStatement)
{
	if (m_inLoopDepth <= 0)
		// we're not in a for/while loop, report syntax error
		m_errorReporter.syntaxError(_breakStatement.location(), "\"break\" has to be in a \"for\" or \"while\" loop.");
	return true;
}

bool JSTransfer::visit(Throw const& _throwStatement)
{
	bool const v050 = m_sourceUnit->annotation().experimentalFeatures.count(ExperimentalFeature::V050);

	if (v050)
		m_errorReporter.syntaxError(
			_throwStatement.location(),
			"\"throw\" is deprecated in favour of \"revert()\", \"require()\" and \"assert()\"."
		);
	else
		m_errorReporter.warning(
			_throwStatement.location(),
			"\"throw\" is deprecated in favour of \"revert()\", \"require()\" and \"assert()\"."
		);

	return true;
}

bool JSTransfer::visit(PlaceholderStatement const&)
{
	m_placeholderFound = true;
	return true;
}


void JSTransfer::transferFunctionDeclaration(FunctionDefinition const& _function){

	// preset flags when starting function definition process
	setAstHandled(_function.id());
	m_transfer_status->setIsFunctionDeclaration(true);

	string func_name = _function.name();
	if(_function.visibility() == CallableDeclaration::Visibility::Private){
		func_name = "_" + func_name;
	}
	
	string source_line = func_name + ": function(";

	vector<ASTPointer<VariableDeclaration>> paras = _function.parameters();
	for(size_t i =0; i<paras.size(); i++){
		ASTPointer<VariableDeclaration> para = paras[i];
		setAstHandled(para->id());
		clearExprStack();
		source_line += transferVariableDeclaration(*para);
		if(i < paras.size()-1)
			source_line += ", ";
	}
	source_line += "){\n";

	// for return parameterlist, we do not need to handle for js
	appendSourceLine(source_line);

	// reset related flags
	m_transfer_status->setIsFunctionDeclaration(false);

	indention++;
}


bool JSTransfer::visit(FunctionDefinition const& _function)
{
	if(isAstHandled(_function.id()))
		return true;

	transferFunctionDeclaration(_function);
	
	return true;
}

void JSTransfer::endVisit(FunctionDefinition const&){
	indention--;
	appendSourceLine("},\n");
}


bool JSTransfer::visit(FunctionTypeName const& _node)
{
	if(isAstHandled(_node.id()))
		return true;

	for (auto const& decl: _node.parameterTypeList()->parameters())
		if (!decl->name().empty())
			m_errorReporter.warning(decl->location(), "Naming function type parameters is deprecated.");

	for (auto const& decl: _node.returnParameterTypeList()->parameters())
		if (!decl->name().empty())
			m_errorReporter.warning(decl->location(), "Naming function type return parameters is deprecated.");

	return true;
}

void JSTransfer::endVisit(FunctionTypeName const&){

}

// function parameter variable declaration is different from normal var declaration
string JSTransfer::transferVariableDeclaration(VariableDeclaration const& _declaration){

	setAstHandled(_declaration.id());
	setAstHandled(_declaration.typeName()->id());			// no need to handle type separately

	string varName = _declaration.name();
	string exprValue = "";

	if(varName.empty())
		return "";

	if(isBigNumberType((ElementaryTypeName*)_declaration.typeName())){
		m_transfer_status->setBignumFlag(true);
	}

	if(_declaration.value()){
		_declaration.value()->accept(*this);
		exprValue = popExprStack();
	}

	string source_line = "";
	if(m_transfer_status->isPureVarDeclaration()){
		// for variable declaration, we put it into the init function
		//appendSourceLine(source_line);

		source_line = "this." + varName;
		if(!exprValue.empty())
			source_line += " = " + exprValue;
		else
			source_line += " = null";
		source_line += ";\n";
		appendInitSourceLine(source_line);

	}else{
		if(m_transfer_status->isFunctionDeclaration()){
			source_line = varName;
		}else{
			source_line = "var " + varName;
			if(!exprValue.empty())
				source_line += " = " + exprValue;
		}
		pushExprStack(source_line);
	}

	return source_line;
}


bool JSTransfer::visit(VariableDeclaration const& _declaration)
{
	if(isAstHandled(_declaration.id()))
		return true;

	transferVariableDeclaration(_declaration);
	return true;
}

void JSTransfer::endVisit(VariableDeclaration const& _declaration){
	if(isAstHandled(_declaration.id()))
		return;

	setAstHandled(_declaration.id());
}

void JSTransfer::transferVariableDeclarationStatement(VariableDeclarationStatement const& _statement){

	m_transfer_status->setIsPureStatement(true);

	string source_line = "";

	// check initial values firstly
	_statement.initialValue()->accept(*this);

	string init_value = popExprStack();

	//check each varaible declaration
	size_t vec_size = _statement.declarations().size();
	for(size_t i=0; i<vec_size; i++){
		ASTPointer<VariableDeclaration> vdptr = _statement.declarations()[i];
		vdptr->accept(*this);
		string var = popExprStack();
		source_line += var + " = " + init_value;
		if(i<vec_size-1){
			source_line += ",";
		}
	}

	m_transfer_status->setIsPureStatement(false);

	// check if we should put this into the source lines
	if(m_transfer_status->isPureVarDeclaration())
		appendSourceLine(source_line);
	else
		pushExprStack(source_line);
}

bool JSTransfer::visit(VariableDeclarationStatement const& _statement){
	if(isAstHandled(_statement.id()))
		return true;

	transferVariableDeclarationStatement(_statement);

	return true;
}
void JSTransfer::endVisit(VariableDeclarationStatement const& _statement){
	if(isAstHandled(_statement.id()))
		return;
	setAstHandled(_statement.id());
}


bool JSTransfer::visit(StructDefinition const& _struct)
{
	bool const v050 = m_sourceUnit->annotation().experimentalFeatures.count(ExperimentalFeature::V050);

	if (_struct.members().empty())
	{
		if (v050)
			m_errorReporter.syntaxError(_struct.location(), "Defining empty structs is disallowed.");
		else
			m_errorReporter.warning(_struct.location(), "Defining empty structs is deprecated.");
	}
	return true;
}

void JSTransfer::endVisit(StructDefinition const& ){

}

bool JSTransfer::visit(ImportDirective const&){
	return true;

}

bool JSTransfer::visit(InheritanceSpecifier const& ){
	return true;

}


bool JSTransfer::visit(UsingForDirective const& ){
	return true;

}

bool JSTransfer::visit(EnumDefinition const& ){
	return true;

}

void JSTransfer::endVisit(EnumDefinition const& ){

}

bool JSTransfer::visit(EnumValue const&){
	return true;

}

void JSTransfer::endVisit(EnumValue const&){

}

bool JSTransfer::visit(ParameterList const&){
	return true;

}
void JSTransfer::endVisit(ParameterList const&){

}

bool JSTransfer::visit(ModifierInvocation const& ){
	return true;

}
void JSTransfer::endVisit(ModifierInvocation const&){

}

bool JSTransfer::visit(ElementaryTypeName const& typeName){
	cout<<"The elementary typename is: "<<typeName.typeName().toString()<<endl;
	return true;
}

bool JSTransfer::visit(UserDefinedTypeName const& ){
	return true;
}
void JSTransfer::endVisit(UserDefinedTypeName const& ){

}

bool JSTransfer::visit(EventDefinition const& ){
	return true;

}
void JSTransfer::endVisit(EventDefinition const& ){

}

bool JSTransfer::visit(Mapping const& ){
	return true;

}
void JSTransfer::endVisit(Mapping const& ){

}

bool JSTransfer::visit(ArrayTypeName const& ){
	return true;

}
void JSTransfer::endVisit(ArrayTypeName const& ){

}

bool JSTransfer::visit(Block const& ){
	return true;

}
void JSTransfer::endVisit(Block const& ){

}

void JSTransfer::transferIfStatement(IfStatement const& _statement){
	
	m_transfer_status->setIsConditionStatement(true);

	_statement.condition().accept(*this);

	string pop_str = "if(" + popExprStack() + "){\n";
	appendSourceLine(pop_str);
	indention++;

	m_transfer_status->setIsConditionStatement(false);

	_statement.trueStatement().accept(*this);

	indention--;

	if(_statement.falseStatement()){
		appendSourceLine("}else{\n");
		indention++;
		_statement.falseStatement()->accept(*this);
		indention--;
	}
}

bool JSTransfer::visit(IfStatement const& _statement){
	if(isAstHandled(_statement.id()))
		return true;

	transferIfStatement(_statement);

	return true;
}

void JSTransfer::endVisit(IfStatement const& _statement){
	if(isAstHandled(_statement.id()))
		return;

	appendSourceLine("}\n");

	setAstHandled(_statement.id());
}

void JSTransfer::transferForStatement(ForStatement const& _statement){

	m_transfer_status->setIsForStatement(true);

	string source_line = "for(";
	
	_statement.initializationExpression()->accept(*this);
	
	source_line += popExprStack() + "; ";

	_statement.condition()->accept(*this);

	source_line += popExprStack() + "; ";

	_statement.loopExpression()->accept(*this);

	source_line += popExprStack();

	source_line += "){\n";

	appendSourceLine(source_line);

	indention++;

	m_transfer_status->setIsForStatement(false);


	// handle all the statements in the loop body
	_statement.body().accept(*this);

	indention--;
}

bool JSTransfer::visit(ForStatement const& _statement)
{
	if(isAstHandled(_statement.id()))
		return true;
	
	transferForStatement(_statement);

	return true;
}

void JSTransfer::endVisit(ForStatement const& _statement)
{
	if(isAstHandled(_statement.id()))
		return;

	appendSourceLine("}\n");

	setAstHandled(_statement.id());
}

bool JSTransfer::visit(InlineAssembly const& ){
	return true;

}
void JSTransfer::endVisit(InlineAssembly const& ){}

bool JSTransfer::visit(Return const& _node){
	if(isAstHandled(_node.id()))
		return true;

	return true;
}
void JSTransfer::endVisit(Return const& _node){
	if(isAstHandled(_node.id()))
		return;

	appendSourceLine("return " + popExprStack() + ";\n");

	setAstHandled(_node.id());
}

bool JSTransfer::visit(EmitStatement const& ){
	return true;

}
void JSTransfer::endVisit(EmitStatement const& ){}

bool JSTransfer::visit(ExpressionStatement const& _node){
	if(isAstHandled(_node.id()))
		return true;

	return true;
}
void JSTransfer::endVisit(ExpressionStatement const& _node){
	if(isAstHandled(_node.id()))
		return;

	// expression statement can be placed in condition statement or for loop statement
	if(!m_transfer_status->isConditionStatement() && !m_transfer_status->isForStatement()){
		string pop_str = popExprStack();
		if(isExprStackEmpty())
			pop_str += ";";
		
		appendSourceLine(pop_str);
	}

	setAstHandled(_node.id());
}

bool JSTransfer::visit(Conditional const& ){
	return true;

}
void JSTransfer::endVisit(Conditional const& ){}

bool JSTransfer::visit(Assignment const& _assignment){
	if(isAstHandled(_assignment.id()))
		return true;
	else
		return true;
}
void JSTransfer::endVisit(Assignment const& _assignment){
	if(isAstHandled(_assignment.id()))
		return;

	TypePointer tp =  _assignment.annotation().type;
	int storage_size = tp->storageBytes();

	Token::Value tk_value = _assignment.assignmentOperator();
	
	string opr2 = popExprStack();
	string opr1 = popExprStack();

	pushExprStack(opr1 + " = " +opr2);

	setAstHandled(_assignment.id());
}

bool JSTransfer::visit(TupleExpression const& ){
	return true;

}
void JSTransfer::endVisit(TupleExpression const& ){}

bool JSTransfer::visit(UnaryOperation const& _operation){
	if(isAstHandled(_operation.id()))
		return true;

	string source_line("");

	// Inc, Dec, Add, Sub, Not, BitNot, Delete
	Token::Value op = _operation.getOperator();

	_operation.subExpression().accept(*this);

	string opr_name = "";
	switch(op){
		case Token::Value::Inc:
			opr_name = ".plus(BigNumber(1))";
			break;
		case Token::Value::Dec:
			opr_name = ".minus(BigNumber(1))";
			break;
		case Token::Value::Not:
			opr_name = "!";
			break;
		case Token::Value::BitNot:
			opr_name = "~";
			break;
		case Token::Value::Delete:
			opr_name = "delete ";
			break;
		default:
			break;
	}

	if(!opr_name.empty()){
		if(_operation.isPrefixOperation())
			pushExprStack(opr_name + popExprStack());
		else
			pushExprStack(popExprStack() + opr_name);
	}

	return true;
}
void JSTransfer::endVisit(UnaryOperation const& _operation){
	if(isAstHandled(_operation.id()))
		return;

	setAstHandled(_operation.id());
}


bool JSTransfer::visit(BinaryOperation const& _operation){
	if(isAstHandled(_operation.id()))
		return true;

	return true;
}

void JSTransfer::endVisit(BinaryOperation const& _operation){

	if(isAstHandled(_operation.id()))
		return;

	//bool big_num_flag = isBigNumberType(_operation.annotation().type);

	if(m_transfer_status->getBignumFlag()){
		if(m_expr_stack.size()>1){
			std::string opr2 = popExprStack();
			std::string opr1 = popExprStack();
			std::string binary_expr = "";

			switch(_operation.getOperator()){
				case Token::Value::Add:
					binary_expr = opr1 + ".plus(" + opr2 + ")";
					break;
				case Token::Value::Sub:
					binary_expr = opr1 + ".minus(" + opr2 + ")";
					break;
				case Token::Value::Mul:
					binary_expr = opr1 + ".mul(" + opr2 + ")";
					break;
				case Token::Value::Div:
					binary_expr = opr1 + ".div(" + opr2 + ")";
					break;
				case Token::Value::Mod:
					binary_expr = opr1 + ".mod(" + opr2 + ")";
					break;
				case Token::Value::Exp:
					binary_expr = opr1 + "**" + opr2;
					break;
				case Token::Value::Equal:
					binary_expr = opr1 + ".equal(" + opr2 + ")";
					break;
				case Token::Value::NotEqual:
					binary_expr = opr1 + "!=" + opr2;
					break;
				case Token::Value::LessThan:
					binary_expr = opr1 + ".lt(" + opr2 + ")";
					break;
				case Token::Value::GreaterThan:
					binary_expr = opr1 + ".gt(" + opr2 + ")";
					break;
				case Token::Value::LessThanOrEqual:
					binary_expr = opr1 + ".lte(" + opr2 + ")";
					break;
				case Token::Value::GreaterThanOrEqual:
					binary_expr = opr1 + ".gte(" + opr2 + ")";
					break;
				case Token::Value::AssignBitOr:
					binary_expr = opr1 + "|=" + opr2;
					break;
				case Token::Value::AssignBitXor:
					binary_expr = opr1 + "^=" + opr2;
					break;
				case Token::Value::AssignBitAnd:
					binary_expr = opr1 + "&=" + opr2;
					break;
				case Token::Value::AssignShl:
					binary_expr = opr1 + "<<=" + opr2;
					break;
				case Token::Value::AssignSar:
					binary_expr = opr1 + ">>=" + opr2;
					break;
				case Token::Value::AssignShr:
					binary_expr = opr1 + ">>>=" + opr2;
					break;
				case Token::Value::AssignAdd:
					binary_expr = opr1 + "+=" + opr2;
					break;
				case Token::Value::AssignSub:
					binary_expr = opr1 + "-=" + opr2;
					break;
				case Token::Value::AssignMul:
					binary_expr = opr1 + "*=" + opr2;
					break;
				case Token::Value::AssignDiv:
					binary_expr = opr1 + "/=" + opr2;
					break;
				case Token::Value::AssignMod:
					binary_expr = opr1 + "%=" + opr2;
					break;
				case Token::Value::Or:
					binary_expr = opr1 + "||" + opr2;
					break;
				case Token::Value::And:
					binary_expr = opr1 + "&&" + opr2;
					break;
				case Token::Value::BitOr:
					binary_expr = opr1 + "|" + opr2;
					break;
				case Token::Value::BitXor:
					binary_expr = opr1 + "^" + opr2;
					break;
				case Token::Value::BitAnd:
					binary_expr = opr1 + "&" + opr2;
					break;
				case Token::Value::SHL:
					binary_expr = opr1 + "<<" + opr2;
					break;
				case Token::Value::SAR:
					binary_expr = opr1 + ">>" + opr2;
					break;
				case Token::Value::SHR:
					binary_expr = opr1 + ">>>" + opr2;
					break;
				default:
					break;
			}

			if(m_expr_stack.size()>0)
				binary_expr = "(" + binary_expr + ")";

			if(!binary_expr.empty())
				pushExprStack(binary_expr);
		}

	}else{
		if(m_expr_stack.size()>1){
			std::string opr2 = popExprStack();
			std::string opr1 = popExprStack();

			std::string binary_expr = "";

			switch(_operation.getOperator()){
				case Token::Value::Add:
					binary_expr = opr1 + "+" + opr2;
					break;
				case Token::Value::Sub:
					binary_expr = opr1 + "-" + opr2;
					break;
				case Token::Value::Mul:
					binary_expr = opr1 + "*" + opr2;
					break;
				case Token::Value::Div:
					binary_expr = opr1 + "/" + opr2;
					break;
				case Token::Value::Mod:
					binary_expr = opr1 + "%" + opr2;
					break;
				case Token::Value::Exp:
					binary_expr = opr1 + "**" + opr2;
					break;
				case Token::Value::Equal:
					binary_expr = opr1 + "==" + opr2;
					break;
				case Token::Value::NotEqual:
					binary_expr = opr1 + "!=" + opr2;
					break;
				case Token::Value::LessThan:
					binary_expr = opr1 + "<" + opr2;
					break;
				case Token::Value::GreaterThan:
					binary_expr = opr1 + ">" + opr2;
					break;
				case Token::Value::LessThanOrEqual:
					binary_expr = opr1 + "<=" + opr2;
					break;
				case Token::Value::GreaterThanOrEqual:
					binary_expr = opr1 + ">=" + opr2;
					break;
				case Token::Value::AssignBitOr:
					binary_expr = opr1 + "|=" + opr2;
					break;
				case Token::Value::AssignBitXor:
					binary_expr = opr1 + "^=" + opr2;
					break;
				case Token::Value::AssignBitAnd:
					binary_expr = opr1 + "&=" + opr2;
					break;
				case Token::Value::AssignShl:
					binary_expr = opr1 + "<<=" + opr2;
					break;
				case Token::Value::AssignSar:
					binary_expr = opr1 + ">>=" + opr2;
					break;
				case Token::Value::AssignShr:
					binary_expr = opr1 + ">>>=" + opr2;
					break;
				case Token::Value::AssignAdd:
					binary_expr = opr1 + "+=" + opr2;
					break;
				case Token::Value::AssignSub:
					binary_expr = opr1 + "-=" + opr2;
					break;
				case Token::Value::AssignMul:
					binary_expr = opr1 + "*=" + opr2;
					break;
				case Token::Value::AssignDiv:
					binary_expr = opr1 + "/=" + opr2;
					break;
				case Token::Value::AssignMod:
					binary_expr = opr1 + "%=" + opr2;
					break;
				case Token::Value::Or:
					binary_expr = opr1 + "||" + opr2;
					break;
				case Token::Value::And:
					binary_expr = opr1 + "&&" + opr2;
					break;
				case Token::Value::BitOr:
					binary_expr = opr1 + "|" + opr2;
					break;
				case Token::Value::BitXor:
					binary_expr = opr1 + "^" + opr2;
					break;
				case Token::Value::BitAnd:
					binary_expr = opr1 + "&" + opr2;
					break;
				case Token::Value::SHL:
					binary_expr = opr1 + "<<" + opr2;
					break;
				case Token::Value::SAR:
					binary_expr = opr1 + ">>" + opr2;
					break;
				case Token::Value::SHR:
					binary_expr = opr1 + ">>>" + opr2;
					break;
				default:
					break;
			}

			if(m_expr_stack.size()>0)
				binary_expr = "(" + binary_expr + ")";

			if(!binary_expr.empty())
				pushExprStack(binary_expr);
		}
	}

	setAstHandled(_operation.id());
}

bool JSTransfer::visit(FunctionCall const& function){
	return true;

}
void JSTransfer::endVisit(FunctionCall const& ){}

bool JSTransfer::visit(NewExpression const& ){
	return true;

}
void JSTransfer::endVisit(NewExpression const& ){}

bool JSTransfer::visit(MemberAccess const& ){
	return true;

}
void JSTransfer::endVisit(MemberAccess const& ){}

bool JSTransfer::visit(IndexAccess const& ){
	return true;

}
void JSTransfer::endVisit(IndexAccess const& ){}

bool JSTransfer::visit(Identifier const& _id){
	if(isAstHandled(_id.id()))
		return true;

	pushExprStack(_id.name());
	setAstHandled(_id.id());
	return true;
}

bool JSTransfer::visit(ElementaryTypeNameExpression const& ){
	return true;

}
void JSTransfer::endVisit(ElementaryTypeNameExpression const& ){}

bool JSTransfer::visit(Literal const& _node){
	if(isAstHandled(_node.id()))
		return true;

	TypePointer nodeType = _node.annotation().type;
	unsigned int storage_bytes = nodeType->storageBytes();
	int memory_size = nodeType->memoryHeadSize();
	u256 storage_size = nodeType->storageSize();
	int stack_size = nodeType->sizeOnStack();
	
	string literal_str = printSource(_node);

	if((nodeType->category() == Type::Category::Integer ||	\
		nodeType->category() == Type::Category::RationalNumber)){
		pushExprStack("BigNumber(\'" + literal_str + "\')");

	}else{
		pushExprStack(literal_str);
	}
	
	setAstHandled(_node.id());
	return true;
}


//====================== Public methods definitions ========================

void JSTransfer::writeSource(const string& srcFileName){
	try{
		fstream of(srcFileName, ios::out);
		of<<"// Smart contract transferred from Solidity, based on 0.4.24"<<endl<<endl;
		of<<"\'use strict\';"<<endl<<endl;

		// write contract header
		of<<m_contract_name+".prototype = {\n"<<endl<<endl;

		// write init function
		for(size_t i=0; i<m_js_init_src->size(); i++){
			if(i>0 && i<m_js_init_src->size()-1)
				of<<indent_space<<m_js_init_src->at(i)<<endl;
			else
				of<<m_js_init_src->at(i)<<endl;
		}

		// write other sources
		vector<string>::iterator iter = m_js_src.begin();
		while(iter != m_js_src.end()){
			of<<*iter<<endl;
			iter++;
		}

		of<<"}\n"<<endl;
		of<<"module.exports = "<<m_contract_name<<";"<<endl;

		of.close();
		m_js_src.clear();

	}catch(Exception e){
		cerr<<e.what()<<endl;
	}
}

bool JSTransfer::isBigNumberType(const ElementaryTypeName* eleTypeName){
	cout<<"type name is: "<<eleTypeName->id()<<endl;
	ElementaryTypeNameToken token = eleTypeName->typeName();
	Token::Value value = token.token();
	unsigned int firstNum = token.firstNumber();
	unsigned int secondNum = token.secondNumber();
	// check if we need to use big number here
	if(firstNum > SUPPORTED_INT_BITS || secondNum > SUPPORTED_INT_BITS){
		return true;
	}else{
		if(token.toString(value).compare("int") || token.toString(value).compare("uint"))
			return true;
	}
	return false;
}

/*
bool JSTransfer::isBigNumberType(const ElementaryTypeName* eleTypeName){
	cout<<"type name is: "<<eleTypeName->id()<<endl;
	ElementaryTypeNameToken token = eleTypeName->typeName();
	Token::Value value = token.token();
	unsigned int firstNum = token.firstNumber();
	unsigned int secondNum = token.secondNumber();
	// check if we need to use big number here
	if(firstNum > SUPPORTED_INT_BITS || secondNum > SUPPORTED_INT_BITS){
		return true;
	}else{
		if(token.toString(value).compare("int") || token.toString(value).compare("uint"))
			return true;
	}
	return false;
}
*/

string JSTransfer::printSource(ASTNode const& _node)
{
	if (!m_source.empty())
	{
		SourceLocation const& location(_node.location());
		return m_source.substr(location.start, location.end - location.start);
	}
	return "";
}

TypePointer JSTransfer::printType(Expression const& _expression)
{
	if (_expression.annotation().type){
		TypePointer tp = _expression.annotation().type;
		string sv = tp->canonicalName();
		return tp;
	}else
		return nullptr;
}

string JSTransfer::printTypeString(Expression const& _expression)
{
	if (_expression.annotation().type)
		return _expression.annotation().type->toString();
	else
		return "unknown";
}
