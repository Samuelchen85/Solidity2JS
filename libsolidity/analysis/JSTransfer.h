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

#pragma once

#include <libsolidity/analysis/TypeChecker.h>
#include <libsolidity/ast/Types.h>
#include <libsolidity/ast/ASTAnnotations.h>
#include <libsolidity/ast/ASTForward.h>
#include <libsolidity/ast/ASTVisitor.h>

namespace dev{

    namespace solidity{

    /**
     * The module iterate the whole ast and transfer the solidity code to be JS code
     * basically, the Javascript to Solidity source to source tranfer
     */
        class JSTransfer: private ASTConstVisitor{    

        typedef enum{
            ct_declaration = 0,
            ct_assignment = 1,
            ct_unknown = 2,
        }ComposeType;       

        typedef enum{
            cp_var_decl = 0,
            cp_state_assign = 1,
            cp_func_decl = 2,
            cp_func_call = 3,
            cp_event = 4,
            cp_emit = 5,
        }ConversionPhase;

        // class used to compose different parts of a semantic component
        // e.g: a variable declaration int64 a=10;
        class TransferStatus{
            private:
                // when this flag is turned on, then we need to handle 
                // the value side of the var declaration using big number
                bool m_bignum_flag;

                bool m_is_function_declaration_flag;
                bool m_is_function_call_flag;
                bool m_is_event_flag;
                bool m_is_emit_flag;
                bool m_is_pure_statement_flag;

            public:
                TransferStatus():m_bignum_flag(false), m_is_function_declaration_flag(false),   \
                                m_is_function_call_flag(false), m_is_event_flag(false), \
                                m_is_emit_flag(false), m_is_pure_statement_flag(false){}
                ~TransferStatus(){}

                inline bool getBignumFlag(){ return m_bignum_flag; }
                inline void setBignumFlag(bool flag){m_bignum_flag = flag;}

                inline bool isFunctionDeclaration(){ return m_is_function_declaration_flag; }
                inline void setIsFunctionDeclaration(bool flag){m_is_function_declaration_flag = flag;}

                inline bool isFunctionCall(){ return m_is_function_call_flag; }
                inline void setIsFunctionCall(bool flag){m_is_function_call_flag = flag;}

                inline bool isEvent(){ return m_is_event_flag; }
                inline void setIsEvent(bool flag){m_is_event_flag = flag;}

                inline bool isEmit(){ return m_is_emit_flag; }
                inline void setIsEmit(bool flag){m_is_emit_flag = flag;}

                inline bool isPureVarDeclaration(){
                    return !m_is_function_declaration_flag &&   \
                            !m_is_function_call_flag &&     \
                            !m_is_event_flag && \
                            !m_is_emit_flag;
                }

                inline bool isPureStatement(){ return m_is_pure_statement_flag; }
                inline void setIsPureStatement(bool flag){ m_is_pure_statement_flag = flag; }

                
                /*
                inline std::string compose_assignment(){
                    std::string compose_str;
                    if(!m_right_part.empty())
                        compose_str = m_left_part + " = " + m_right_part + ";";
                    else
                        compose_str = m_left_part + ";";
                    return compose_str;
                }
                */

                inline void reset(){
                    m_bignum_flag = false;
                    m_is_function_call_flag = false;
                    m_is_function_declaration_flag = false;
                    m_is_event_flag = false;
                    m_is_emit_flag = false;
                    m_is_pure_statement_flag = false;
                }
        };

        public:
            // constants for analysis
            unsigned int SUPPORTED_INT_BITS = 32;

            /// @param _errorReporter provides the error logging functionality.
            //JSTransfer(ErrorReporter& _errorReporter):m_errorReporter(_errorReporter){}

            JSTransfer(std::string const& _source):
                    m_errorReporter(m_errorList),
                    m_errorList(),
                    m_source(_source){
                        m_transfer_status = std::unique_ptr<TransferStatus>(new TransferStatus());
                        indention = 0;
                    }

            bool transfer(ASTNode const& _astRoot);

            // output javascript source code to a source file
            void writeSource(const std::string&);

        private:

            virtual bool visit(SourceUnit const& _sourceUnit) override;
            virtual void endVisit(SourceUnit const& _sourceUnit) override;

            virtual bool visit(PragmaDirective const& _pragma) override;

            virtual bool visit(ImportDirective const& _node) override;

            virtual bool visit(ContractDefinition const& _contract) override;
            virtual void endVisit(ContractDefinition const& _contract) override;

            virtual bool visit(InheritanceSpecifier const& _node) override;

            virtual bool visit(StructDefinition const& _node) override;
            virtual void endVisit(StructDefinition const& _node) override;

            virtual bool visit(UsingForDirective const& _node) override;

            virtual bool visit(EnumDefinition const& _node) override;
            virtual void endVisit(EnumDefinition const& _node) override;

            virtual bool visit(EnumValue const& _node) override;
            virtual void endVisit(EnumValue const& _node) override;

            virtual bool visit(ParameterList const& _node) override;
            virtual void endVisit(ParameterList const& _node) override;

            virtual bool visit(FunctionDefinition const& _node) override;
            virtual void endVisit(FunctionDefinition const& _node) override;

            virtual bool visit(FunctionTypeName const& _node) override;
            virtual void endVisit(FunctionTypeName const& _node) override;

            virtual bool visit(ModifierDefinition const& _modifier) override;
            virtual void endVisit(ModifierDefinition const& _modifier) override;

            virtual bool visit(ModifierInvocation const& _node) override;
            virtual void endVisit(ModifierInvocation const& _node) override;

            virtual bool visit(VariableDeclaration const& _declaration) override;
            virtual void endVisit(VariableDeclaration const& _Declaration) override;

            virtual bool visit(ElementaryTypeName const& _node) override;

            virtual bool visit(UserDefinedTypeName const& _node) override;
            virtual void endVisit(UserDefinedTypeName const& _node) override;

            virtual bool visit(EventDefinition const& _node) override;
            virtual void endVisit(EventDefinition const& _node) override;

            virtual bool visit(Mapping const& _node) override;
            virtual void endVisit(Mapping const& _node) override;

            virtual bool visit(ArrayTypeName const& _node) override;
            virtual void endVisit(ArrayTypeName const& _node) override;

            virtual bool visit(Block const& _node) override;
            virtual void endVisit(Block const& _node) override;

            virtual bool visit(PlaceholderStatement const& _node) override;

            virtual bool visit(IfStatement const& _node) override;
            virtual void endVisit(IfStatement const& _node) override;

            virtual bool visit(WhileStatement const& _node) override;
            virtual void endVisit(WhileStatement const& _node) override;

            virtual bool visit(ForStatement const& _node) override;
            virtual void endVisit(ForStatement const& _node) override;

            virtual bool visit(Continue const& _node) override;

            virtual bool visit(InlineAssembly const& _node) override;
            virtual void endVisit(InlineAssembly const& _node) override;
           
            virtual bool visit(Break const& _node) override;

            virtual bool visit(Return const& _node) override;
            virtual void endVisit(Return const& _node) override;
          
            virtual bool visit(Throw const& _node) override;

            virtual bool visit(EmitStatement const& _node) override;
            virtual void endVisit(EmitStatement const& _node) override;

            virtual bool visit(VariableDeclarationStatement const& _node) override;
            virtual void endVisit(VariableDeclarationStatement const& _node) override;

            virtual bool visit(ExpressionStatement const& _node) override;
            virtual void endVisit(ExpressionStatement const& _node) override;

            virtual bool visit(Conditional const& _node) override;
            virtual void endVisit(Conditional const& _node) override;

            virtual bool visit(Assignment const&) override;
            virtual void endVisit(Assignment const&) override;

            virtual bool visit(TupleExpression const& _node) override;
            virtual void endVisit(TupleExpression const& _node) override;

            virtual bool visit(UnaryOperation const& _node) override;
            virtual void endVisit(UnaryOperation const& _node) override;

            virtual bool visit(BinaryOperation const& _node) override;
            virtual void endVisit(BinaryOperation const& _node) override;

            virtual bool visit(FunctionCall const& _node) override;
            virtual void endVisit(FunctionCall const& _node) override;

            virtual bool visit(NewExpression const& _node) override;
            virtual void endVisit(NewExpression const& _node) override;

            virtual bool visit(MemberAccess const& _node) override;
            virtual void endVisit(MemberAccess const& _node) override;

            virtual bool visit(IndexAccess const& _node) override;
            virtual void endVisit(IndexAccess const& _node) override;

            virtual bool visit(Identifier const& _node) override;

            virtual bool visit(ElementaryTypeNameExpression const& _node) override;
            virtual void endVisit(ElementaryTypeNameExpression const& _node) override;

            virtual bool visit(Literal const& _node) override;

            
            // member functions for handling specific compound semantic components
            void transferFunctionDeclaration(FunctionDefinition const& _function);

            std::string transferVariableDeclaration(VariableDeclaration const& );

            std::string transferExpressionStatement(ExpressionStatement const& );

            void transferIfStatement(IfStatement const&);

            void transferForStatement(ForStatement const&);


            // Utility methods for handling compose parts belong to certain statement
            inline void appendSourceLine(const std::string& sourceLine){
                std::string new_line = sourceLine;
                for(int i=0; i<indention; i++)
                    new_line = "   " + new_line;
                m_js_src.push_back(new_line);
            }

            inline void clearExprStack(){
                while(!m_expr_stack.empty()){
                    m_expr_stack.pop();
                }
            }

            inline std::string getExprFromStack(){
                std::string res = "";
                if(m_expr_stack.size() > 0){
                    res = m_expr_stack.top();
                    m_expr_stack.pop();
                }
                return res;
            }

            /*
            // check the transfer stages
            inline bool isPureVarDeclaration(){
                return !pure_statement_flag && !function_declaration_flag && !function_declaration_flag;
            }

            inline void setIsPureStatement(bool flag){
                pure_statement_flag = flag;
            }
            inline bool isPureStatement(){
                return pure_statement_flag;
            }

            inline void setIsFunctionDeclaration(bool flag){
                function_declaration_flag = flag;
            }
            inline bool isFunctionDeclaration(){
                return function_declaration_flag;
            }

            inline void setIsFunctionCallStatement(bool flag){
                function_call_statement_flag = flag;
            }
            inline bool isFunctionCallStatement(){
                return function_call_statement_flag;
            }
            */

            inline bool isAstHandled(size_t id){
                if(m_ast_id_set.count(id))
                    return true;
                else
                    return false;
            }
            inline void setAstHandled(size_t id){
                m_ast_id_set.insert(id);
            }

            inline std::string popExprStack(){
                std::string pop_str("");
                if(m_expr_stack.size()>0){
                    pop_str = m_expr_stack.top();
                    m_expr_stack.pop();
                }
                return pop_str;
            }
            inline void pushExprStack(const std::string& push_str){
                m_expr_stack.push(push_str);
            }
            inline bool isExprStackEmpty(){
                return m_expr_stack.empty();
            }


            bool isBigNumberType(const ElementaryTypeName* _typeName);

            std::string printSource(ASTNode const& _node);

            TypePointer printType(Expression const& _expression);

            std::string printTypeString(Expression const& _expression);

        private:

            ErrorReporter m_errorReporter;

            ErrorList m_errorList;

            /// Flag that indicates whether a function modifier actually contains '_'.
            bool m_placeholderFound = false;

            /// Flag that indicates whether some version pragma was present.
            bool m_versionPragmaFound = false;

            int m_inLoopDepth = 0;

            SourceUnit const* m_sourceUnit = nullptr;

            ContractDefinition const* m_currentContract;


            std::vector<std::string> m_js_src;

            // for storing source code from single statement
            std::vector<std::string> m_single_stat_src;

            std::stack<std::string> m_expr_stack;

            std::unique_ptr<TransferStatus> m_transfer_status;

            std::set<size_t> m_ast_id_set;

		    
            std::string m_source;

            int indention;

        };
    }
}
