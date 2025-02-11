package com.oracle.truffle.sl.parser;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.api.strings.TruffleString;
import com.oracle.truffle.sl.SLLanguage;
import com.oracle.truffle.sl.nodes.SLExpressionNode;
import com.oracle.truffle.sl.nodes.SLRootNode;
import com.oracle.truffle.sl.nodes.SLStatementNode;
import com.oracle.truffle.sl.nodes.controlflow.*;
import com.oracle.truffle.sl.nodes.expression.*;
import com.oracle.truffle.sl.nodes.local.*;
import com.oracle.truffle.sl.nodes.util.SLUnboxNodeGen;
import com.oracle.truffle.sl.runtime.SLStrings;
import syspro.tm.Tasks;
import syspro.tm.lexer.IdentifierToken;
import syspro.tm.lexer.IntegerLiteralToken;
import syspro.tm.lexer.StringLiteralToken;
import syspro.tm.lexer.Token;
import syspro.tm.parser.AnySyntaxKind;
import syspro.tm.parser.SyntaxNode;
import syspro.tm.parser.TextSpan;
import syspro.tm.symbols.*;

import java.util.*;

import static syspro.tm.parser.SyntaxKind.*;

public class SimpleLanguageParser {
	private static final LanguageServer languageServer = Tasks.LanguageServer.authorSolution();

	/* State while parsing a source unit. */
	private final Source source;
	private final Map<TruffleString, RootCallTarget> allFunctions;

	/* State while parsing a function. */
	private FrameDescriptor.Builder frameDescriptorBuilder;

	/* State while parsing a block. */
	private LexicalScope lexicalScope;
	private final SLLanguage language;

	public SimpleLanguageParser(SLLanguage language, Source source) {
		this.language = language;
		this.source = source;
		this.allFunctions = new HashMap<>();
	}

	/*
	public void SemErr(Token token, String message) {
	    assert token != null;
	    throwParseError(source, token.getLine(), token.getCharPositionInLine(), token, message);
	}

	private static void throwParseError(Source source, int line, int charPositionInLine, Token token, String message) {
	    int col = charPositionInLine + 1;
	    String location = "-- line " + line + " col " + col + ": ";
	    int length = token == null ? 1 : Math.max(token.getStopIndex() - token.getStartIndex(), 0);
	    throw new SLParseError(source, line, col, length, String.format("Error(s) parsing script:%n" + location + message));
	}
	 */

	public static Map<TruffleString, RootCallTarget> parseSL(SLLanguage language, Source source) {
		SemanticModel semanticModel = languageServer.buildModel(source.getCharacters().toString());
		SimpleLanguageParser parser = new SimpleLanguageParser(language, source);
		SyntaxNode root = semanticModel.root();
		parser.parseRoot(root);
		return parser.getAllFunctions();
	}

	/**
	 * Local variable names that are visible in the current block. Variables are not visible outside
	 * of their defining block, to prevent the usage of undefined variables. Because of that, we can
	 * decide during parsing if a name references a local variable or is a function name.
	 */
	static final class LexicalScope {
		protected final LexicalScope outer;
		protected final SyntaxNode syntax;
		protected final List<SLStatementNode> body = new ArrayList<>();
		protected final HashMap<String, VariableSymbol> locals = new HashMap<>();

		LexicalScope(LexicalScope outer, SyntaxNode syntax) {
			this.outer = outer;
            this.syntax = syntax;
        }

		public SLBlockNode toBlockNode() {
			final List<SLStatementNode> flattenedNodes = new ArrayList<>(body.size());
			flattenBlocks(body, flattenedNodes);
			for (SLStatementNode statement : flattenedNodes) {
				if (statement.hasSource()) {
					statement.addStatementTag();
				}
			}
			SLBlockNode blockNode = new SLBlockNode(flattenedNodes.toArray(new SLStatementNode[flattenedNodes.size()]));
			srcFromNode(blockNode, syntax);
			return blockNode;
		}

		public VariableSymbol find(String name) {
			VariableSymbol local = locals.get(name);
			return local == null && outer != null ? outer.find(name) : local;
		}
	}

	public Map<TruffleString, RootCallTarget> getAllFunctions() {
		return allFunctions;
	}

	private static void flattenBlocks(Iterable<? extends SLStatementNode> bodyNodes, List<SLStatementNode> flattenedNodes) {
		for (SLStatementNode n : bodyNodes) {
			if (n instanceof SLBlockNode) {
				flattenBlocks(((SLBlockNode) n).getStatements(), flattenedNodes);
			} else {
				flattenedNodes.add(n);
			}
		}
	}

	/**
	 * Returns an {@link SLDebuggerNode} for the given token.
	 *
	 * @param debuggerToken The token containing the debugger node's info.
	 * @return A SLDebuggerNode for the given token.
	 */
	SLStatementNode createDebugger(Token debuggerToken) {
		final SLDebuggerNode debuggerNode = new SLDebuggerNode();
		srcFromToken(debuggerNode, debuggerToken);
		return debuggerNode;
	}

	/**
	 * Returns an {@link SLBreakNode} for the given token.
	 *
	 * @param breakToken The token containing the break node's info.
	 * @return A SLBreakNode for the given token.
	 */
	public SLStatementNode createBreak(Token breakToken) {
		final SLBreakNode breakNode = new SLBreakNode();
		srcFromToken(breakNode, breakToken);
		return breakNode;
	}

	/**
	 * Returns an {@link SLContinueNode} for the given token.
	 *
	 * @param continueToken The token containing the continue node's info.
	 * @return A SLContinueNode built using the given token.
	 */
	public SLStatementNode createContinue(Token continueToken) {
		final SLContinueNode continueNode = new SLContinueNode();
		srcFromToken(continueNode, continueToken);
		return continueNode;
	}

	/**
	 * Returns an {@link SLWhileNode} for the given parameters.
	 *
	 * @param conditionNode The conditional node for this while loop
	 * @param bodyNode The body of the while loop
	 * @return A SLWhileNode built using the given parameters. null if either conditionNode or
	 *         bodyNode is null.
	 */
	public SLStatementNode createWhile(SLExpressionNode conditionNode, SLStatementNode bodyNode) {
		if (conditionNode == null || bodyNode == null) {
			return null;
		}

		conditionNode.addStatementTag();
        return new SLWhileNode(conditionNode, bodyNode);
	}

	/**
	 * Returns an {@link SLIfNode} for the given parameters.
	 *
	 * @param conditionNode The condition node of this if statement
	 * @param thenPartNode The then part of the if
	 * @param elsePartNode The else part of the if (null if no else part)
	 * @return An SLIfNode for the given parameters. null if either conditionNode or thenPartNode is
	 *         null.
	 */
	public SLStatementNode createIf(SLExpressionNode conditionNode, SLStatementNode thenPartNode, SLStatementNode elsePartNode) {
		if (conditionNode == null || thenPartNode == null) {
			return null;
		}

		conditionNode.addStatementTag();
        return new SLIfNode(conditionNode, thenPartNode, elsePartNode);
	}

	/**
	 * Returns an {@link SLReturnNode} for the given parameters.
	 *
	 * @param t The token containing the return node's info
	 * @param valueNode The value of the return (null if not returning a value)
	 * @return An SLReturnNode for the given parameters.
	 */
	public SLStatementNode createReturn(Token t, SLExpressionNode valueNode) {
		final TextSpan span = t.span();
		final int start = span.start;
		final int length = valueNode == null ? span.length : valueNode.getSourceEndIndex() - start;
		final SLReturnNode returnNode = new SLReturnNode(valueNode);
		returnNode.setSourceSection(start, length);
		return returnNode;
	}

	/**
	 * Returns the corresponding subclass of {@link SLExpressionNode} for binary expressions. </br>
	 * These nodes are currently not instrumented.
	 *
	 * @param opToken The operator of the binary expression
	 * @param leftNode The left node of the expression
	 * @param rightNode The right node of the expression
	 * @return A subclass of SLExpressionNode using the given parameters based on the given opToken.
	 *         null if either leftNode or rightNode is null.
	 */
	public SLExpressionNode createBinary(SyntaxNode opToken, SLExpressionNode leftNode, SLExpressionNode rightNode) {
		if (leftNode == null || rightNode == null) {
			return null;
		}
		final SLExpressionNode leftUnboxed = SLUnboxNodeGen.create(leftNode);
		final SLExpressionNode rightUnboxed = SLUnboxNodeGen.create(rightNode);

		final SLExpressionNode result;
		AnySyntaxKind kind = opToken.kind();
		if (kind == ADD_EXPRESSION) {
			result = SLAddNodeGen.create(leftUnboxed, rightUnboxed);
		} else if (kind == MULTIPLY_EXPRESSION) {
			result = SLMulNodeGen.create(leftUnboxed, rightUnboxed);
		} else if (kind == DIVIDE_EXPRESSION) {
			result = SLDivNodeGen.create(leftUnboxed, rightUnboxed);
		} else if (kind == SUBTRACT_EXPRESSION) {
			result = SLSubNodeGen.create(leftUnboxed, rightUnboxed);
		} else if (kind == LESS_THAN_EXPRESSION) {
			result = SLLessThanNodeGen.create(leftUnboxed, rightUnboxed);
		} else if (kind == LESS_THAN_OR_EQUAL_EXPRESSION) {
			result = SLLessOrEqualNodeGen.create(leftUnboxed, rightUnboxed);
		} else if (kind == GREATER_THAN_EXPRESSION) {
			result = SLLogicalNotNodeGen.create(SLLessOrEqualNodeGen.create(leftUnboxed, rightUnboxed));
		} else if (kind == GREATER_THAN_OR_EQUAL_EXPRESSION) {
			result = SLLogicalNotNodeGen.create(SLLessThanNodeGen.create(leftUnboxed, rightUnboxed));
		} else if (kind == EQUALS_EXPRESSION) {
			result = SLEqualNodeGen.create(leftUnboxed, rightUnboxed);
		} else if (kind == NOT_EQUALS_EXPRESSION) {
			result = SLLogicalNotNodeGen.create(SLEqualNodeGen.create(leftUnboxed, rightUnboxed));
		} else if (kind == LOGICAL_AND_EXPRESSION) {
			result = new SLLogicalAndNode(leftUnboxed, rightUnboxed);
		} else if (kind == LOGICAL_OR_EXPRESSION) {
			result = new SLLogicalOrNode(leftUnboxed, rightUnboxed);
		} else {
			throw new RuntimeException("unexpected operation: " + opToken.kind());
		}

		int start = leftNode.getSourceCharIndex();
		int length = rightNode.getSourceEndIndex() - start;
		result.setSourceSection(start, length);
		result.addExpressionTag();

		return result;
	}

	/**
	 * Returns an {@link SLInvokeNode} for the given parameters.
	 *
	 * @param functionNode The function being called
	 * @param parameterNodes The parameters of the function call
	 * @return An SLInvokeNode for the given parameters. null if functionNode or any of the
	 *         parameterNodes are null.
	 */
	public SLExpressionNode createCall(SLExpressionNode functionNode, List<SLExpressionNode> parameterNodes) {
		if (functionNode == null || containsNull(parameterNodes)) {
			return null;
		}

        return new SLInvokeNode(functionNode, parameterNodes.toArray(new SLExpressionNode[parameterNodes.size()]));
	}

	/**
	 * Returns a {@link SLReadLocalVariableNode} if this read is a local variable or a
	 * {@link SLFunctionLiteralNode} if this read is global. In SL, the only global names are
	 * functions.
	 *
	 * @param nameNode The name of the variable/function being read
	 * @return either:
	 *         <ul>
	 *         <li>A SLReadLocalVariableNode representing the local variable being read.</li>
	 *         <li>A SLFunctionLiteralNode representing the function definition.</li>
	 *         <li>null if nameNode is null.</li>
	 *         </ul>
	 */
	public SLExpressionNode createRead(SLExpressionNode nameNode) {
		throw new IllegalStateException();
		/*
		if (nameNode == null) {
			return null;
		}

		TruffleString name = ((SLStringLiteralNode) nameNode).executeGeneric(null);
		final SLExpressionNode result;
		final Integer frameSlot = lexicalScope.find(name);
		if (frameSlot != null) {
			/* Read of a local variable. *//*
			result = SLReadLocalVariableNodeGen.create(frameSlot);
		} else {
			/* Read of a global name. In our language, the only global names are functions. *//*
			result = new SLFunctionLiteralNode(name);
		}
		result.setSourceSection(nameNode.getSourceCharIndex(), nameNode.getSourceLength());
		result.addExpressionTag();
		return result;
		*/
	}

	public SLExpressionNode createStringLiteral(StringLiteralToken token) {
		final SLStringLiteralNode result = new SLStringLiteralNode(SLStrings.fromJavaString(token.value));
		srcFromToken(result, token);
		result.addExpressionTag();
		return result;
	}

	public SLExpressionNode createStringLiteral(IdentifierToken token) {
		final SLStringLiteralNode result = new SLStringLiteralNode(SLStrings.fromJavaString(token.value));
		srcFromToken(result, token);
		result.addExpressionTag();
		return result;
	}

	public SLExpressionNode createNumericLiteral(IntegerLiteralToken literalToken) {
		final SLExpressionNode result = new SLLongLiteralNode(literalToken.value);
		srcFromToken(result, literalToken);
		result.addExpressionTag();
		return result;
	}

	public SLExpressionNode createParenExpression(SLExpressionNode expressionNode, int start, int length) {
		if (expressionNode == null) {
			return null;
		}

		final SLParenExpressionNode result = new SLParenExpressionNode(expressionNode);
		result.setSourceSection(start, length);
		return result;
	}

	/**
	 * Returns an {@link SLReadPropertyNode} for the given parameters.
	 *
	 * @param receiverNode The receiver of the property access
	 * @param nameNode The name of the property being accessed
	 * @return An SLExpressionNode for the given parameters. null if receiverNode or nameNode is
	 *         null.
	 */
	public SLExpressionNode createReadProperty(SLExpressionNode receiverNode, SLExpressionNode nameNode) {
		if (receiverNode == null || nameNode == null) {
			return null;
		}

		final SLExpressionNode result = SLReadPropertyNodeGen.create(receiverNode, nameNode);

		final int startPos = receiverNode.getSourceCharIndex();
		final int endPos = nameNode.getSourceEndIndex();
		result.setSourceSection(startPos, endPos - startPos);
		result.addExpressionTag();

		return result;
	}

	/**
	 * Returns an {@link SLWritePropertyNode} for the given parameters.
	 *
	 * @param receiverNode The receiver object of the property assignment
	 * @param nameNode The name of the property being assigned
	 * @param valueNode The value to be assigned
	 * @return An SLExpressionNode for the given parameters. null if receiverNode, nameNode or
	 *         valueNode is null.
	 */
	public SLExpressionNode createWriteProperty(SLExpressionNode receiverNode, SLExpressionNode nameNode, SLExpressionNode valueNode) {
		if (receiverNode == null || nameNode == null || valueNode == null) {
			return null;
		}

		final SLExpressionNode result = SLWritePropertyNodeGen.create(receiverNode, nameNode, valueNode);

		final int start = receiverNode.getSourceCharIndex();
		final int length = valueNode.getSourceEndIndex() - start;
		result.setSourceSection(start, length);
		result.addExpressionTag();

		return result;
	}

	/**
	 * Creates source description of a single token.
	 */
	private static void srcFromToken(SLStatementNode node, Token token) {
		final TextSpan span = token.span();
		node.setSourceSection(span.start, span.length);
	}

	/**
	 * Creates source description of a single token.
	 */
	private static void srcFromNode(SLStatementNode node, SyntaxNode syntax) {
		final TextSpan span = syntax.span();
		node.setSourceSection(span.start, span.length);
	}

	/**
	 * Checks whether a list contains a null.
	 */
	private static boolean containsNull(List<?> list) {
		for (Object e : list) {
			if (e == null) {
				return true;
			}
		}
		return false;
	}

	private void parseRoot(SyntaxNode root) {
		assert root.kind() == SOURCE_TEXT;
		SyntaxNode rootList = root.slot(0);
		assert rootList.kind() == LIST;
		for (int i = 0; i < rootList.slotCount(); i++) {
			final SyntaxNode typeDef = rootList.slot(i);
			assert typeDef.kind() == TYPE_DEFINITION;
			final SyntaxNode bodyList = typeDef.slot(7);
			assert bodyList.kind() == LIST;
			for (int j = 0; j < bodyList.slotCount(); j++) {
				final SyntaxNode memberDef = bodyList.slot(j);
				if (memberDef.kind() == FUNCTION_DEFINITION) {
					parseFunction(memberDef);
				} else {
					assert false : memberDef.kind();
				}
			}
		}
	}

	private SourceSection toSourceSection(SyntaxNode node) {
		final TextSpan span = node.span();
		return source.createSection(span.start, span.length);
	}

	private FrameSlotKind toFrameSlotKind(TypeLikeSymbol symbol) {
		String name = symbol.name();
        return switch (name) {
            case "Boolean" -> FrameSlotKind.Boolean;
            case "Int32", "UInt32", "Rune" -> FrameSlotKind.Int;
            case "Int64", "UInt64" -> FrameSlotKind.Long;
            default -> FrameSlotKind.Object;
        };
	}

	private final IdentityHashMap<VariableSymbol, LocalSlot> locals = new IdentityHashMap<>();

	private static final class LocalSlot {
		final int frameSlot;

        private LocalSlot(int frameSlot) {
            this.frameSlot = frameSlot;
        }
    }

	private void addStatement(SLStatementNode node) {
		lexicalScope.body.add(node);
	}

	private void parseFunction(SyntaxNode memberDef) {
		IdentifierToken nameToken = (IdentifierToken) memberDef.slot(2).token();
		SyntaxNode bodyNode = memberDef.slot(9);

		final var functionName = SLStrings.fromJavaString(nameToken.value);

		final var functionScope = new LexicalScope(null, memberDef);
		try {
			assert frameDescriptorBuilder == null;
			assert lexicalScope == null;
			frameDescriptorBuilder = FrameDescriptor.newBuilder();
			lexicalScope = functionScope;

			FunctionSymbol symbol = (FunctionSymbol) ((SyntaxNodeWithSymbols) memberDef).symbol();
			int paramCount = 0;
			for (VariableSymbol param : symbol.parameters()) {
				SLReadArgumentNode paramValue = new SLReadArgumentNode(paramCount++);
				int localSlot = createLocalSlot(param);
				functionScope.locals.put(param.name(), param);
				SourceSection sourceSection = toSourceSection(param.definition());
				SLWriteLocalVariableNode copyNode = SLWriteLocalVariableNodeGen.create(paramValue, localSlot, sourceSection, true);
				copyNode.addNonStatementTag();
				addStatement(copyNode);
			}
			for (VariableSymbol local : symbol.locals()) {
				createLocalSlot(local);
			}

			addStatement(parseBlock(bodyNode));

			final SLFunctionBodyNode functionBodyNode = new SLFunctionBodyNode(functionScope.toBlockNode());
			srcFromNode(functionBodyNode, bodyNode);

			final var functionSrc = toSourceSection(bodyNode);

			final SLRootNode rootNode = new SLRootNode(language, frameDescriptorBuilder.build(), functionBodyNode, functionSrc, functionName);
			allFunctions.put(functionName, rootNode.getCallTarget());
		} finally {
			assert lexicalScope == functionScope;
			lexicalScope = null;
			frameDescriptorBuilder = null;
		}
	}

	private int createLocalSlot(VariableSymbol local) {
		final int frameSlot = frameDescriptorBuilder.addSlot(toFrameSlotKind(local.type()), local.name(), null);
		locals.put(local, new LocalSlot(frameSlot));
		return frameSlot;
	}

	private SLBlockNode parseBlock(SyntaxNode list) {
		if (list == null) {
			return null;
		}

		assert list.kind() == LIST : list.kind();
		final var newScope = new LexicalScope(lexicalScope, list);
		try {
			lexicalScope = newScope;
			for (int i = 0; i < list.slotCount(); i++) {
                parseStatement(list.slot(i));
			}
			return lexicalScope.toBlockNode();
		} finally {
			assert lexicalScope == newScope;
			lexicalScope = lexicalScope.outer;
		}
	}

	private void parseStatement(SyntaxNode node) {
		final AnySyntaxKind kind = node.kind();
		SLStatementNode statement;
		if (kind == VARIABLE_DEFINITION_STATEMENT) {
			assert node.slotCount() == 1;
			final SyntaxNodeWithSymbols def = (SyntaxNodeWithSymbols) node.slot(0);
			assert def.kind() == VARIABLE_DEFINITION : def.kind();
			assert def.slotCount() == 6;
			final SemanticSymbol symbol = def.symbol();
			assert symbol instanceof VariableSymbol : def;
			lexicalScope.locals.put(symbol.name(), (VariableSymbol) symbol);
			statement = SLWriteLocalVariableNodeGen.create(parseExpression(def.slot(5)), locals.get(symbol).frameSlot, toSourceSection(def.slot(1)), true);
			srcFromNode(statement, node);
		} else if (kind == ASSIGNMENT_STATEMENT) {
			assert node.slotCount() == 3;
			statement = parseAssignment((SyntaxNodeWithSymbols) node.slot(0), parseExpression(node.slot(2)));
			srcFromNode(statement, node);
		} else if (kind == EXPRESSION_STATEMENT) {
			assert node.slotCount() == 1;
			statement = parseExpression(node.slot(0));
		} else if (kind == IF_STATEMENT) {
			assert node.slotCount() == 9;
			statement = createIf(parseExpression(node.slot(1)), parseBlock(node.slot(3)), parseBlock(node.slot(7)));
			srcFromNode(statement, node);
		} else if (kind == WHILE_STATEMENT) {
			assert node.slotCount() == 5;
			statement = createWhile(parseExpression(node.slot(1)), parseBlock(node.slot(3)));
			srcFromNode(statement, node);
		} else if (kind == RETURN_STATEMENT) {
			assert node.slotCount() == 2;
			statement = new SLReturnNode(parseExpression(node.slot(1)));
			srcFromNode(statement, node);
		} else {
			assert false : kind;
			return;
		}
		if (statement != null) {
			addStatement(statement);
		}
	}

	private SLExpressionNode parseAssignment(SyntaxNodeWithSymbols left, SLExpressionNode right) {
		if (left.kind() == IDENTIFIER_NAME_EXPRESSION) {
			final var symbol = identifierNameToSymbol(left);
			assert symbol != null : left;
			LocalSlot localSlot = locals.get(symbol);
			assert localSlot != null : symbol.name();
			return SLWriteLocalVariableNodeGen.create(right, localSlot.frameSlot, toSourceSection(left), false);
		}
		assert false : left.kind();
		return null;
	}

	private SLExpressionNode parseExpression(SyntaxNode node) {
		final SLExpressionNode result = parseExpression0(node);
		if (result != null) {
			srcFromNode(result, node);
			result.addExpressionTag();
		}
		return result;
	}

	private SLExpressionNode parseExpression0(SyntaxNode node) {
		final AnySyntaxKind kind = node.kind();
		if (kind == INTEGER_LITERAL_EXPRESSION) {
			assert node.slotCount() == 1;
			final var terminal = node.slot(0);
			assert terminal.kind() == INTEGER;
            return new SLLongLiteralNode(((IntegerLiteralToken) terminal.token()).value);
		}

		if (kind == IDENTIFIER_NAME_EXPRESSION) {
			final VariableSymbol symbol = identifierNameToSymbol(node);
			final LocalSlot localSlot = locals.get(symbol);
			assert localSlot != null : symbol.name();
			return SLReadLocalVariableNodeGen.create(localSlot.frameSlot);
		}

		if (node.slotCount() == 3 && !node.slot(0).isTerminal() && node.slot(1).isTerminal() && !node.slot(2).isTerminal()) {
			final var leftUnboxed = parseExpression(node.slot(0));
			final var rightUnboxed = parseExpression(node.slot(2));
			if (kind == ADD_EXPRESSION) {
				return SLAddNodeGen.create(leftUnboxed, rightUnboxed);
			} else if (kind == MULTIPLY_EXPRESSION) {
				return SLMulNodeGen.create(leftUnboxed, rightUnboxed);
			} else if (kind == DIVIDE_EXPRESSION) {
				return SLDivNodeGen.create(leftUnboxed, rightUnboxed);
			} else if (kind == SUBTRACT_EXPRESSION) {
				return SLSubNodeGen.create(leftUnboxed, rightUnboxed);
			} else if (kind == LESS_THAN_EXPRESSION) {
				return SLLessThanNodeGen.create(leftUnboxed, rightUnboxed);
			} else if (kind == LESS_THAN_OR_EQUAL_EXPRESSION) {
				return SLLessOrEqualNodeGen.create(leftUnboxed, rightUnboxed);
			} else if (kind == GREATER_THAN_EXPRESSION) {
				return SLLogicalNotNodeGen.create(SLLessOrEqualNodeGen.create(leftUnboxed, rightUnboxed));
			} else if (kind == GREATER_THAN_OR_EQUAL_EXPRESSION) {
				return SLLogicalNotNodeGen.create(SLLessThanNodeGen.create(leftUnboxed, rightUnboxed));
			} else if (kind == EQUALS_EXPRESSION) {
				return SLEqualNodeGen.create(leftUnboxed, rightUnboxed);
			} else if (kind == NOT_EQUALS_EXPRESSION) {
				return SLLogicalNotNodeGen.create(SLEqualNodeGen.create(leftUnboxed, rightUnboxed));
			} else if (kind == LOGICAL_AND_EXPRESSION) {
				return new SLLogicalAndNode(leftUnboxed, rightUnboxed);
			} else if (kind == LOGICAL_OR_EXPRESSION) {
				return new SLLogicalOrNode(leftUnboxed, rightUnboxed);
			}
		}

		if (kind == INVOCATION_EXPRESSION) {
			assert node.slotCount() == 4;

			final var methodGroup = node.slot(0);
			assert methodGroup.kind() == IDENTIFIER_NAME_EXPRESSION;
			final var terminal = methodGroup.slot(0);
			assert terminal.kind() == IDENTIFIER;
			final var name = ((IdentifierToken) terminal.token()).value;

			final var paramList = node.slot(2);
			assert paramList.kind() == SEPARATED_LIST;

			final List<SLExpressionNode> args = new ArrayList<>();
			for (int i = 0; i < paramList.slotCount(); i += 2) {
				args.add(parseExpression(paramList.slot(i)));
			}

			return createCall(new SLFunctionLiteralNode(SLStrings.fromJavaString(name)), args);
		}

		assert false : kind;
		return null;
	}

	private VariableSymbol identifierNameToSymbol(SyntaxNode node) {
		assert node.slotCount() == 1;
		final var terminal = node.slot(0);
		assert terminal.kind() == IDENTIFIER;
		final var name = ((IdentifierToken) terminal.token()).value;
		final VariableSymbol symbol = lexicalScope.find(name);
		assert symbol != null : name;
		return symbol;
	}
}