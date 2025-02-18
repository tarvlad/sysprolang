/*
 * Copyright (c) 2012, 2020, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * The Universal Permissive License (UPL), Version 1.0
 *
 * Subject to the condition set forth below, permission is hereby granted to any
 * person obtaining a copy of this software, associated documentation and/or
 * data (collectively the "Software"), free of charge and under any and all
 * copyright rights in the Software, and any and all patent rights owned or
 * freely licensable by each licensor hereunder covering either (i) the
 * unmodified Software as contributed to or provided by such licensor, or (ii)
 * the Larger Works (as defined below), to deal in both
 *
 * (a) the Software, and
 *
 * (b) any piece of software and/or hardware listed in the lrgrwrks.txt file if
 * one is included with the Software each a "Larger Work" to which the Software
 * is contributed by such licensors),
 *
 * without restriction, including without limitation the rights to copy, create
 * derivative works of, display, perform, and distribute the Software and make,
 * use, sell, offer for sale, import, export, have made, and have sold the
 * Software and the Larger Work(s), and to sublicense the foregoing rights on
 * either these or other terms.
 *
 * This license is subject to the following condition:
 *
 * The above copyright notice and either this complete permission notice or at a
 * minimum a reference to the UPL must be included in all copies or substantial
 * portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package com.oracle.truffle.sl.tck;

import static org.graalvm.polyglot.tck.TypeDescriptor.NUMBER;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.tck.LanguageProvider;
import org.graalvm.polyglot.tck.ResultVerifier;
import org.graalvm.polyglot.tck.Snippet;
import org.graalvm.polyglot.tck.TypeDescriptor;
import org.junit.Assert;

public class SLTCKLanguageProvider implements LanguageProvider {
    private static final String ID = "sl";
    private static final String PATTERN_VALUE_FNC = "function %s() {return %s;}";
    private static final String PATTERN_BIN_OP_FNC = "function %s(a,b) {return a %s b;}";
    private static final String PATTERN_POST_OP_FNC = "function %s(a) {a %s;}";
    private static final String PATTERN_BUILTIN0 = "function %sBuiltin0() {return %s();}";
    private static final String PATTERN_BUILTIN1 = "function %sBuiltin1(a) {return %s(a);}";
    private static final String PATTERN_BUILTIN2 = "function %sBuiltin2(a, b) {return %s(a, b);}";
    private static final String[] PATTERN_STATEMENTS = {
                    "function %s() {r = 0;\n%s\nreturn r;\n}",
                    "function %s(p1) {r = 0;\n%s\nreturn r;\n}",
    };

    private static final TypeDescriptor NUMBER_RETURN = TypeDescriptor.union(NUMBER, TypeDescriptor.intersection());

    @Override
    public String getId() {
        return ID;
    }

    @Override
    public Value createIdentityFunction(Context context) {
        return eval(context, "function id (a) {return a;}", "id");
    }

    @Override
    public Collection<? extends Snippet> createValueConstructors(Context context) {
        final Collection<Snippet> res = new ArrayList<>();

        return Collections.unmodifiableCollection(res);
    }

    @Override
    public Collection<? extends Snippet> createExpressions(Context context) {
        final Collection<Snippet> res = new ArrayList<>();
        return Collections.unmodifiableCollection(res);
    }

    @Override
    public Collection<? extends Snippet> createStatements(Context context) {
        final Collection<Snippet> res = new ArrayList<>();
        return res;
    }

    @Override
    public Collection<? extends Snippet> createScripts(Context context) {
        final Collection<Snippet> res = new ArrayList<>();
        return Collections.unmodifiableCollection(res);
    }

    @Override
    public Collection<? extends Source> createInvalidSyntaxScripts(Context context) {
        final Collection<Source> res = new ArrayList<>();
        return Collections.unmodifiableCollection(res);
    }

    private static Snippet createValueConstructor(
                    final Context context,
                    final String value,
                    final String id,
                    final String functionName,
                    final TypeDescriptor type) {
        final Snippet.Builder opb = Snippet.newBuilder(
                        id,
                        eval(context, String.format(PATTERN_VALUE_FNC, functionName, value), functionName),
                        type);
        return opb.build();
    }

    private static Snippet.Builder createBinaryOperator(
                    final Context context,
                    final String operator,
                    final String functionName,
                    final TypeDescriptor type,
                    final TypeDescriptor ltype,
                    final TypeDescriptor rtype) {
        final Value fnc = eval(context, String.format(PATTERN_BIN_OP_FNC, functionName, operator), functionName);
        return Snippet.newBuilder(operator, fnc, type).parameterTypes(ltype, rtype);
    }

    private static Snippet.Builder createPostfixOperator(
                    final Context context,
                    final String operator,
                    final String functionName,
                    final TypeDescriptor type,
                    final TypeDescriptor ltype) {
        final Value fnc = eval(context, String.format(PATTERN_POST_OP_FNC, functionName, operator), functionName);
        return Snippet.newBuilder(operator, fnc, type).parameterTypes(ltype);
    }

    private static Snippet createStatement(
                    final Context context,
                    final String id,
                    final String functionName,
                    final String expression,
                    final TypeDescriptor returnType,
                    TypeDescriptor... paramTypes) {
        final Object[] formalParams = new String[paramTypes.length + 1];
        formalParams[0] = "r";
        for (int i = 1; i < formalParams.length; i++) {
            formalParams[i] = "p" + i;
        }
        final String formattedExpression = MessageFormat.format(expression, formalParams);
        final Value fnc = eval(context,
                        String.format(PATTERN_STATEMENTS[paramTypes.length], functionName, formattedExpression),
                        functionName);
        return Snippet.newBuilder(id, fnc, returnType).parameterTypes(paramTypes).build();
    }

    private static Snippet createBuiltin(
                    final Context context,
                    final String builtinName,
                    final TypeDescriptor returnType,
                    TypeDescriptor... paramTypes) {

        String pattern;
        switch (paramTypes.length) {
            case 0:
                pattern = PATTERN_BUILTIN0;
                break;
            case 1:
                pattern = PATTERN_BUILTIN1;
                break;
            case 2:
                pattern = PATTERN_BUILTIN2;
                break;
            default:
                throw new AssertionError();
        }

        final String formattedExpression = String.format(pattern, builtinName, builtinName);
        final Value fnc = eval(context, formattedExpression, builtinName);
        return Snippet.newBuilder(builtinName, fnc, returnType).parameterTypes(paramTypes).build();
    }

    private static Snippet loadScript(
                    final Context context,
                    final String resourceName,
                    final TypeDescriptor type,
                    final ResultVerifier verifier) {
        try {
            final Source src = createSource(resourceName);
            return Snippet.newBuilder(src.getName(), context.eval(src), type).resultVerifier(verifier).build();
        } catch (IOException ioe) {
            throw new AssertionError("IOException while creating a test script.", ioe);
        }
    }

    private static Source createSource(final String resourceName) throws IOException {
        int slashIndex = resourceName.lastIndexOf('/');
        String scriptName = slashIndex >= 0 ? resourceName.substring(slashIndex + 1) : resourceName;
        try (Reader in = new InputStreamReader(SLTCKLanguageProvider.class.getResourceAsStream(resourceName), "UTF-8")) {
            return Source.newBuilder(ID, in, scriptName).build();
        }
    }

    private static Value eval(final Context context, final String fncDecl, final String functionName) {
        return context.eval(ID,
                        fncDecl +
                                        "\n" +
                                        "function main() {\n" +
                                        String.format("  return %s;\n", functionName) +
                                        "}");
    }
}
