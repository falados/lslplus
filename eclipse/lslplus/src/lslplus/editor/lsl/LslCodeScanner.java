package lslplus.editor.lsl;

import java.util.ArrayList;
import java.util.List;

import lslplus.util.LslColorProvider;
import lslplus.util.LslWhitespaceDetector;
import lslplus.util.LslWordDetector;

import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.EndOfLineRule;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.RuleBasedScanner;
import org.eclipse.jface.text.rules.SingleLineRule;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.rules.WhitespaceRule;
import org.eclipse.jface.text.rules.WordRule;
import org.eclipse.swt.SWT;

/**
 *  An LSL Plus rule-based code scanner.
 */
public class LslCodeScanner extends RuleBasedScanner {

    private static String[] lslPlusKeywords = {
            "$module", "jump", "default", "do", "else", "for", "if", "$import", "state", "return", "while" }; //$NON-NLS-11$ //$NON-NLS-10$ //$NON-NLS-9$ //$NON-NLS-8$ //$NON-NLS-7$ //$NON-NLS-6$ //$NON-NLS-5$ //$NON-NLS-4$ //$NON-NLS-3$ //$NON-NLS-2$ //$NON-NLS-1$

    private static String[] lslPlusTypes = {
            "integer", "string", "float", "list", "vector", "rotation", "key", "quaternion" }; //$NON-NLS-1$ //$NON-NLS-5$ //$NON-NLS-7$ //$NON-NLS-8$ //$NON-NLS-6$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-2$

    /**
     * Creates an LslPlus code scanner.
     * 
     * @param provider the color provider
     * @param handlerNames
     * @param predefFuncNames
     * @param predefConstNames
     */
    public LslCodeScanner(LslColorProvider provider, String[] handlerNames,
            String[] predefFuncNames, String[] predefConstNames) {

        IToken keyword = new Token(new TextAttribute(provider.getColor(LslColorProvider.KEYWORD),
                null, SWT.BOLD));
        IToken type = new Token(new TextAttribute(provider.getColor(LslColorProvider.TYPE), null,
                SWT.BOLD));
        IToken string = new Token(new TextAttribute(provider.getColor(LslColorProvider.STRING)));
        IToken comment = new Token(new TextAttribute(provider
                .getColor(LslColorProvider.SINGLE_LINE_COMMENT)));
        IToken other = new Token(new TextAttribute(provider.getColor(LslColorProvider.DEFAULT)));
        IToken handler = new Token(new TextAttribute(provider.getColor(LslColorProvider.HANDLER),
                null, SWT.BOLD));
        IToken predefFunc = new Token(new TextAttribute(provider
                .getColor(LslColorProvider.PREDEF_FUNC)));
        IToken predefConst = new Token(new TextAttribute(provider
                .getColor(LslColorProvider.PREDEF_CONST)));
        List rules = new ArrayList();

        // Add rule for single line comments.
        rules.add(new EndOfLineRule("//", comment)); //$NON-NLS-1$

        // Add rule for strings and character constants.
        rules.add(new SingleLineRule("\"", "\"", string, '\\')); //$NON-NLS-2$ //$NON-NLS-1$
        rules.add(new SingleLineRule("'", "'", string, '\\')); //$NON-NLS-2$ //$NON-NLS-1$

        // Add generic whitespace rule.
        rules.add(new WhitespaceRule(new LslWhitespaceDetector()));

        // Add word rule for keywords, types, handlers, constants and functions.
        WordRule wordRule = new WordRule(new LslWordDetector(), other);
        addWordsToRule(wordRule, lslPlusKeywords, keyword);
        addWordsToRule(wordRule, lslPlusTypes, type);
        addWordsToRule(wordRule, handlerNames, handler);
        addWordsToRule(wordRule, predefConstNames, predefConst);
        addWordsToRule(wordRule, predefFuncNames, predefFunc);
        rules.add(wordRule);

        IRule[] result = new IRule[rules.size()];
        rules.toArray(result);
        setRules(result);
    }

    private static void addWordsToRule(WordRule rule, String[] words, IToken t) {
        for (int i = 0; i < words.length; i++) {
            rule.addWord(words[i], t);
        }
    }
}
