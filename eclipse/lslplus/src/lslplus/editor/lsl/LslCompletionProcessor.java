package lslplus.editor.lsl;


import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import lslplus.LslPlusPlugin;
import lslplus.language_metadata.LslConstant;
import lslplus.language_metadata.LslFunction;
import lslplus.language_metadata.LslHandler;
import lslplus.language_metadata.LslParam;
import lslplus.util.LslWordDetector;
import lslplus.util.Util;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.TextPresentation;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ContextInformation;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationPresenter;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;
import org.eclipse.jface.text.rules.IWordDetector;
import org.eclipse.swt.graphics.Image;

/**
 * LSL Plus Completion Processor.
 */
public class LslCompletionProcessor implements IContentAssistProcessor {
	private static IWordDetector wordDetector = new LslWordDetector();
	private Image functionImage;
	private Image handlerImage;
	private Image constantImage;
	private Image keywordImage;
	private static final Comparator PROPOSAL_COMPARATOR = new Comparator() {
		public int compare(Object aProposal1, Object aProposal2) {
			String text1 = ((ICompletionProposal)aProposal1).getDisplayString();
			String text2 = ((ICompletionProposal)aProposal2).getDisplayString();
			return text1.compareTo(text2);
		}

		public boolean equals(Object aProposal) {
			return false;
		}
	};

	protected static class Validator implements IContextInformationValidator,
	    IContextInformationPresenter {

		protected int fInstallOffset;

		/*
		 * @see IContextInformationValidator#isContextInformationValid(int)
		 */
		public boolean isContextInformationValid(int offset) {
			return Math.abs(fInstallOffset - offset) < 5;
		}

		/*
		 * @see IContextInformationValidator#install(IContextInformation, ITextViewer, int)
		 */
		public void install(IContextInformation info, ITextViewer viewer, int offset) {
			fInstallOffset= offset;
		}
		
		/*
		 * @see org.eclipse.jface.text.contentassist.IContextInformationPresenter#updatePresentation(int, TextPresentation)
		 */
		public boolean updatePresentation(int documentPosition, TextPresentation presentation) {
			return false;
		}
	}

	protected final static String[] fgProposals=
		{ "default", //$NON-NLS-1$
		  "do", //$NON-NLS-1$
		  "else", //$NON-NLS-1$
		  "float", //$NON-NLS-1$
		  "for", //$NON-NLS-1$
		  "if", //$NON-NLS-1$
		  "$import", //$NON-NLS-1$
		  "integer", //$NON-NLS-1$
		  "return", //$NON-NLS-1$
		  "while", //$NON-NLS-1$
		  "vector", //$NON-NLS-1$
		  "string", //$NON-NLS-1$
		  "list", //$NON-NLS-1$
		  "rotation", //$NON-NLS-1$
		  "key", //$NON-NLS-1$
		  "state", //$NON-NLS-1$
		  "jump"}; //$NON-NLS-1$

	protected static CompletionInfo[] possibleProposals = null;
	protected IContextInformationValidator validator = new Validator();

	public LslCompletionProcessor() {
		functionImage = Util.findDescriptor("$nl$/icons/function.gif").createImage(true); //$NON-NLS-1$
		handlerImage = Util.findDescriptor("$nl$/icons/handler.gif").createImage(); //$NON-NLS-1$
		constantImage = Util.findDescriptor("$nl$/icons/constant.gif").createImage(); //$NON-NLS-1$
        keywordImage = Util.findDescriptor("$nl$/icons/keyword.gif").createImage(); //$NON-NLS-1$
	}
	
	protected CompletionInfo [] getPossibleProposals() {
		if (possibleProposals == null) {
		    LslHandler handlers[] = LslPlusPlugin.getDefault().getLslMetaData().getHandlers();
		    CompletionInfo[] handlerNames = (CompletionInfo[])Util.arrayMap(new Util.ArrayMapFunc() {
			    		public Class elementType() { return CompletionInfo.class; }
				    	public Object map(Object o) {
				    		LslHandler handler = (LslHandler) o;
				    		String proto = handler.getName() + formatParams(handler.getParams());
				    		String startLine = proto + " {"; //$NON-NLS-1$
				    		return new CompletionInfo(handler.getName(),
				    				startLine, //$NON-NLS-1$
				    				proto,
				    				handler.getDescription(), handlerImage,
				    				startLine.length());
				    	}
				    }, handlers);
		    
		    CompletionInfo[] functions = (CompletionInfo[])Util.arrayMap(new Util.ArrayMapFunc() {
				public Class elementType() { return CompletionInfo.class; }
				public Object map(Object o) {
					LslFunction f = (LslFunction) o;
					return new CompletionInfo(f.getName(),
							f.getName() + formatArgs(f.getParams()),
							f.getName() + formatParams(f.getParams()),
							f.getDescription(), functionImage, f.getName().length() + 1);
				}
		    }, LslPlusPlugin.getDefault().getLslMetaData().getFunctions());

		    CompletionInfo[] constants = (CompletionInfo[]) Util.arrayMap(
		    		new Util.ArrayMapFunc() {
						public Class elementType() { return CompletionInfo.class; }
						public Object map(Object o) {
							LslConstant k = (LslConstant) o;
							return new CompletionInfo(k.getName(),k.getName(),
									k.getName() + " - " + k.getType(), //$NON-NLS-1$
									k.getDescription(),constantImage, k.getName().length());
						}
		    		}, LslPlusPlugin.getDefault().getLslMetaData().getConstants());
		    CompletionInfo[] keywords = (CompletionInfo[]) Util.arrayMap(
		    		new Util.ArrayMapFunc() {
						public Class elementType() { return CompletionInfo.class; }
						public Object map(Object o) {
							String k = (String) o;
							return new CompletionInfo(k,k,k,null,keywordImage,k.length());
						}
		    		}, fgProposals);
		    possibleProposals = (CompletionInfo[])
		        Util.concat(new Object[][] { handlerNames, functions, constants, keywords });
		}
		return possibleProposals;
	}
	
	private static String formatParams(LslParam[] params) {
		StringBuilder buf = new StringBuilder("("); //$NON-NLS-1$
		String sep = ""; //$NON-NLS-1$
		for (int i = 0; i < params.length; i++) {
			buf.append(sep);
			sep = ", "; //$NON-NLS-1$
			buf.append(params[i].getType()).append(" ").append(params[i].getName()); //$NON-NLS-1$
		}
		return buf.append(")").toString(); //$NON-NLS-1$
	}
	
	private static String formatArgs(LslParam[] params) {
		StringBuilder buf = new StringBuilder("("); //$NON-NLS-1$
		String sep = ""; //$NON-NLS-1$
		for (int i = 0; i < params.length; i++) {
			buf.append(sep).append(params[i].getName());
			sep = ", "; //$NON-NLS-1$
		}
		return buf.append(")").toString(); //$NON-NLS-1$
	}
	private static class CompletionInfo {
		public CompletionInfo(String matchName,String insertText,String displayText, 
		        String additionalInfo, Image image, int cursorOffset) {
			this.matchName = matchName;
			this.displayText = displayText;
			this.insertText = insertText;
			this.additionalInfo = additionalInfo;
			this.image = image;
			this.cursorOffset = cursorOffset;
		}
		public String matchName;
		public String insertText;
		public String displayText;
		public String additionalInfo;
		public Image image;
		public int cursorOffset;
	}
	/* (non-Javadoc)
	 * Method declared on IContentAssistProcessor
	 */
	public ICompletionProposal[] computeCompletionProposals(ITextViewer viewer, int documentOffset) {
		List proposals = new ArrayList();
		Couple c = guessTextLine(viewer.getDocument(), documentOffset);
		String prefix = c.o;
		possibleProposals = getPossibleProposals();
		//String[] rules = fgProposals.getRules(prefix);
		for (int i = 0; i < possibleProposals.length; i++) {
			CompletionInfo prop = possibleProposals[i];
			if (prop.matchName.startsWith(prefix)) {
				proposals.add(new CompletionProposal(prop.insertText,
						documentOffset - prefix.length(), prefix.length(),
						/*prop.insertText.length()*/prop.cursorOffset, prop.image, prop.displayText, 
						new ContextInformation(prop.matchName,prop.displayText),
						prop.additionalInfo)); 
			}
		}
		Collections.sort(proposals, PROPOSAL_COMPARATOR);
		return (ICompletionProposal[])proposals.toArray(
									new ICompletionProposal[proposals.size()]);
	}
	
	private Couple guessTextLine(IDocument doc, int offset) {
		try {
		 	// Guess start position
			int start = offset;
			while (start >= 1 && isWordPart(doc.getChar(start - 1))) {
				start--;
			}

			// Guess end position
			int end = offset;
			return new Couple( doc.get(start, end - start),doc.getLineOfOffset(start) + 1);
		} catch (BadLocationException e) {
			return new Couple("",-1); //$NON-NLS-1$
		}
		
	}
	class Couple {
		public Couple(String o, int p) {
			this.o = o;
			this.p = p;
		}
		
		String o;
		int p;
	}
	
	private static final boolean isWordPart(char aChar) {
		return wordDetector.isWordPart(aChar);
	}

	/**
	 * TODO: does nothing now... come up with good Context info...
	 * @param viewer the viewer
	 * @param documentOffset the place in the document we need info for.
	 * @return  the array of context info.
	 */
	public IContextInformation[] computeContextInformation(ITextViewer viewer, int documentOffset) {
		IContextInformation[] result= new IContextInformation[0];
		for (int i= 0; i < result.length; i++) {
			result[i]= new ContextInformation("placeholder!", "placeholder!");  //$NON-NLS-1$//$NON-NLS-2$
		}
		return result;
	}
	
	public char[] getCompletionProposalAutoActivationCharacters() {
		return new char[] { '.', '(' };
	}
	
	public char[] getContextInformationAutoActivationCharacters() {
		return new char[] { '#' };
	}
	
	public IContextInformationValidator getContextInformationValidator() {
		return validator;
	}
	
	public String getErrorMessage() {
		return null;
	}
}
